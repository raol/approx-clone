(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config
open Http_daemon
open Printf

let prog = Filename.basename Sys.argv.(0)

let log = Syslog.openlog ~facility: `LOG_DAEMON prog

let message fmt = kprintf (Syslog.syslog log `LOG_INFO) fmt

let error_message = function
  | Unix.Unix_error (err, str, arg) ->
      if err = Unix.EADDRINUSE && str = "bind" then
	begin
	  message "Port %d is already in use" port;
	  if port = 9999 then message "Perhaps apt-proxy is already running?"
	end
      else
	message "%s: %s%s"
	  str (Unix.error_message err)
	  (if arg = "" then "" else sprintf " (%s)" arg)
  | e ->
      message "%s" (Printexc.to_string e)

let print_config () =
  let units u = function
    | 0 -> ""
    | 1 -> sprintf " 1 %s" u
    | n -> sprintf " %d %ss" n u
  in
  message "Config file: %s" config_file;
  message "Port: %d" port;
  message "Cache: %s" cache_dir;
  message "Interval:%s%s"
    (units "hour" (interval / 60)) (units "minute" (interval mod 60));
  message "Debug: %B" debug

let http_time t = Netdate.format ~fmt: "%a, %d %b %Y %T GMT" (Netdate.create t)

let string_of_time = http_time

let copy ~src ~dst =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input src buf 0 len with
    | 0 -> ()
    | n -> output dst buf 0 n; loop ()
  in
  loop ()

let response_headers ?(msg = "Local") code headers chan =
  if debug then
    begin
      message "%s response: %d" msg code;
      List.iter (fun (x, y) -> message "  %s: %s" x y) headers
    end;
  send_basic_headers chan ~code: (`Code code);
  send_headers chan ~headers;
  send_CRLF chan

let content_headers file_name =
  if Filename.check_suffix file_name ".gz" then
    [ "Content-Type", "application/x-gzip";
      "Content-Encoding", "x-gzip" ]
  else
    [ "Content-Type", "text/plain" ]

let proxy_headers file_name length =
  response_headers 200
    (content_headers file_name @ [ "Content-Length", string_of_int length ])

let respond_not_modified =
  response_headers 304 []

let relay_headers code headers =
  response_headers (int_of_string code) headers ~msg: "HTTP proxy"

let ftp_headers file_name =
  response_headers 200 (content_headers file_name) ~msg: "FTP proxy"

let is_mutable name =
  not (Filename.check_suffix name ".deb")

let minutes_old stats =
  int_of_float ((Unix.time () -. stats.Unix.st_ctime) /. 60. +. 0.5)

let ims_time headers =
  let rec loop = function
    | [] -> raise Not_found
    | (name, value) :: rest ->
	if name = "if-modified-since" then value
	else loop rest
  in
  try Netdate.parse_epoch (loop headers)
  with Not_found | Invalid_argument _ -> 0.

let is_stale name stats =
  stats.Unix.st_kind <> Unix.S_REG ||
  (is_mutable name && minutes_old stats > interval)

let serve_local dir path ims ochan =
  let name = dir ^/ path in
  let stats = Unix.stat name in
  if debug then
    begin
      message "%s" name;
      let age = minutes_old stats in
      message "  %d minute%s old" age (if age = 1 then "" else "s");
      message "  ims %s  mtime %s"
	(string_of_time ims) (string_of_time stats.Unix.st_mtime)
    end;
  if is_stale name stats then
    raise Not_found
  else if stats.Unix.st_mtime <= ims then
    respond_not_modified ochan
  else
    let ichan = open_in name in
    if not debug then message "%s" name;
    proxy_headers name stats.Unix.st_size ochan;
    copy ~src: ichan ~dst: ochan;
    close_in ichan

let make_directory path =
  let rec loop cwd = function
    | dir :: rest ->
	let name = cwd ^/ dir in
	begin
	  try
	    if (Unix.stat name).Unix.st_kind <> Unix.S_DIR then
	      failwith (name ^ " exists but is not a directory")
	  with
	    Unix.Unix_error (Unix.ENOENT, "stat", _) -> Unix.mkdir name 0o755
	end;
	loop name rest
    | [] -> ()
  in
  match explode_path path with
  | "" :: dirs -> loop "/" dirs
  | dirs -> loop "." dirs

let create_file path =
  make_directory (Filename.dirname path);
  open_out path

let cache_chan = ref None
let cache_file = ref ""

let open_cache name =
  assert (!cache_chan = None && !cache_file = "");
  try
    if debug then message "  open cache %s" name;
    (* use a ".tmp" suffix in case the download is interrupted *)
    cache_chan := Some (create_file (name ^ ".tmp"));
    cache_file := name
  with e ->
    error_message e;
    message "Cannot cache %s" name

let write_cache str =
  match !cache_chan with
  | Some ch -> output_string ch str
  | None -> ()

let close_cache t =
  match !cache_chan with
  | Some ch ->
      if debug then message "  close cache %s" !cache_file;
      close_out ch;
      Sys.rename (!cache_file ^ ".tmp") !cache_file;
      if t <> 0. then
	begin
	  if debug then
	    message "  setting mtime to %s" (string_of_time t);
	  Unix.utimes !cache_file t t
	end;
      cache_chan := None;
      cache_file := ""
  | None -> ()

let remove_cache () =
  let name = !cache_file in
  close_cache 0.;
  if name <> "" then
    begin
      message "Removing %s" name;
      Sys.remove name
    end

(* Update the ctime but not the mtime of the file, if it exists *)

let update_ctime name =
  try
    let stats = Unix.stat name in
    let now = Unix.time () in
    if debug then
      message "  updating ctime to %s" (string_of_time now);
    Unix.utimes name stats.Unix.st_mtime now
  with Unix.Unix_error (Unix.ENOENT, "stat", _) -> ()

let map_dir dir =
  try Config.get dir
  with Not_found ->
    message "No mapping for %s" dir;
    raise Not_found

let print_header str =
  let n = String.length str in
  if n >= 2 && str.[n-2] = '\r' && str.[n-1] = '\n' then
    (if n > 2 then message "  %s" (substring str ~until: (n-2)))
  else
    message "No CRLF in header: %s" str

let status_re = Pcre.regexp "^HTTP/\\d+\\.\\d+ (\\d{3}) (.*?)\\s*\\r\\n"
let header_re = Pcre.regexp "^(.*?):\\s*(.*?)\\s*\\r\\n"

let with_pair rex str proc =
  match Pcre.extract ~rex ~full_match: false str with
  | [| a; b |] ->
      assert (not (String.contains b '\r' || String.contains b '\n'));
      proc (a, b)
  | _ ->
      assert false

let serve_http url local_name ims chan =
  let headers = ref [] in
  let last_modified = ref 0. in
  let add_header (header, value as pair) =
    match String.lowercase header with
    | "content-encoding" | "content-length" | "content-type"
    | "date" | "server" ->
	headers := pair :: !headers
    | "last-modified" ->
	headers := pair :: !headers;
	(try last_modified := Netdate.parse_epoch value;
	 with Invalid_argument _ ->
	  message "Cannot parse last-modified date %s" value)
    | _ -> ()
  in
  let status = ref "" in
  let do_status (code, _) =
    status := code;
    if code = "304" then update_ctime local_name  (* not modified *)
  in
  let header =
    if ims > 0. then Some ("If-Modified-Since: " ^ http_time ims) else None
  in
  let header_callback str =
    if debug then print_header str;
    try with_pair header_re str add_header
    with Not_found ->  (* e.g., status line or CRLF *)
      try with_pair status_re str do_status
      with Not_found ->
	if str <> "\r\n" then message "Unrecognized response: %s" str
  in
  let body_seen = ref false in
  let body_callback str =
    if not !body_seen then
      begin
	body_seen := true;
	relay_headers !status (List.rev !headers) chan;
	if !status = "200" then open_cache local_name
      end;
    output_string chan str;
    write_cache str
  in
  Url.iter url ?header ~header_callback body_callback;
  close_cache !last_modified;
  if not !body_seen then
    relay_headers !status (List.rev !headers) chan

let serve_ftp url local_name ims chan =
  let body_seen = ref false in
  let body_callback str =
    if not !body_seen then
      begin
	body_seen := true;
	ftp_headers local_name chan;
	open_cache local_name
      end;
    output_string chan str;
    write_cache str
  in
  Url.iter url body_callback;
  close_cache 0.;
  if not !body_seen then
    ftp_headers local_name chan

let method_of_url url =
  let meth =
    try String.lowercase (substring url ~until: (String.index url ':'))
    with Not_found ->
      message "No method in URL: %s" url;
      raise Not_found
  in
  match meth with
  | "http" -> serve_http
  | "ftp" -> serve_ftp
  | m ->
      message "Unsupported URL method: %s" m;
      raise Not_found

let serve_remote dir path ims chan =
  try
    let local_name = dir ^/ path in
    let url = map_dir dir ^/ path in
    message "%s" url;
    let handler = method_of_url url in
    handler url local_name ims chan
  with e ->
    remove_cache ();
    error_message e;
    respond_not_found ~url: path chan

let serve_file path headers chan =
  try
    if debug then
      begin
	message "Request %s" path;
	List.iter (fun (x, y) -> message "  %s: %s" x y) headers
      end;
    let dir, path = split_path path in
    let ims = ims_time headers in
    try serve_local dir path ims chan
    with _ -> serve_remote dir path ims chan
  with Failure _ ->
    respond_forbidden ~url: path chan

let callback req chan =
  message "Connection from %s" req#clientAddr;
  match req#params with
  | [] -> serve_file req#path req#headers chan
  | _ -> respond_forbidden ~url: req#path chan

let daemon () =
  ignore (Unix.setsid ());
  List.iter Unix.close [Unix.stdin; Unix.stdout; Unix.stderr];
  try
    Unix.chdir cache_dir;
    print_config ();
    main (daemon_spec ~port ~callback ~mode: `Single ~timeout: None ())
  with e ->
    error_message e

let () =
  (* double fork to detach daemon *)
  if Unix.fork () = 0 && Unix.fork () = 0 then
    daemon ()
