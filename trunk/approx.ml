(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config
open Http_daemon
open Printf
open Unix

let usage () =
  prerr_endline "Usage: approx [options]";
  prerr_endline "Proxy server for Debian archive files";
  prerr_endline "Options:";
  prerr_endline "    -f|--foreground    stay in foreground instead of detaching";
  exit 1

let foreground = ref false

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--foreground" -> foreground := true
    | _ -> usage ()
  done

let print_message =
  if !foreground then
    fun _ -> prerr_endline
  else
    let prog = Filename.basename Sys.argv.(0) in
    let log = Syslog.openlog ~facility: `LOG_DAEMON prog in
    Syslog.syslog log

let message level fmt = kprintf (print_message level) fmt

let error_message fmt = message `LOG_ERR fmt
let info_message fmt = message `LOG_INFO fmt
let debug_message fmt = message `LOG_DEBUG fmt

let exception_message exc =
  match exc with
  | Sys_error str ->
      error_message "%s" str
  | Failure str ->
      error_message "Failure: %s" str
  | Invalid_argument str ->
      error_message "Invalid argument: %s" str
  | Curl.CurlException (_, _, str) ->
      error_message "Curl exception: %s" str
  | Unix_error (err, str, arg) ->
      if err = EADDRINUSE && str = "bind" then
	error_message "Port %d is already in use" port
      else
	error_message "%s: %s%s" str (Unix.error_message err)
	  (if arg = "" then "" else sprintf " (%s)" arg)
  | e ->
      error_message "%s" (Printexc.to_string e)

let print_config () =
  let units u = function
    | 0 -> ""
    | 1 -> sprintf " 1 %s" u
    | n -> sprintf " %d %ss" n u
  in
  info_message "Config file: %s" config_file;
  info_message "Port: %d" port;
  info_message "Cache: %s" cache_dir;
  info_message "Interval:%s%s"
    (units "hour" (interval / 60)) (units "minute" (interval mod 60));
  info_message "Max wait: %d" max_wait;
  info_message "Debug: %B" debug

let http_time t =
  Netdate.format ~fmt: "%a, %d %b %Y %T GMT" (Netdate.create ~zone: 0 t)

let string_of_time = http_time

(* Note that if we include a Content-Length field in our response,
   APT assumes that we will keep the connection alive.
   If we close it instead, APT prints error messages of the form
	Error reading from server - read (104 Connection reset by peer)
   or passes partial files to gzip for decompression, resulting in
	Sub-process gzip returned an error code (1) *)

let has_header h =
  let h = String.lowercase h in
  let rec loop = function
    | (name, _) :: rest ->
	if String.lowercase name = h then true
	else loop rest
    | [] -> false
  in
  loop

let response_headers ?(msg = "Local") code headers chan =
  send_basic_headers chan ~code: (`Code code);
  if debug then
    begin
      debug_message "%s response: %d" msg code;
      List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers
    end;
  send_headers chan ~headers;
  if not (has_header "Connection" headers) then
    send_headers chan ~headers: [ "Connection", "keep-alive" ];
  if code = 200 && not (has_header "Content-Length" headers) then
    error_message "No HTTP Content-Length header";
  send_CRLF chan

let content_type file_name = "Content-Type", "text/plain"
let content_lastmod time = "Last-Modified", http_time time
let content_length length = "Content-Length", string_of_int length

let proxy_headers file_name stats =
  response_headers 200
    [ content_type file_name;
      content_lastmod stats.st_mtime;
      content_length stats.st_size ]

let relay_headers = response_headers ~msg: "HTTP proxy"

let respond_not_modified = response_headers 304 []

let in_progress name = name ^ ".tmp"  (* temporary name in case the download
					 is interrupted *)

let wait_for_download_in_progress name =
  let name' = in_progress name in
  let rec wait n =
    if Sys.file_exists name' then
      if n < max_wait then
	begin
	  if n = 0 then
	    info_message "Waiting for download of %s to complete" name;
	  sleep 1;
	  wait (n+1)
	end
      else
	begin
	  error_message "Concurrent download of %s is taking too long" name;
	  (* remove the other process's .tmp file so we can create our own *)
	  try Sys.remove name' with _ -> ()
	end
  in
  wait 0

let copy ~src ~dst =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input src buf 0 len with
    | 0 -> ()
    | n -> output dst buf 0 n; loop ()
  in
  loop ()

(* Deliver a file from the local cache *)

let deliver_local name ochan =
  wait_for_download_in_progress name;
  if not debug then info_message "%s" name;
  with_channel open_in name (fun ichan ->
    proxy_headers name (fstat (descr_of_in_channel ichan)) ochan;
    copy ~src: ichan ~dst: ochan)

(* Return the age of a file in minutes, using the last status change
   time (ctime) rather than the modification time (mtime).

   The mtime tells when the contents of the file last changed, and is
   used by the "If-Modified-Since" logic.  Whenever we learn that the
   file is still valid via a "Not Modified" response, we update the
   ctime so that the file will continue to be considered current. *)

let minutes_old stats =
  int_of_float ((time () -. stats.st_ctime) /. 60. +. 0.5)

let print_age name stats ims =
  debug_message "%s" name;
  let age = minutes_old stats in
  debug_message "  %d minute%s old" age (if age = 1 then "" else "s");
  debug_message "  ims %s  mtime %s"
    (string_of_time ims) (string_of_time stats.st_mtime)

let is_mutable name = not (Filename.check_suffix name ".deb")

let always_refresh name =
  match Filename.basename name with
  | "Release" | "Release.gpg" -> true
  | _ -> false

let too_old stats =  minutes_old stats > interval

(* Attempt to serve the requested file from the local cache *)

exception Stale of float

let serve_local name ims ochan =
  wait_for_download_in_progress name;
  let stats =
    try stat name
    with Unix_error (ENOENT, "stat", _) -> raise Not_found
  in
  if debug then print_age name stats ims;
  if stats.st_kind <> S_REG then
    raise Not_found
  else if is_mutable name && (always_refresh name || too_old stats) then
    raise (Stale stats.st_mtime)
  else if stats.st_mtime > ims then
    deliver_local name ochan
  else
    respond_not_modified ochan

let make_directory path =
  let rec loop cwd = function
    | dir :: rest ->
	let name = cwd ^/ dir in
	(try
	  if (stat name).st_kind <> S_DIR then
	    failwith (name ^ " exists but is not a directory")
	with
	  Unix_error (ENOENT, "stat", _) -> mkdir name 0o755);
	loop name rest
    | [] -> ()
  in
  match explode_path path with
  | "" :: dirs -> loop "/" dirs
  | dirs -> loop "." dirs

let create_file path =
  make_directory (Filename.dirname path);
  (* open file exclusively so we don't conflict with a concurrent download *)
  out_channel_of_descr (openfile path [ O_CREAT; O_WRONLY; O_EXCL ] 0o644)

let cache_chan = ref None
let cache_file = ref ""
let tmp_cache_file = ref ""

let open_cache name =
  assert (!cache_chan = None);
  try
    if debug then debug_message "  open cache %s" name;
    cache_file := name;
    tmp_cache_file := in_progress name;
    cache_chan := Some (create_file !tmp_cache_file);
  with e ->
    exception_message e;
    error_message "Cannot cache %s" name

let write_cache str =
  match !cache_chan with
  | None -> assert false
  | Some chan ->
      output_string chan str;
      flush chan

let close_cache ?(mod_time=0.) ?(size=(-1)) () =
  match !cache_chan with
  | None -> assert false
  | Some chan ->
      if debug then debug_message "  close cache %s" !cache_file;
      close_out chan;
      cache_chan := None;
      try
	if size = -1 || size = file_size !tmp_cache_file then
	  begin
	    Sys.rename !tmp_cache_file !cache_file;
	    if mod_time <> 0. then
	      begin
		if debug then
		  debug_message "  setting mtime to %s"
		    (string_of_time mod_time);
		utimes !cache_file mod_time mod_time
	      end
	  end
	else
	  begin
	    error_message "Size of %s should be %d" !cache_file size;
	    Sys.remove !tmp_cache_file
	  end
      with e ->
	(* gc_approx or another approx process might have removed .tmp file *)
	error_message "Cannot close cache file %s" !cache_file;
	exception_message e

let remove_cache () =
  match !cache_chan with
  | None -> ()
  | Some chan ->
      let name = !tmp_cache_file in
      close_out chan;
      cache_chan := None;
      error_message "Removing %s (size: %d)" name (file_size name);
      Sys.remove name

type download_status =
  | Delivered
  | Cached
  | Not_modified
  | File_not_found

let string_of_download_status = function
  | Delivered -> "Delivered"
  | Cached -> "Cached"
  | Not_modified -> "Not modified"
  | File_not_found -> "File not found"

(* Update the ctime but not the mtime of the file, if it exists *)

let update_ctime name =
  try
    let stats = stat name in
    utimes name stats.st_atime stats.st_mtime
  with Unix_error (ENOENT, "stat", _) -> ()

let print_header str =
  let n = String.length str in
  if n >= 2 && str.[n-2] = '\r' && str.[n-1] = '\n' then
    (if n > 2 then debug_message "  %s" (substring str ~until: (n-2)))
  else
    error_message "No CRLF in header: %s" str

let status_re = Pcre.regexp "^HTTP/\\d+\\.\\d+ (\\d{3}) (.*?)\\s*\\r\\n"
let header_re = Pcre.regexp "^(.*?):\\s*(.*?)\\s*\\r\\n"

let with_pair rex str proc =
  match Pcre.extract ~rex ~full_match: false str with
  | [| a; b |] ->
      assert (not (String.contains b '\r' || String.contains b '\n'));
      proc (a, b)
  | _ ->
      assert false

let once flag proc =
  if not !flag then
    begin
      flag := true;
      proc ()
    end

(* Respond to a request for a file at an HTTP repository *)

let serve_http url name ims chan =
  let header_list = ref [] in
  let length = ref (-1) in
  let last_modified = ref 0. in
  let chunked = ref false in
  let add_header (header, value as pair) =
    match String.lowercase header with
    | "content-encoding" | "content-type" ->
	header_list := pair :: !header_list
    | "content-length" ->
	header_list := pair :: !header_list;
	(try length := int_of_string value
	with Failure _ ->
	  error_message "Cannot parse Content-Length value %s" value)
    | "last-modified" ->
	header_list := pair :: !header_list;
	(try last_modified := Netdate.parse_epoch value;
	with Invalid_argument _ ->
	  error_message "Cannot parse Last-Modified date %s" value)
    | "transfer-encoding" ->
	if String.lowercase value = "chunked" then
	  chunked := true
	else
	  error_message "Unknown Transfer-Encoding type %s" value
    | _ ->
	()
  in
  let status = ref 0 in
  let do_status (code, _) =
    status := int_of_string code
  in
  let header_callback str =
    if debug then print_header str;
    try with_pair header_re str add_header
    with Not_found ->  (* e.g., status line or CRLF *)
      try with_pair status_re str do_status
      with Not_found ->
	if str <> "\r\n" then error_message "Unrecognized response: %s" str
  in
  let body_seen = ref false in
  let start_transfer () =
    open_cache name;
    if not !chunked then
      (* start our response now *)
      relay_headers !status (List.rev !header_list) chan
  in
  let body_callback str =
    if !status = 200 then
      begin
	once body_seen start_transfer;
	write_cache str;
	if not !chunked then
	  (* stream the data back to the client as we receive it *)
	  begin
	    output_string chan str;
	    flush chan
	  end
      end
  in
  let headers =
    if ims > 0. then [ "If-Modified-Since: " ^ http_time ims ] else []
  in
  Url.iter url ~headers ~header_callback body_callback;
  match !status with
  | 200 ->
      close_cache ~mod_time: !last_modified ~size: !length ();
      if !chunked then Cached else Delivered
  | 304 ->
      Not_modified
  | 404 ->
      File_not_found
  | n ->
      error_message "Unexpected status code: %d" n;
      File_not_found

(* Respond to a request for a file at an FTP repository *)

let serve_ftp url name ims chan =
  let mod_time = Url.mod_time url in
  if debug then
    debug_message "  ims %s  mtime %s"
      (string_of_time ims) (string_of_time mod_time);
  if mod_time > ims || mod_time = 0. then
    begin
      (* Since don't know the length in advance,
	 we cache the whole file before delivering it to the client.
	 The alternative -- using chunked transfer encoding --
	 triggers a bug in APT. *)
      open_cache name;
      Url.iter url write_cache;
      close_cache ~mod_time ();
      Cached
    end
  else
    Not_modified

let serve_url url =
  let meth =
    try String.lowercase (substring url ~until: (String.index url ':'))
    with Not_found -> invalid_arg "no method in URL"
  in
  match meth with
  | "http" -> serve_http url
  | "ftp" -> serve_ftp url
  | _ -> invalid_arg "unsupported URL method"

(* Remove any files from the cache that have been invalidated
   as a result of downloading a given file *)

let cleanup_after name =
  let remove file =
    info_message "Removing invalid file %s" file;
    Sys.remove file
  in
  if Sys.file_exists name then
    List.iter remove (Release.files_invalidated_by name)

let serve_remote url name ims ims' chan =
  try
    info_message "%s" url;
    let status = serve_url url name (max ims ims') chan in
    if debug then
      debug_message "  status: %s" (string_of_download_status status);
    match status with
    | Delivered ->
	cleanup_after name
    | Cached ->
	deliver_local name chan;
	cleanup_after name
    | Not_modified ->
	update_ctime name;
	if ims < ims' then
	  deliver_local name chan
	else
	  respond_not_modified chan
    | File_not_found ->
	respond_not_found ~url: name chan
  with e ->
    remove_cache ();
    exception_message e;
    respond_not_found ~url: name chan

let ims_time headers =
  (* the OCaml HTTP library converts header names to lowercase *)
  try Netdate.parse_epoch (List.assoc "if-modified-since" headers)
  with Not_found | Invalid_argument _ -> 0.

let print_request path headers =
  debug_message "Request %s" path;
  List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers

let validate_path path =
  let name = relative_path path in
  match explode_path name with
  | dir :: rest ->
      (try name, implode_path (Config.get dir :: rest)
      with Not_found ->
	invalid_arg ("no remote repository found for " ^ dir))
  | [] ->
      invalid_arg "invalid path"

let serve_file path headers chan =
  if debug then print_request path headers;
  try
    let name, url = validate_path path in
    let ims = ims_time headers in
    try
      serve_local name ims chan
    with
    | Stale mtime ->
	(* old version in the local cache *)
	serve_remote url name ims mtime chan
    | Not_found ->
	(* file not in local cache *)
	serve_remote url name ims ims chan
  with Invalid_argument msg ->
    error_message "%s: %s" path msg;
    respond_forbidden ~url: path chan

let callback req chan =
  info_message "Connection from %s" req#clientAddr;
  match req#params with
  | [] -> serve_file req#path req#headers chan
  | _ -> respond_forbidden ~url: req#path chan

let server () =
  let mode = `Fork in
  let root_dir = Some cache_dir in
  let timeout = None in
  try
    print_config ();
    main (daemon_spec ~callback ~mode ~port ~root_dir ~timeout ())
  with e ->
    exception_message e;
    exit 1

let daemonize proc =
  ignore (setsid ());
  List.iter close [ stdin; stdout; stderr ];
  (* double fork to detach daemon *)
  if fork () = 0 && fork () = 0 then
    proc ()

let () =
  if !foreground then server ()
  else daemonize server
