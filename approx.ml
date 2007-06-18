(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix
open Util
open Default_config
open Log

let usage () =
  prerr_endline
    "Usage: approx [options]
Proxy server for Debian archive files

Options:
    -f|--foreground    remain in foreground instead of detaching
    -v|--version       display version information and exit";
  exit 1

let version () =
  eprintf "%s %s\n" Version.name Version.number;
  prerr_endline
    "Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>\n\
     Released under the GNU General Public License"

let foreground = ref false

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--foreground" -> foreground := true
    | "-v" | "--version" -> version ()
    | _ -> usage ()
  done;
  if not !foreground then use_syslog ()

let print_config () =
  let units u = function
    | 0 -> ""
    | 1 -> sprintf " 1 %s" u
    | n -> sprintf " %d %ss" n u
  in
  info_message "Version: %s %s" Version.name Version.number;
  info_message "Interface: %s" interface;
  info_message "Port: %d" port;
  info_message "Interval:%s%s"
    (units "hour" (interval / 60)) (units "minute" (interval mod 60));
  info_message "Max wait: %d" max_wait;
  info_message "Max rate: %s" max_rate;
  info_message "User: %s" user;
  info_message "Group: %s" group;
  info_message "Syslog: %s" syslog;
  info_message "Verbose: %B" verbose;
  info_message "Debug: %B" debug

let http_time t =
  Netdate.format ~fmt: "%a, %d %b %Y %T GMT" (Netdate.create ~zone: 0 t)

let string_of_time = http_time

let in_progress name = name ^ ".tmp"  (* temporary name in case the download
					 is interrupted *)

let wait_for_download_in_progress name =
  let name' = in_progress name in
  let rec wait n =
    if Sys.file_exists name' then
      if n < max_wait then
	begin
	  if n = 0 && verbose then
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

let print_headers msg headers =
  debug_message "%s" msg;
  List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers

let proxy_headers size modtime =
  [ "Content-Type", "text/plain";
    "Content-Length", Int64.to_string size ] @
  if modtime <> 0. then [ "Last-Modified", http_time modtime ] else []

(* Deliver a file from the local cache *)

let deliver_local name env =
  if verbose && not debug then info_message "%s" name;
  let size = file_size name in
  env#set_output_header_fields (proxy_headers size (file_modtime name));
  if debug then print_headers "Cache hit" env#output_header_fields;
  `File (`Ok, None, cache_dir ^/ name, 0L, size)

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

let immutable_suffixes = [ ".deb"; ".dsc"; "orig.tar.gz"; ".diff.gz" ]

let is_mutable name =
  not (List.exists (Filename.check_suffix name) immutable_suffixes)

let too_old stats =  minutes_old stats > interval

(* Attempt to serve the requested file from the local cache *)

exception Cache_miss of float

let not_found () = raise (Cache_miss 0.)

let stale mod_time = raise (Cache_miss mod_time)

let serve_local name ims env =
  wait_for_download_in_progress name;
  let stats =
    try stat name
    with Unix_error (ENOENT, "stat", _) -> not_found ()
  in
  if debug then print_age name stats ims;
  if stats.st_kind <> S_REG then
    not_found ()
  else if is_mutable name && too_old stats then
    stale stats.st_mtime
  else if stats.st_mtime > ims then
    deliver_local name env
  else
    `Std_response (`Not_modified, None, None)

let make_directory path =
  let rec loop cwd = function
    | dir :: rest ->
	let name = cwd ^/ dir in
	if not (Sys.file_exists name) then
	  mkdir name 0o755
	else if not (is_directory name) then
	  failwith ("file " ^ name ^ " is not a directory");
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
    cache_chan := Some (create_file !tmp_cache_file)
  with e ->
    error_message "Cannot cache %s" name;
    raise e

let write_cache str pos len =
  match !cache_chan with
  | None -> assert false
  | Some chan -> output chan str pos len

exception Wrong_size

let close_cache size mod_time =
  match !cache_chan with
  | None -> assert false
  | Some chan ->
      let real_name = !cache_file in
      let tmp_name = !tmp_cache_file in
      if debug then debug_message "  close cache %s" real_name;
      close_out chan;
      cache_chan := None;
      if size = file_size tmp_name then
	begin
	  Sys.rename tmp_name real_name;
	  if mod_time <> 0. then
	    begin
	      if debug then
		debug_message "  setting mtime to %s"
		  (string_of_time mod_time);
	      utimes real_name mod_time mod_time
	    end;
	end
      else
	begin
	  error_message "Size of %s should be %Ld, not %Ld"
	    real_name size (file_size tmp_name);
	  Sys.remove tmp_name;
	  raise Wrong_size
	end

let remove_cache () =
  match !cache_chan with
  | None -> ()
  | Some chan ->
      let tmp_name = !tmp_cache_file in
      close_out chan;
      cache_chan := None;
      error_message "Removing %s (size: %Ld)" tmp_name (file_size tmp_name);
      Sys.remove tmp_name

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

let send_header size modtime (cgi : Netcgi1_compat.Netcgi_types.cgi_activation) =
  let headers = proxy_headers size modtime in
  let fields = List.map (fun (name, value) -> (name, [value])) headers in
  cgi#set_header ~status: `Ok ~fields ();
  if debug then
    print_headers "Proxy response" cgi#environment#output_header_fields

type response_state =
  { name : string;
    mutable status : int;
    mutable length : int64;
    mutable last_modified : float;
    mutable body_seen : bool }

let initial_response_state =
  { name = "?";
    status = 0;
    length = -1L;
    last_modified = 0.;
    body_seen = false }

let new_response name = { initial_response_state with name = name }

let finish_delivery resp =
  close_cache resp.length resp.last_modified;
  if resp.length >= 0L then Delivered else Cached

(* Update the ctime but not the mtime of the file, if it exists *)

let update_ctime name =
  try
    let stats = stat name in
    utimes name stats.st_atime stats.st_mtime
  with Unix_error (ENOENT, "stat", _) -> ()

let with_pair rex str proc =
  match Pcre.extract ~rex ~full_match: false str with
  | [| a; b |] -> proc (a, b)
  | _ -> assert false

let status_re = Pcre.regexp "^HTTP/\\d+\\.\\d+ (\\d{3}) (.*?)\\s*$"
let header_re = Pcre.regexp "^(.*?):\\s*(.*?)\\s*$"

let process_header resp str =
  let do_status (code, _) =
    resp.status <- int_of_string code
  in
  let do_header (header, value) =
    match String.lowercase header with
    | "content-length" ->
	(try resp.length <- Int64.of_string value
	 with Failure _ ->
	   error_message "Cannot parse Content-Length %s" value)
    | "last-modified" ->
	(try resp.last_modified <- Netdate.parse_epoch value
	 with Invalid_argument _ ->
	   error_message "Cannot parse Last-Modified date %s" value)
    | _ -> ()
  in
  if debug then debug_message "  %s" str;
  try with_pair header_re str do_header
  with Not_found ->  (* e.g., status line or CRLF *)
    try with_pair status_re str do_status
    with Not_found -> error_message "Unrecognized response: %s" str

let process_body resp cgi str pos len =
  if resp.status = 200 then
    let size = resp.length in
    if not resp.body_seen then
      begin
	resp.body_seen <- true;
	open_cache resp.name;
	if size >= 0L then
	  (* we can start our response now *)
	  send_header size resp.last_modified cgi
      end;
    write_cache str pos len;
    if size >= 0L then
      (* stream the data back to the client as we receive it *)
      cgi#output#really_output str pos len

(* Download a file from an HTTP repository *)

let download_http url name ims cgi =
  let headers =
    if ims > 0. then [ "If-Modified-Since: " ^ http_time ims ] else []
  in
  let resp = new_response name in
  let header_callback = process_header resp in
  let body_callback = process_body resp cgi in
  Url.download url ~headers ~header_callback body_callback;
  match resp.status with
  | 200 -> finish_delivery resp
  | 304 -> Not_modified
  | 404 -> File_not_found
  | n -> error_message "Unexpected status code: %d" n; File_not_found

(* Download a file from an FTP repository *)

let download_ftp url name ims cgi =
  let resp = new_response name in
  let header_callback = process_header resp in
  Url.head url header_callback;
  let mod_time = resp.last_modified in
  if debug then
    debug_message "  ims %s  mtime %s"
      (string_of_time ims) (string_of_time mod_time);
  if 0. < mod_time && mod_time <= ims then
    Not_modified
  else
    let body_callback = process_body resp cgi in
    resp.status <- 200;  (* for process_body *)
    Url.download url body_callback;
    finish_delivery resp

let download_url url =
  match Url.method_of url with
  | Url.HTTP -> download_http url
  | Url.FTP | Url.FILE -> download_ftp url

(* Remove any files from the cache that have been invalidated
   as a result of downloading a given file *)

let cleanup_after name =
  let rm file =
    if verbose then info_message "Removing invalid file %s" file;
    Sys.remove file
  in
  if Sys.file_exists name then
    List.iter rm (Release.files_invalidated_by name)

let copy_to dst src =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input src buf 0 len with
    | 0 -> ()
    | n -> dst#really_output buf 0 n; loop ()
  in
  loop ()

(* Similar to deliver_local, but we have to copy it ourselves *)

let copy_from_cache name cgi =
  wait_for_download_in_progress name;
  send_header (file_size name) (file_modtime name) cgi;
  let output = cgi#output in
  with_channel open_in name (copy_to output);
  output#commit_work ()

let serve_remote url name ims mod_time cgi =
  let respond code =
    raise (Nethttpd_types.Standard_response (code, None, None))
  in
  if verbose then info_message "%s" url;
  let status =
    try download_url url name (max ims mod_time) cgi
    with e ->
      remove_cache ();
      exception_message e;
      respond `Not_found
  in
  if debug then debug_message "  => %s" (string_of_download_status status);
  match status with
  | Delivered ->
      cgi#output#commit_work ();
      cleanup_after name
  | Cached ->
      copy_from_cache name cgi;
      cleanup_after name
  | Not_modified ->
      update_ctime name;
      if mod_time > ims then
	(* the cached copy is newer than what the client has *)
	copy_from_cache name cgi
      else
	respond `Not_modified
  | File_not_found ->
      respond `Not_found

let remote_service url name ims mod_time =
  object
    method process_body _ =
      object
	method generate_response env =
	  let cgi =
	    (* buffered activation runs out of memory on large downloads *)
	    Nethttpd_services.std_activation `Std_activation_unbuffered env
	  in
	  serve_remote url name ims mod_time cgi
      end
  end

let validate_path url =
  let path = relative_url url in
  match explode_path path with
  | dir :: rest ->
      (try path, implode_path (Config.get dir :: rest)
       with Not_found ->
	 invalid_arg ("no remote repository found for " ^ dir))
  | [] ->
      invalid_arg ("invalid path " ^ path)

(* Check if a file should be denied (reported to the client as not found) *)

let should_deny name =
  Filename.basename name = "Index" &&
  Filename.check_suffix (Filename.dirname name) ".diff"

let get_dependencies url name =
  match Filename.basename name with
  | "Release" ->
      (* since older versions of apt do not know about Release.gpg,
	 we fetch it now to ensure the cache will be consistent
	 for other clients using secure apt *)
      let url, file = url ^ ".gpg", name ^ ".gpg" in
      (try Url.download_file ~url ~file with e -> exception_message e)
  | _ ->
      ()

(* Handle a cache miss, either because the file is not present (mod_time = 0)
   or it hasn't been verified recently enough *)

let cache_miss url name ims mod_time =
  if should_deny name then
    begin
      if debug then debug_message "Denying %s" name;
      `Std_response (`Not_found, None, None)
    end
  else
    begin
      get_dependencies url name;
      `Accept_body (remote_service url name ims mod_time)
    end

let ims_time env =
  try Netdate.parse_epoch (env#input_header#field "If-Modified-Since")
  with Not_found | Invalid_argument _ -> 0.

let serve_file env =
  (* handle URL-encoded '+', '~', etc. *)
  let path = Netencoding.Url.decode ~plus: false env#cgi_request_uri in
  let headers = env#input_header_fields in
  if debug then print_headers (sprintf "Request %s" path) headers;
  try
    let name, url = validate_path path in
    let ims = ims_time env in
    try serve_local name ims env
    with Cache_miss mod_time -> cache_miss url name ims mod_time
  with Invalid_argument msg ->
    `Std_response (`Forbidden, None, Some msg)

let proxy_service =
  object
    method name = "proxy_service"
    method def_term = `Proxy_service
    method print fmt = Format.fprintf fmt "%s" "proxy_service"
    method process_header env =
      match env#remote_socket_addr with
      | ADDR_INET (host, port) ->
	  if verbose then
	    info_message "Connection from %s:%d"
	      (string_of_inet_addr host) port;
	  if env#cgi_request_method = "GET" && env#cgi_query_string = "" then
	    serve_file env
	  else
	    `Std_response (`Forbidden, None, Some "Invalid request line")
      | ADDR_UNIX path ->
	  failwith ("connection from UNIX socket " ^ path)
  end

let server () =
  try
    Sys.chdir cache_dir;
    if verbose then print_config ();
    Server.main ~user ~group ~interface port proxy_service
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
