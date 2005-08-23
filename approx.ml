(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config
open Printf
open Nethttpd_types
open Unix
open Log (* Log.error_message shadows Unix.error_message *)

let usage () =
  prerr_endline
    "Usage: approx [options]
Proxy server for Debian archive files

Options:
    -f|--foreground    remain in foreground instead of detaching
    -h|--help          display this message and exit
    -v|--version       display version information and exit";
  exit 1

let version () =
  eprintf "%s %s\n" Version.name Version.number;
  prerr_endline
    "Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
Released under the GNU General Public License"

let foreground = ref false

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--foreground" -> foreground := true
    | "-v" | "--version" -> version ()
    | _ -> usage ()
  done

let () = if not !foreground then use_syslog ()

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
  | Unix_error (err, str, "") ->
      error_message "%s: %s" str (Unix.error_message err)
  | Unix_error (err, str, arg) ->
      error_message "%s: %s (%s)" str (Unix.error_message err) arg
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

let print_headers msg headers =
  debug_message "%s" msg;
  List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers

let proxy_headers name env =
  env#set_output_header_field "Content-Type" "text/plain";
  env#set_output_header_field "Last-Modified" (http_time (file_modtime name));
  if debug then print_headers "Local response" env#output_header_fields

(* Deliver a file from the local cache *)

let deliver_local name env =
  if not debug then info_message "%s" name;
  proxy_headers name env;
  `File (`Ok, None, cache_dir ^/ name, 0L, file_size name)

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
  else if is_mutable name && (always_refresh name || too_old stats) then
    stale stats.st_mtime
  else if stats.st_mtime > ims then
    deliver_local name env
  else
    `Std_response (`Not_modified, None, None)

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

let close_cache ?(mod_time=0.) ?(size=(-1L)) () =
  match !cache_chan with
  | None -> assert false
  | Some chan ->
      if debug then debug_message "  close cache %s" !cache_file;
      close_out chan;
      cache_chan := None;
      try
	if size = -1L || size = file_size !tmp_cache_file then
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
	    error_message "Size of %s should be %Ld" !cache_file size;
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
      error_message "Removing %s (size: %Ld)" name (file_size name);
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

let serve_http url name ims env output =
  let header_list = ref [] in
  let length = ref (-1L) in
  let last_modified = ref 0. in
  let chunked = ref false in
  let add_header (header, value as pair) =
    match String.lowercase header with
    | "content-encoding" | "content-type" ->
	header_list := pair :: !header_list
    | "content-length" ->
	header_list := pair :: !header_list;
	(try length := Int64.of_string value
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
    if not !chunked then
      (* start our response now *)
      begin
	env#set_output_header_fields !header_list;
	if debug then print_headers "Proxy response" env#output_header_fields
      end;
    open_cache name
  in
  let body_callback str =
    if !status = 200 then
      begin
	once body_seen start_transfer;
	write_cache str;
	if not !chunked then
	  (* stream the data back to the client as we receive it *)
	  output#output_string str
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

let serve_ftp url name ims env output =
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

let copy src dst =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input src buf 0 len with
    | 0 -> dst#flush ()
    | n -> dst#really_output buf 0 n; loop ()
  in
  loop ()

(* Similar to deliver_local, but we have to copy it ourselves *)

let copy_from_cache name env output =
  wait_for_download_in_progress name;
  env#set_output_header_field "Content-Length"
    (Int64.to_string (file_size name));
  proxy_headers name env;
  with_channel open_in name (fun input -> copy input output)

let serve_remote url name ims mod_time env output =
  info_message "%s" url;
  let status =
    try serve_url url name (max ims mod_time) env output
    with e ->
      remove_cache ();
      exception_message e;
      raise (Standard_response (`Not_found, None, Some "Download failed"))
  in
  if debug then
    debug_message "  status: %s" (string_of_download_status status);
  match status with
  | Delivered ->
      cleanup_after name
  | Cached ->
      copy_from_cache name env output;
      cleanup_after name
  | Not_modified ->
      update_ctime name;
      if mod_time > ims then
	(* the cached copy is newer than what the client has *)
	copy_from_cache name env output
      else
	raise (Standard_response (`Not_modified, None, None))
  | File_not_found ->
      raise (Standard_response (`Not_found, None, None))

let remote_service url name ims mod_time =
  object
    method process_body env =
      let cgi =
	Nethttpd_services.std_activation `Std_activation_buffered env
      in
      object
	method generate_response env =
	  serve_remote url name ims mod_time env cgi#output;
	  cgi#output#commit_work ()
      end
  end

let validate_path path =
  let name = relative_path path in
  match explode_path name with
  | dir :: rest ->
      (try name, implode_path (Config.get dir :: rest)
      with Not_found ->
	invalid_arg ("no remote repository found for " ^ dir))
  | [] ->
      invalid_arg "invalid path"

let ims_time env =
  try Netdate.parse_epoch (env#input_header#field "If-Modified-Since")
  with Not_found | Invalid_argument _ -> 0.

let serve_file env =
  let path = env#cgi_request_uri in
  let headers = env#input_header_fields in
  if debug then print_headers (sprintf "Request %s" path) headers;
  try
    let name, url = validate_path path in
    let ims = ims_time env in
    try serve_local name ims env
    with Cache_miss mod_time ->
      (* The file is either not present (mod_time = 0)
	 or it hasn't been verified recently enough.
	 In either case, we must contact the remote repository. *)
      `Accept_body (remote_service url name ims mod_time)
  with Invalid_argument msg ->
    `Std_response (`Forbidden, None, Some msg)

let proxy_service =
  let version = Version.name ^/ Version.number in
  object (self)
    method name = "proxy_service"
    method def_term = `Proxy_service
    method print fmt = Format.fprintf fmt "%s" "proxy_service"

    method process_header env =
      (match env#remote_socket_addr with
       | ADDR_INET (host, port) ->
	   info_message "Connection from %s:%d" (string_of_inet_addr host) port
       | ADDR_UNIX path ->
	   failwith ("connection from UNIX socket " ^ path));
      env#set_output_header_field "Server" version;
      if env#cgi_request_method = "GET" && env#cgi_query_string = "" then
	serve_file env
      else
	`Std_response (`Forbidden, None, Some "Invalid request line")
  end

let server () =
  try
    Sys.chdir cache_dir;
    print_config ();
    Server.main port proxy_service
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
