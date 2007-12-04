(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix
open Unix.LargeFile
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
     Released under the GNU General Public License";
  exit 0

let foreground = ref false

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--foreground" -> foreground := true
    | "-v" | "--version" -> version ()
    | _ -> usage ()
  done;
  if not !foreground then use_syslog ()

let stat_file name =
  try Some (stat name) with Unix_error (ENOENT, "stat", _) -> None

(* Temporary name in case the download is interrupted *)

let in_progress name = name ^ ".tmp"

let wait_for_download_in_progress name =
  let name' = in_progress name in
  let rec wait n prev =
    match stat_file name' with
    | Some { st_size = cur } ->
	if cur = prev && n = max_wait then
	  begin
	    error_message "Concurrent download of %s is taking too long" name;
	    (* remove the other process's .tmp file if it still exists,
	       so we can create our own *)
	    rm name'
	  end
	else
	  begin
	    if prev = 0L && debug then
	      debug_message "Waiting for concurrent download of %s" name;
	    sleep 1;
	    wait (if cur = prev then n + 1 else 0) cur
	  end
    | None -> ()
  in
  wait 0 0L

let print_headers msg headers =
  debug_message "%s" msg;
  List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers

let proxy_headers size modtime =
  [ "Content-Type", "text/plain";
    "Content-Length", Int64.to_string size ] @
  if modtime <> 0. then [ "Last-Modified", Url.string_of_time modtime ] else []

type local_status =
  | Done of Nethttpd_types.http_service_reaction
  | Stale of float
  | Missing

(* Deliver a file from the local cache *)

let deliver_local name env =
  let size = file_size name in
  env#set_output_header_fields (proxy_headers size (file_modtime name));
  if debug then print_headers "Local response" env#output_header_fields;
  Done (`File (`Ok, None, cache_dir ^/ name, 0L, size))

let not_found = Done (`Std_response (`Not_found, None, None))
let not_modified = Done (`Std_response (`Not_modified, None, None))

let cache_hit name ims mod_time env =
  if Release.immutable name || Release.valid_file name then
    if mod_time <= ims then
      (if debug then debug_message "  => not modified";
       not_modified)
    else
      (if debug then debug_message "  => delivering from cache";
       deliver_local name env)
  else Missing

let deny name =
  if debug then debug_message "Denying %s" name;
  not_found

(* See if the given file should be denied (reported to the client as
   not found) rather than fetched remotely. This is done in two cases:
     * the client is requesting a .bz2 version of an index
     * the client is requesting a DiffIndex and an up-to-date .gz version
       of the corresponding index exists in the cache
   By denying the request, the client will fall back to requesting
   the Packages.gz or Sources.gz file.  Using .gz instead of .bz2
   allows pdiffs to be applied more quickly and ensures correct operation
   with older apt clients that do not support .bz2 *)

let should_deny name =
  (Release.is_index name && extension name = ".bz2") ||
  (pdiffs && Release.is_diff_index name &&
     Release.valid_file (Pdiff.file_of_diff_index name ^ ".gz"))

(* Attempt to serve the requested file from the local cache *)

let serve_local name ims env =
  wait_for_download_in_progress name;
  match stat_file name with
  | Some { st_mtime = mod_time } ->
      if Release.is_release name then Stale mod_time
      else if should_deny name then deny name
      else cache_hit name ims mod_time env
  | None ->
      if should_deny name then deny name
      else Missing

let make_directory path =
  let rec loop cwd = function
    | dir :: rest ->
	let name = cwd ^/ dir in
	if not (Sys.file_exists name) then
	  mkdir name 0o755
	else if not (Sys.is_directory name) then
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
  open_out_excl path

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
		  (Url.string_of_time mod_time);
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
  | Delivered -> "delivered"
  | Cached -> "cached"
  | Not_modified -> "not modified"
  | File_not_found -> "not found"

let send_header size modtime cgi =
  let cgi : Netcgi1_compat.Netcgi_types.cgi_activation = cgi in
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

let with_pair rex str proc =
  match Pcre.extract ~rex ~full_match: false str with
  | [| a; b |] -> proc (a, b)
  | _ -> assert false

let status_re = Pcre.regexp "^HTTP/\\d+\\.\\d+\\s+(\\d{3})\\s+(.*?)\\s*$"
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
	(try resp.last_modified <- Url.time_of_string value
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
    if ims > 0. then [ "If-Modified-Since: " ^ Url.string_of_time ims ] else []
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
      (Url.string_of_time ims) (Url.string_of_time mod_time);
  if 0. < mod_time && mod_time <= ims then
    Not_modified
  else
    let body_callback = process_body resp cgi in
    resp.status <- 200;  (* for process_body *)
    Url.download url body_callback;
    finish_delivery resp

let download_url url name ims cgi =
  let dl =
    match Url.protocol url with
    | Url.HTTP -> download_http
    | Url.FTP | Url.FILE -> download_ftp
  in
  try dl url name ims cgi
  with e ->
    remove_cache ();
    if verbose && e <> Failure url then exception_message e;
    File_not_found

(* Perform any pdiff processing triggered by downloading a given file *)

let cleanup_after url file =
  if pdiffs && Release.is_pdiff file then
    try Pdiff.apply file
    with e -> if verbose then exception_message e

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
  with_in_channel open_in name (copy_to output);
  output#commit_work ()

let serve_remote url name ims mod_time cgi =
  let respond code =
    raise (Nethttpd_types.Standard_response (code, None, None))
  in
  let copy_if_newer () =
    (* deliver the the cached copy if it is newer than the client's *)
    if mod_time > ims then copy_from_cache name cgi
    else respond `Not_modified
  in
  let status = download_url url name (max ims mod_time) cgi in
  if verbose then info_message "%s: %s" url (string_of_download_status status);
  match status with
  | Delivered ->
      cgi#output#commit_work ();
      cleanup_after url name
  | Cached ->
      copy_from_cache name cgi;
      cleanup_after url name
  | Not_modified ->
      copy_if_newer ()
  | File_not_found ->
      if offline && Sys.file_exists name then copy_if_newer ()
      else respond `Not_found

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

(* Handle a cache miss, either because the file is not present (mod_time = 0)
   or it hasn't been verified recently enough *)

let cache_miss url name ims mod_time =
  if debug then debug_message "  => cache miss";
  `Accept_body (remote_service url name ims mod_time)

let ims_time env =
  try Netdate.parse_epoch (env#input_header#field "If-Modified-Since")
  with Not_found | Invalid_argument _ -> 0.

let forbidden msg = `Std_response (`Forbidden, None, Some msg)

let serve_file env =
  (* handle URL-encoded '+', '~', etc. *)
  let path = Netencoding.Url.decode ~plus: false env#cgi_request_uri in
  let headers = env#input_header_fields in
  if debug then print_headers (sprintf "Request %s" path) headers;
  try
    let url, name = Url.translate_request path in
    let ims = ims_time env in
    match serve_local name ims env with
    | Done reaction -> reaction
    | Stale mod_time -> cache_miss url name ims mod_time
    | Missing -> cache_miss url name ims 0.
  with Invalid_argument msg | Failure msg -> forbidden msg

let proxy_service =
  object
    method name = "proxy_service"
    method def_term = `Proxy_service
    method print fmt = Format.fprintf fmt "%s" "proxy_service"
    method process_header env =
      match env#remote_socket_addr with
      | ADDR_INET (host, port) ->
	  if debug then
	    debug_message "Connection from %s:%d"
	      (string_of_inet_addr host) port;
	  if env#cgi_request_method = "GET" && env#cgi_query_string = "" then
	    serve_file env
	  else
	    forbidden "invalid HTTP request"
      | ADDR_UNIX path ->
	  failwith ("connection from UNIX socket " ^ path)
  end

let server () =
  try
    Sys.chdir cache_dir;
    info_message "Version: %s" Version.number;
    if verbose then print_config (info_message "%s");
    Server.main ~user ~group ~interface port proxy_service
  with e ->
    exception_message e;
    exit 1

let daemonize proc x =
  ignore (setsid ());
  List.iter close [ stdin; stdout; stderr ];
  (* double fork to detach daemon *)
  if fork () = 0 && fork () = 0 then
    proc x

let () =
  if !foreground then server ()
  else daemonize server ()
