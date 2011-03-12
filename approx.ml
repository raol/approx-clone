(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix
open Unix.LargeFile
open Util
open Config
open Log
open Program

let stat_file name = try Some (stat name) with Unix_error _ -> None

(* Hint that a download is in progress *)

let in_progress name = name ^ ".hint"

let wait_for_download_in_progress name =
  let hint = in_progress name in
  let timeout = float_of_int max_wait in
  let rec wait n =
    match stat_file hint with
    | Some { st_mtime = mtime } ->
        if time () -. mtime > timeout then begin
          error_message "Concurrent download of %s is taking too long" name;
          (* remove the other process's hint file if it still exists,
             so we can create our own *)
          rm hint
        end else begin
          if n = 0 then
            debug_message "Waiting for concurrent download of %s" name;
          sleep 1;
          wait (n + 1)
        end
    | None -> ()
  in
  wait 0

let debug_headers msg headers =
  debug_message "%s" msg;
  List.iter (fun (x, y) -> debug_message "  %s: %s" x y) headers

let proxy_headers size modtime =
  let headers =
    ["Content-Type", "text/plain";
     "Content-Length", Int64.to_string size]
  in
  if modtime = 0. then headers
  else ("Last-Modified", Url.string_of_time modtime) :: headers

type local_status =
  | Done of Nethttpd_types.http_service_reaction
  | Cache_miss of float

let head_request env = env#cgi_request_method = "HEAD"

(* Deliver a file from the local cache *)

let deliver_local name env =
  debug_message "  => delivering from cache";
  let size = file_size name in
  env#set_output_header_fields (proxy_headers size (file_modtime name));
  debug_headers "Local response" env#output_header_fields;
  let file = if head_request env then "/dev/null" else cache_dir ^/ name in
  Done (`File (`Ok, None, file, 0L, size))

let not_modified () =
  debug_message "  => not modified";
  Done (`Std_response (`Not_modified, None, None))

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

let deny name =
  debug_message "Denying %s" name;
  Done (`Std_response (`Not_found, None, None))

(* Attempt to serve the requested file from the local cache *)

let serve_local name ims env =
  let deliver_if_newer mod_time =
    if mod_time > ims then deliver_local name env
    else not_modified ()
  in
  wait_for_download_in_progress name;
  match stat_file name with
  | Some { st_mtime = mod_time } ->
      if Release.is_release name then Cache_miss mod_time
      else if should_deny name then deny name
      else if Release.immutable name || Release.valid_file name then
        deliver_if_newer mod_time
      else Cache_miss mod_time
  | None ->
      if should_deny name then deny name
      else Cache_miss 0.

let create_hint name =
  make_directory (Filename.dirname name);
  close (openfile (in_progress name) [O_CREAT; O_WRONLY] 0o644)

let remove_hint name = rm (in_progress name)

type cache_info = { file : string; tmp_file : string; chan : out_channel }

type cache_state =
  | Cache of cache_info
  | Pass_through
  | Undefined

(* Don't cache the result of a request for a directory *)

let should_pass_through name =
  if Sys.file_exists name then Sys.is_directory name
  else let n = String.length name in n > 0 && name.[n - 1] = '/'

let open_cache file =
  if should_pass_through file then begin
    debug_message "  pass-through %s" file;
    Pass_through
  end else
    try
      debug_message "  open cache %s" file;
      make_directory (Filename.dirname file);
      let tmp_file = gensym file in
      let chan = open_out_excl tmp_file in
      Cache { file = file; tmp_file = tmp_file; chan = chan }
    with e ->
      error_message "Cannot cache %s" file;
      raise e

let write_cache cache str pos len =
  match cache with
  | Cache { chan = chan } -> output chan str pos len
  | Pass_through -> ()
  | Undefined -> assert false

exception Wrong_size

let close_cache cache size mod_time =
  match cache with
  | Cache { file = file; tmp_file = tmp_file; chan = chan } ->
      debug_message "  close cache %s" file;
      close_out chan;
      if size = file_size tmp_file then begin
        Sys.rename tmp_file file;
        if mod_time <> 0. then begin
          debug_message "  setting mtime to %s" (Url.string_of_time mod_time);
          utimes file mod_time mod_time
        end
      end else begin
        error_message "Size of %s should be %Ld, not %Ld"
          file size (file_size tmp_file);
        rm tmp_file;
        raise Wrong_size
      end
  | Pass_through -> ()
  | Undefined -> assert false

let remove_cache cache =
  match cache with
  | Cache { tmp_file = tmp_file; chan = chan } ->
      close_out chan;
      error_message "Removing %s (size: %Ld)" tmp_file (file_size tmp_file);
      rm tmp_file
  | Pass_through | Undefined -> ()

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

type response_state =
  { name : string;
    mutable status : int;
    mutable length : int64;
    mutable last_modified : float;
    mutable location : string;
    mutable content_type : string;
    mutable body_seen : bool;
    mutable cache : cache_state }

let new_response url name =
  { name = name;
    status = 0;
    length = -1L;
    last_modified = 0.;
    location = url;
    content_type = "text/plain";
    body_seen = false;
    cache = Undefined }

type cgi = Netcgi1_compat.Netcgi_types.cgi_activation

let send_header size modtime (cgi : cgi) =
  let headers = proxy_headers size modtime in
  let fields = List.map (fun (name, value) -> (name, [value])) headers in
  cgi#set_header ~status: `Ok ~fields ();
  debug_headers "Proxy response" cgi#environment#output_header_fields

let pass_through_header resp (cgi : cgi) =
  let fields = ["Content-Type", [resp.content_type]] in
  let fields =
    if resp.length < 0L then fields
    else ("Content-Length", [Int64.to_string resp.length]) :: fields
  in
  cgi#set_header ~status: `Ok ~fields ();
  debug_headers "Pass-through response" cgi#environment#output_header_fields

let finish_delivery resp =
  if should_pass_through (relative_url resp.location) then
    (* the request was redirected to content that should not be cached,
       like a directory listing *)
    remove_cache resp.cache
  else
    close_cache resp.cache resp.length resp.last_modified;
  if resp.length >= 0L or resp.cache = Pass_through then Delivered else Cached

let finish_head resp cgi =
  send_header resp.length resp.last_modified cgi;
  Delivered

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
    | "location" ->
        (try resp.location <- Neturl.string_of_url (Neturl.parse_url value)
         with Neturl.Malformed_URL ->
           error_message "Cannot parse Location %s" value)
    | "content-type" ->  (* only used for pass-through content *)
        resp.content_type <- value
    | _ -> ()
  in
  debug_message "  %s" str;
  try with_pair header_re str do_header
  with Not_found ->  (* e.g., status line or CRLF *)
    try with_pair status_re str do_status
    with Not_found -> error_message "Unrecognized response: %s" str

(* Process a chunk of the response body.
   If no Content-Length was present in the header, we cache the whole
   file before delivering it to the client.  The alternative -- using
   chunked transfer encoding -- triggers a bug in APT. *)

let process_body resp cgi str pos len =
  if resp.status = 200 then begin
    if not resp.body_seen then begin
      resp.body_seen <- true;
      assert (resp.cache = Undefined);
      resp.cache <- open_cache resp.name;
      if resp.cache = Pass_through then
        pass_through_header resp cgi
      else if resp.length >= 0L then
        send_header resp.length resp.last_modified cgi
    end;
    write_cache resp.cache str pos len;
    if resp.length >= 0L || resp.cache = Pass_through then
      (* stream the data back to the client as we receive it *)
      cgi#output#really_output str pos len
  end

(* Download a file from an HTTP or HTTPS repository *)

let download_http resp url name ims cgi =
  let headers =
    if ims > 0. then ["If-Modified-Since: " ^ Url.string_of_time ims] else []
  in
  let header_callback = process_header resp in
  let body_callback = process_body resp cgi in
  let is_head = head_request cgi#environment in
  let rec loop redirects =
    resp.status <- 0;
    if is_head then
      Url.head url header_callback
    else
      Url.download resp.location ~headers ~header_callback body_callback;
    match resp.status with
    | 200 -> if is_head then finish_head resp cgi else finish_delivery resp
    | 304 -> Not_modified
    | 301 | 302 | 303 | 307 ->
        if redirects < max_redirects then loop (redirects + 1)
        else begin
          error_message "Too many redirections for %s" url;
          File_not_found
        end
    | 404 -> File_not_found
    | n -> error_message "Unexpected status code: %d" n; File_not_found
  in
  loop 0

(* Download a file from an FTP repository *)

let download_ftp resp url name ims cgi =
  Url.head url (process_header resp);
  let mod_time = resp.last_modified in
  debug_message "  ims %s  mtime %s"
    (Url.string_of_time ims) (Url.string_of_time mod_time);
  if 0. < mod_time && mod_time <= ims then Not_modified
  else if head_request cgi#environment then finish_head resp cgi
  else begin
    resp.status <- 200;  (* for process_body *)
    Url.download url (process_body resp cgi);
    finish_delivery resp
  end

let download_url url name ims cgi =
  let dl =
    match Url.protocol url with
    | Url.HTTP | Url.HTTPS -> download_http
    | Url.FTP | Url.FILE -> download_ftp
  in
  let resp = new_response url name in
  try
    create_hint name;
    unwind_protect
      (fun () -> dl resp url name ims cgi)
      (fun () -> remove_hint name)
  with e ->
    remove_cache resp.cache;
    if e <> Failure url then info_message "%s" (string_of_exception e);
    File_not_found

(* Perform any pdiff processing triggered by downloading a given file *)

let cleanup_after url file =
  if pdiffs && Release.is_pdiff file then
    try Pdiff.apply file
    with e -> info_message "%s" (string_of_exception e)

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
  if not (head_request cgi#environment) then
    with_in_channel open_in name (copy_to output);
  output#commit_work ()

let serve_remote url name ims mod_time cgi =
  let respond code =
    raise (Nethttpd_types.Standard_response (code, None, None))
  in
  let copy_if_newer () =
    (* deliver the cached copy if it is newer than the client's *)
    if mod_time > ims then copy_from_cache name cgi
    else respond `Not_modified
  in
  let status = download_url url name (max ims mod_time) cgi in
  info_message "%s: %s" url (string_of_download_status status);
  match status with
  | Delivered ->
      cgi#output#commit_work ();
      if not (head_request cgi#environment) then cleanup_after url name
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
  debug_message "  => cache miss";
  `Accept_body (remote_service url name ims mod_time)

let ims_time env =
  try Netdate.parse_epoch (env#input_header#field "If-Modified-Since")
  with Not_found | Invalid_argument _ -> 0.

let server_error e =
  let bt = Printexc.get_backtrace () in
  if bt <> "" then begin
    error_message "%s" "Uncaught exception";
    let pr s = if s <> "" then error_message "  %s" s in
    List.iter pr (split_lines bt)
  end;
  `Std_response (`Internal_server_error, None, Some (string_of_exception e))

let is_repository name =
  try String.index name '/' = String.length name - 1
  with Not_found -> true

let redirect url =
  debug_message "  => redirect to %s" url;
  let header = new Netmime.basic_mime_header ["Location", url] in
  `Std_response (`Temporary_redirect, Some header, None)

let serve_file env =
  (* handle URL-encoded '+', '~', etc. *)
  let path = Netencoding.Url.decode ~plus: false env#cgi_request_uri in
  if path = "/" then
    `Static (`Ok, None, if head_request env then "" else Config.index)
  else
    try
      let url, name = Url.translate_request path in
      if is_repository name then redirect url
      else if should_pass_through name then cache_miss url name 0. 0.
      else
        let ims = ims_time env in
        match serve_local name ims env with
        | Done reaction -> reaction
        | Cache_miss mod_time -> cache_miss url name ims mod_time
    with
    | Not_found -> `Std_response (`Not_found, None, None)
    | e -> server_error e

let process_request env =
  debug_message "Connection from %s"
    (string_of_sockaddr env#remote_socket_addr ~with_port: true);
  let meth = env#cgi_request_method in
  debug_headers (sprintf "Request: %s %s" meth env#cgi_request_uri)
    env#input_header_fields;
  if (meth = "GET" || meth = "HEAD") && env#cgi_query_string = "" then
    serve_file env
  else
    `Std_response (`Forbidden, None, Some "invalid HTTP request")

let error_response code =
  let msg =
    try Nethttp.string_of_http_status (Nethttp.http_status_of_int code)
    with Not_found -> "???"
  in
  sprintf "<html><title>%d %s</title><body><h1>%d: %s</h1></body></html>"
    code msg code msg

let config =
  object
    (* http_protocol_config *)
    method config_max_reqline_length = 256
    method config_max_header_length = 32768
    method config_max_trailer_length = 32768
    method config_limit_pipeline_length = 5
    method config_limit_pipeline_size = 250000
    method config_announce_server = `Ocamlnet_and ("approx/" ^ version)
    (* http_processor_config *)
    method config_timeout_next_request = 15.
    method config_timeout = 300.
    method config_cgi = Netcgi1_compat.Netcgi_env.default_config
    method config_error_response n = error_response n
    method config_log_error _ _ _ _ msg = error_message "%s" msg
    (* http_reactor_config *)
    method config_reactor_synch = `Write
  end

let proxy_service =
  object
    method name = "proxy_service"
    method def_term = `Proxy_service
    method print fmt = Format.fprintf fmt "%s" "proxy_service"
    method process_header = process_request
  end

let approx () =
  check_id ~user ~group;
  Sys.chdir cache_dir;
  set_nonblock stdin;
  Nethttpd_reactor.process_connection config stdin proxy_service

let () = main_program approx ()
