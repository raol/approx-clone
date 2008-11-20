(* approx: proxy server for Debian archive files *)

let copyright =
   "Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>\n\
    Released under the GNU General Public License"

open Printf
open Unix
open Unix.LargeFile
open Util
open Config
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
  prerr_endline copyright;
  exit 0

let foreground = ref false

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--foreground" -> foreground := true
    | "-v" | "--version" -> version ()
    | _ -> usage ()
  done

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
  ["Content-Type", "text/plain"; "Content-Length", Int64.to_string size] @
  (if modtime <> 0. then ["Last-Modified", Url.string_of_time modtime] else [])

type local_status =
  | Done of Nethttpd_types.http_service_reaction
  | Stale of float
  | Missing

(* Deliver a file from the local cache *)

let deliver_local name env =
  let size = file_size name in
  env#set_output_header_fields (proxy_headers size (file_modtime name));
  debug_headers "Local response" env#output_header_fields;
  Done (`File (`Ok, None, cache_dir ^/ name, 0L, size))

let not_modified = Done (`Std_response (`Not_modified, None, None))

let cache_hit name ims mod_time env =
  if Release.immutable name || Release.valid_file name then
    if mod_time <= ims then begin
      debug_message "  => not modified";
      not_modified
    end else begin
      debug_message "  => delivering from cache";
      deliver_local name env
    end
  else Missing

let not_found = Done (`Std_response (`Not_found, None, None))

let deny name =
  debug_message "Denying %s" name;
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

let new_response =
  let initial_state =
    { name = "";
      status = 0;
      length = -1L;
      last_modified = 0.;
      location = "";
      content_type = "text/plain";
      body_seen = false;
      cache = Undefined }
  in
  fun url name -> { initial_state with name = name; location = url }

type cgi = Netcgi1_compat.Netcgi_types.cgi_activation

let send_header size modtime (cgi : cgi) =
  let headers = proxy_headers size modtime in
  let fields = List.map (fun (name, value) -> (name, [value])) headers in
  cgi#set_header ~status: `Ok ~fields ();
  debug_headers "Proxy response" cgi#environment#output_header_fields

let pass_through_header resp (cgi : cgi) =
  let fields =
    ["Content-Type", [resp.content_type]] @
    (if resp.length >= 0L then
       ["Content-Length", [Int64.to_string resp.length]]
     else [])
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

(* Download a file from an HTTP repository *)

let download_http resp url name ims cgi =
  let headers =
    if ims > 0. then ["If-Modified-Since: " ^ Url.string_of_time ims] else []
  in
  let header_callback = process_header resp in
  let body_callback = process_body resp cgi in
  let rec loop redirects =
    resp.status <- 0;
    Url.download resp.location ~headers ~header_callback body_callback;
    match resp.status with
    | 200 -> finish_delivery resp
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
  else begin
    resp.status <- 200;  (* for process_body *)
    Url.download url (process_body resp cgi);
    finish_delivery resp
  end

let download_url url name ims cgi =
  let dl =
    match Url.protocol url with
    | Url.HTTP -> download_http
    | Url.FTP | Url.FILE -> download_ftp
  in
  let resp = new_response url name in
  let download_with_hint () =
    create_hint name;
    unwind_protect
      (fun () -> dl resp url name ims cgi)
      (fun () -> remove_hint name)
  in
  try download_with_hint ()
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
  info_message "%s: %s" url (string_of_download_status status);
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
  debug_message "  => cache miss";
  `Accept_body (remote_service url name ims mod_time)

let ims_time env =
  try Netdate.parse_epoch (env#input_header#field "If-Modified-Since")
  with Not_found | Invalid_argument _ -> 0.

let forbidden msg = `Std_response (`Forbidden, None, Some msg)

let serve_file env =
  (* handle URL-encoded '+', '~', etc. *)
  let path = Netencoding.Url.decode ~plus: false env#cgi_request_uri in
  debug_headers (sprintf "Request %s" path) env#input_header_fields;
  try
    let url, name = Url.translate_request path in
    if should_pass_through name then cache_miss url name 0. 0.
    else
      let ims = ims_time env in
      match serve_local name ims env with
      | Done reaction -> reaction
      | Stale mod_time -> cache_miss url name ims mod_time
      | Missing -> cache_miss url name ims 0.
  with Failure msg | Invalid_argument msg-> forbidden msg

let proxy_service =
  object
    method name = "proxy_service"
    method def_term = `Proxy_service
    method print fmt = Format.fprintf fmt "%s" "proxy_service"
    method process_header env =
      debug_message "Connection from %s"
        (Server.remote_address env#remote_socket_addr ~with_port: true);
      if env#cgi_request_method = "GET" && env#cgi_query_string = "" then
        serve_file env
      else begin
        debug_headers (sprintf "Request %s" env#cgi_request_uri)
          env#input_header_fields;
        forbidden "invalid HTTP request"
      end
  end

let server s =
  Sys.chdir cache_dir;
  info_message "Version: %s" Version.number;
  print_config (info_message "%s");
  Server.loop s proxy_service

let daemonize proc x =
  ignore (setsid ());
  use_syslog ();
  List.iter close [stdin; stdout; stderr];
  (* double fork to detach daemon *)
  if fork () = 0 && fork () = 0 then
    proc x

let () =
  try
    let s = Server.init ~user ~group ~interface ~port in
    if !foreground then server s
    else daemonize server s
  with e ->
    error_message "%s" (string_of_exception e);
    exit 1
