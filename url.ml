(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util
open Default_config
open Log

let string_of_time t =
  Netdate.format ~fmt: "%a, %d %b %Y %T GMT" (Netdate.create ~zone: 0 t)

let time_of_string = Netdate.parse_epoch

let split_cache_path path =
  if is_prefix cache_dir path then
    let i = String.length cache_dir + 1 in
    let j = String.index_from path i '/' in
    substring path ~from: i ~until: j, substring path ~from: (j + 1)
  else
    invalid_arg "split_cache_path"

let translate_request url =
  let path = relative_url url in
  match explode_path path with
  | dist :: rest ->
      (try implode_path (Config.get dist :: rest), path
       with Not_found -> failwith ("no remote repository for " ^ dist))
  | [] ->
      invalid_arg "translate_request"

let translate_file file =
  let dist, path = split_cache_path file in
  try Config.get dist ^/ path
  with Not_found -> invalid_arg ("translate_file " ^ file)

type protocol = HTTP | FTP | FILE

let protocol url =
  try
    match String.lowercase (substring url ~until: (String.index url ':')) with
    | "http" -> HTTP
    | "ftp" -> FTP
    | "file" -> FILE
    | proto -> invalid_arg ("unsupported URL protocol " ^ proto)
  with Not_found ->
    invalid_arg ("no protocol in URL " ^ url)

let rate_option =
  match String.lowercase max_rate with
  | "" | "none" | "unlimited" -> ""
  | str -> "--limit-rate " ^ str

let curl_command options url =
  sprintf "/usr/bin/curl --fail --silent %s %s %s"
    rate_option (String.concat " " options) (quoted_string url)

let head_command = curl_command ["--head"]

let iter_headers proc chan =
  let next () =
    try Some (input_line chan)
    with End_of_file -> None
  in
  let rec loop () =
    match next () with
    | Some header ->
        let n = String.length header in
        if n > 0 && header.[n - 1] = '\r' then
          if n > 1 then begin
            proc (String.sub header 0 (n - 1));
            loop ()
          end else () (* CRLF terminates headers *)
        else error_message "Unexpected header: %s" header
    | None -> ()
  in
  loop ()

let head url callback =
  let cmd = head_command url in
  if debug then debug_message "Command: %s" cmd;
  with_process cmd ~error: url (iter_headers callback)

let download_command headers header_callback =
  let hdr_opts = List.map (fun h -> "--header " ^ quoted_string h) headers in
  let options =
    match header_callback with
    | Some _ -> "--include" :: hdr_opts
    | None -> hdr_opts
  in
  curl_command options

let iter_body proc chan =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input chan buf 0 len with
    | 0 -> ()
    | n -> proc buf 0 n; loop ()
  in
  loop ()

let seq f g x = (f x; g x)

let download url ?(headers=[]) ?header_callback callback =
  let cmd = download_command headers header_callback url in
  if debug then debug_message "Command: %s" cmd;
  with_process cmd ~error: url
    (match header_callback with
     | Some proc -> seq (iter_headers proc) (iter_body callback)
     | None -> iter_body callback)

let download_file file =
  let file' = gensym file in
  let options =
    ["--output"; file'; "--remote-time";
     "--location"; "--max-redirs"; string_of_int max_redirects] @
    (if Sys.file_exists file then
       ["--time-cond"; quoted_string (string_of_time (file_modtime file))]
     else [])
  in
  let cmd = curl_command options (translate_file file) in
  if debug then debug_message "Command: %s" cmd;
  if Sys.command cmd = 0 then
    (* file' may not exist if file was not modified *)
    try Sys.rename file' file with _ -> ()
  else begin
    rm file';
    failwith ("cannot download " ^ file)
  end
