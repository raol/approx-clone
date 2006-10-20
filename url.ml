(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util
open Default_config
open Log

type url_method = HTTP | FTP | FILE

let method_of url =
  try
    match String.lowercase (substring url ~until: (String.index url ':')) with
    | "http" -> HTTP
    | "ftp" -> FTP
    | "file" -> FILE
    | meth -> invalid_arg ("unsupported URL method " ^ meth)
  with Not_found ->
    invalid_arg ("no method in URL " ^ url)

let rate_option =
  match String.lowercase max_rate with
  | "" | "none" | "unlimited" -> ""
  | str -> "--limit-rate " ^ str

let curl_command options url =
  sprintf "/usr/bin/curl --fail --silent --location %s %s %s"
    rate_option (String.concat " " options) (quoted_string url)

let head_command = curl_command ["--head"]

let iter_headers chan proc =
  let next () =
    try Some (input_line chan)
    with End_of_file -> None
  in
  let rec loop () =
    match next () with
    | Some header ->
	let n = String.length header in
	if n > 0 && header.[n - 1] = '\r' then
	  if n > 1 then
	    begin
	      proc (String.sub header 0 (n - 1));
	      loop ()
	    end
	  else
	    (* CRLF terminates headers *)
	    ()
	else
	  error_message "Unexpected header: %s" header
    | None -> ()
  in
  loop ()

let finish chan =
  if Unix.close_process_in chan <> Unix.WEXITED 0 then
    failwith "download failed"

let head url callback =
  let cmd = head_command url in
  if debug then debug_message "Command: %s" cmd;
  let chan = Unix.open_process_in cmd in
  iter_headers chan callback;
  finish chan

let download_command headers header_callback =
  let hdr_opts = List.map (fun h -> "--header " ^ quoted_string h) headers in
  let options =
    match header_callback with
    | Some _ -> "--include" :: hdr_opts
    | None -> hdr_opts
  in
  curl_command options

let iter_body chan proc =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input chan buf 0 len with
    | 0 -> ()
    | n -> proc buf 0 n; loop ()
  in
  loop ()

let download url ?(headers=[]) ?header_callback callback =
  let cmd = download_command headers header_callback url in
  if debug then debug_message "Command: %s" cmd;
  let chan = Unix.open_process_in cmd in
  (match header_callback with
   | Some proc -> iter_headers chan proc
   | None -> ());
  iter_body chan callback;
  finish chan

let download_file ~url ~file =
  let file' = file ^ ".tmp" in
  let cmd = curl_command ["--remote-time"; "--output"; file'] url in
  if debug then debug_message "Command: %s" cmd;
  if Sys.command cmd = 0 then
    Sys.rename file' file
  else
    failwith ("cannot download " ^ url)
