(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config
open Log
open Printf

let curl_command options url =
  "/usr/bin/curl --silent --location " ^
    String.concat " " options ^ " " ^
    quoted_string url

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

let finish = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      failwith "download failed"

let head url callback =
  let cmd = head_command url in
  if debug then debug_message "Command: %s" cmd;
  let chan = Unix.open_process_in cmd in
  iter_headers chan callback;
  finish (Unix.close_process_in chan)

let download_command headers_wanted headers =
  let options =
    (if headers_wanted then ["--include"] else []) @
    List.map (fun h -> "--header " ^ quoted_string h) headers
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
  let cmd = download_command (header_callback <> None) headers url in
  if debug then debug_message "Command: %s" cmd;
  let chan = Unix.open_process_in cmd in
  (match header_callback with
   | Some proc -> iter_headers chan proc
   | None -> ());
  iter_body chan callback;
  finish (Unix.close_process_in chan)
