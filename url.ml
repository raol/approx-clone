(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config
open Log
open Printf

let head_command url =
  sprintf "/usr/bin/curl --silent --head %s" (quoted_string url)

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

let head url callback =
  let cmd = head_command url in
  if debug then debug_message "Command: %s" cmd;
  let chan = Unix.open_process_in cmd in
  iter_headers chan callback;
  ignore (Unix.close_process_in chan)

let download_command url headers headers_wanted =
  let buf = Buffer.create 200 in
  let add_header str h =
    sprintf "%s --header %s" str (quoted_string h)
  in
  sprintf "/usr/bin/curl --silent%s%s %s"
    (if headers_wanted then " --include" else "")
    (List.fold_left add_header "" headers)
    (quoted_string url)

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
  let cmd = download_command url headers (header_callback <> None) in
  if debug then debug_message "Command: %s" cmd;
  let chan = Unix.open_process_in cmd in
  (match header_callback with
   | Some proc -> iter_headers chan proc
   | None -> ());
  iter_body chan callback;
  ignore (Unix.close_process_in chan)
