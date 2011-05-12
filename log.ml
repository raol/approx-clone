(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Printf
open Syslog

let stderr_log _ msg =
  prerr_string msg;
  flush stderr

let writer = ref stderr_log

let log_to_stderr () =
  writer := stderr_log

let log_to_syslog () =
  let facility = facility_of_string Config.syslog in
  let ident =
    sprintf "%s[%d]" (Filename.basename Sys.argv.(0)) (Unix.getpid ())
  in
  let log = openlog ~facility ident in
  writer := syslog log

let message enabled level =
  (* ensure message is newline-terminated,
     otherwise syslog-ng behaves differently than syslog *)
  let terminate str =
    let n = String.length str in
    if n = 0 || str.[n - 1] <> '\n' then str ^ "\n"
    else str
  in
  ksprintf (fun str -> if enabled then !writer level (terminate str))

let error_message fmt = message true `LOG_ERR fmt
let info_message fmt = message Config.verbose `LOG_INFO fmt
let debug_message fmt = message Config.debug `LOG_DEBUG fmt
