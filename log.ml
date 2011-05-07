(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Printf
open Syslog

let facility = facility_of_string Config.syslog
let ident = sprintf "%s[%d]" (Filename.basename Sys.argv.(0)) (Unix.getpid ())
let log = openlog ~facility ident

let message enabled level =
  (* ensure message is newline-terminated,
     otherwise syslog-ng behaves differently than syslog *)
  let terminate str =
    let n = String.length str in
    if n = 0 || str.[n - 1] <> '\n' then str ^ "\n"
    else str
  in
  ksprintf (fun str -> if enabled then syslog log level (terminate str))

let error_message fmt = message true `LOG_ERR fmt
let info_message fmt = message Config.verbose `LOG_INFO fmt
let debug_message fmt = message Config.debug `LOG_DEBUG fmt
