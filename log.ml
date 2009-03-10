(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Syslog

let facility = facility_of_string Config.syslog
let log = openlog ~facility Sys.argv.(0)

let message enabled level =
  (* ensure message is newline-terminated,
     otherwise syslog-ng behaves differently than syslog *)
  let terminate str =
    let n = String.length str in
    if n = 0 then "\n"
    else if str.[n - 1] = '\n' then str
    else str ^ "\n"
  in
  Printf.ksprintf (fun str -> if enabled then syslog log level (terminate str))

let error_message fmt = message true `LOG_ERR fmt
let info_message fmt = message Config.verbose `LOG_INFO fmt
let debug_message fmt = message Config.debug `LOG_DEBUG fmt
