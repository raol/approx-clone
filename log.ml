(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util
open Syslog

let printer = ref (fun _ msg -> prerr_string msg; flush stderr)

let message enabled level =
  (* ensure message is newline-terminated,
     otherwise syslog-ng behaves differently than syslog *)
  let terminate str =
    let n = String.length str in
    if n = 0 then "\n"
    else if str.[n - 1] = '\n' then str
    else str ^ "\n"
  in
  ksprintf (fun str -> if enabled then !printer level (terminate str))

let error_message fmt = message true `LOG_ERR fmt
let info_message fmt = message Config.verbose `LOG_INFO fmt
let debug_message fmt = message Config.debug `LOG_DEBUG fmt

let exception_message exc = error_message "%s" (string_of_exception exc)

let facility = facility_of_string Config.syslog

let use_syslog () =
  try
    let log = openlog ~facility Version.name in
    printer := syslog log
  with _ ->
    error_message "Cannot connect to system logger";
    printer := (fun _ -> ignore)
