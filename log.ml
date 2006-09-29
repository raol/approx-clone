(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf

let printer = ref (fun _ -> prerr_endline)

let message level fmt = ksprintf (fun str -> !printer level str) fmt

let error_message fmt = message `LOG_ERR fmt
let info_message fmt = message `LOG_INFO fmt
let debug_message fmt = message `LOG_DEBUG fmt

let use_syslog () =
  let prog = Filename.basename Sys.argv.(0) in
  try
    let log = Syslog.openlog ~facility: `LOG_DAEMON prog in
    printer := Syslog.syslog log
  with _ ->
    error_message "Cannot connect to system logger";
    printer := (fun _ -> ignore)
