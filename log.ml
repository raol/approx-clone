(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

let printer = ref (fun _ msg -> prerr_string msg; flush stderr)

let message level =
  (* ensure message is newline-terminated,
     otherwise syslog-ng behaves differently than syslog *)
  let terminate str =
    let n = String.length str in
    if n = 0 then "\n"
    else if str.[n - 1] = '\n' then str
    else str ^ "\n"
  in
  ksprintf (fun str -> !printer level (terminate str))

let error_message fmt = message `LOG_ERR fmt
let info_message fmt = message `LOG_INFO fmt
let debug_message fmt = message `LOG_DEBUG fmt

let exception_message exc = error_message "%s" (string_of_exception exc)

let facility =
  match Default_config.syslog with
  | "auth" -> `LOG_AUTH
  | "authpriv" -> `LOG_AUTHPRIV
  | "console" -> `LOG_CONSOLE
  | "cron" -> `LOG_CRON
  | "daemon" -> `LOG_DAEMON
  | "ftp" -> `LOG_FTP
  | "kern" -> `LOG_KERN
  | "lpr" -> `LOG_LPR
  | "mail" -> `LOG_MAIL
  | "news" -> `LOG_NEWS
  | "ntp" -> `LOG_NTP
  | "security" -> `LOG_SECURITY
  | "syslog" -> `LOG_SYSLOG
  | "user" -> `LOG_USER
  | "uucp" -> `LOG_UUCP
  | "local0" -> `LOG_LOCAL0
  | "local1" -> `LOG_LOCAL1
  | "local2" -> `LOG_LOCAL2
  | "local3" -> `LOG_LOCAL3
  | "local4" -> `LOG_LOCAL4
  | "local5" -> `LOG_LOCAL5
  | "local6" -> `LOG_LOCAL6
  | "local7" -> `LOG_LOCAL7
  | fac -> error_message "Unknown syslog facility: %s" fac; `LOG_DAEMON

let use_syslog () =
  let prog = Filename.basename Sys.argv.(0) in
  try
    let log = Syslog.openlog ~facility prog in
    printer := Syslog.syslog log
  with _ ->
    error_message "Cannot connect to system logger";
    printer := (fun _ -> ignore)
