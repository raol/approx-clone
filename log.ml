open Printf

let printer = ref (fun _ -> prerr_endline)

let use_syslog () =
  let prog = Filename.basename Sys.argv.(0) in
  let log = Syslog.openlog ~facility: `LOG_DAEMON prog in
  printer := Syslog.syslog log

let message level fmt = kprintf (fun str -> !printer level str) fmt

let error_message fmt = message `LOG_ERR fmt
let info_message fmt = message `LOG_INFO fmt
let debug_message fmt = message `LOG_DEBUG fmt
