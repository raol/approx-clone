open Printf
open Unix
open Nethttp
open Nethttpd_reactor
open Log (* Log.error_message shadows Unix.error_message *)

let error_response code =
  let msg =
    try string_of_http_status (http_status_of_int code)
    with Not_found -> "???"
  in
  sprintf "<html><title>%d %s</title><body><h1>%d: %s</h1></body></html>"
    code msg code msg

let config =
  object
    method config_max_reqline_length = 256
    method config_max_header_length = 32768
    method config_max_trailer_length = 32768
    method config_limit_pipeline_length = 5
    method config_limit_pipeline_size = 250000

    method config_timeout_next_request = 15.
    method config_timeout = 300.
    method config_cgi = Netcgi_env.default_config
    method config_error_response n = error_response n
    method config_log_error _ _ _ _ msg = error_message "%s" msg

    method config_reactor_synch = `Write
  end

let serial = false

let main ~user ~interface port service =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let addr =
    if interface = "any" then inet_addr_any
    else Netif.inet_addr_of_interface interface
  in
  bind sock (ADDR_INET (addr, port));
  listen sock 10;
  setuid (Unix.getpwnam user).Unix.pw_uid;  (* drop privileges *)
  while true do
    let fd, _ = accept sock in
    set_nonblock fd;
    if serial then
      process_connection config fd service
    else
      match fork () with
      | 0 ->
	  if fork () <> 0 then exit 0;
	  close sock;
	  process_connection config fd service;
	  exit 0
      | pid ->
	  close fd;
	  ignore (waitpid [] pid)
  done
