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

let main port service =
  let session input output =
    let fd = descr_of_in_channel input in
    set_nonblock fd;
    process_connection config fd service
  in
  establish_server session (ADDR_INET (inet_addr_any, port))
