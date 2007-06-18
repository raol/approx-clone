(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix
open Nethttp
open Nethttpd_reactor
open Util
open Log

let error_response code =
  let msg =
    try string_of_http_status (http_status_of_int code)
    with Not_found -> "???"
  in
  sprintf "<html><title>%d %s</title><body><h1>%d: %s</h1></body></html>"
    code msg code msg

let version = Version.name ^ "/" ^ Version.number

let config =
  object
    (* http_protocol_config *)
    method config_max_reqline_length = 256
    method config_max_header_length = 32768
    method config_max_trailer_length = 32768
    method config_limit_pipeline_length = 5
    method config_limit_pipeline_size = 250000
    method config_announce_server = `Ocamlnet_and version
    (* http_processor_config *)
    method config_timeout_next_request = 15.
    method config_timeout = 300.
    method config_cgi = Netcgi1_compat.Netcgi_env.default_config
    method config_error_response n = error_response n
    method config_log_error _ _ _ _ msg = error_message "%s" msg
    (* http_reactor_config *)
    method config_reactor_synch = `Write
  end

let main ~user ~group ~interface port service =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let addr =
    if interface = "any" then inet_addr_any
    else Internet.address_of_interface interface
  in
  bind sock (ADDR_INET (addr, port));
  listen sock 10;
  drop_privileges ~user ~group;
  while true do
    let fd, _ = accept sock in
    set_nonblock fd;
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
