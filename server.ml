(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
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

type t = file_descr

let address interface = function
  | PF_INET ->
      if interface = "any" || interface = "all" then inet_addr_any
      else begin
        try
          Interface.address interface
        with Not_found ->
          error_message "IP address for interface %s not found" interface;
          raise Not_found
      end
  | PF_INET6 ->
      if interface = "any" || interface = "all" then inet6_addr_any
      else begin
        error_message
          "The $interface parameter (%s) is not supported for IPv6.\n\
             Attempting to listen on IPv4 socket instead.\n" interface;
        raise Not_found
      end
  | _ -> failwith "invalid protocol family"

let remote_address ~with_port = function
  | ADDR_INET (host, port) ->
      let addr = string_of_inet_addr host in
      if with_port then sprintf "%s:%d" addr port else addr
  | ADDR_UNIX path ->
      failwith ("Unix domain socket " ^ path)

let rec find_some f = function
  | x :: rest -> (match f x with Some y -> y | None -> find_some f rest)
  | [] -> raise Not_found

let init ~user ~group ~interface ~port =
  let make_socket pf =
    try
      let sock = socket pf SOCK_STREAM 0 in
      setsockopt sock SO_REUSEADDR true;
      bind sock (ADDR_INET (address interface pf, port));
      listen sock 10;
      Some sock
    with Unix.Unix_error (EAFNOSUPPORT, "socket", _) | Not_found -> None
  in
  try
    let sock = find_some make_socket [PF_INET6; PF_INET] in
    drop_privileges ~user ~group;
    sock
  with Not_found -> failwith "cannot listen on socket"

let loop sock service =
  while true do
    let fd, _ = accept sock in
    let address = remote_address (getpeername fd) ~with_port: false in
    if Tcp_wrappers.hosts_ctl Version.name ~address then
      match fork () with
      | 0 ->
          if fork () <> 0 then exit 0;
          close sock;
          set_nonblock fd;
          process_connection config fd service;
          exit 0
      | pid ->
          close fd;
          ignore (waitpid [] pid)
    else begin
      close fd;
      debug_message "Connection from %s denied by TCP wrappers" address
    end
  done
