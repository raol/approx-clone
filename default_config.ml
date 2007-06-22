(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config

let config_file = "/etc/approx/approx.conf"
let cache_dir = "/var/cache/approx"

let () = read config_file

let interface = get "$interface" ~default: "any"
let port = get_int "$port" ~default: 9999 (* compatible with apt-proxy *)
let interval = get_int "$interval" ~default: 720 (* minutes *)
let max_wait = get_int "$max_wait" ~default: 10 (* seconds *)
let max_rate = get "$max_rate" ~default: "unlimited"
let user = get "$user" ~default: "approx"
let group = get "$group" ~default: "approx"
let syslog = get "$syslog" ~default: "daemon"
let debug = get_bool "$debug" ~default: false
let verbose = get_bool "$verbose" ~default: false || debug
