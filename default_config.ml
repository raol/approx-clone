(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config

let config_file = "/etc/approx/approx.conf"
let cache_dir = "/var/cache/approx"

let () = read config_file

let port = get_int "port" ~default: 9999 (* for compatibility with apt-proxy *)
let interval = get_int "interval" ~default: 720 (* minutes *)
let max_wait = get_int "max_wait" ~default: 10 (* seconds *)
let debug = get_bool "debug" ~default: false
