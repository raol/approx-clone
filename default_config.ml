(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config

let config_file = "/etc/approx/approx.conf"

let () = read config_file

let cache = get "cache" ~default: "/var/cache/approx"
let port = get_int "port" ~default: 9999
let interval = get_int "interval" ~default: 720 (* minutes *)
let debug = get_bool "debug" ~default: false
