(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config_file
open Util

let version = "4.0"

let config_file = "/etc/approx/approx.conf"

let cache_dir = "/var/cache/approx"

let split_cache_path path =
  if is_prefix cache_dir path then
    let i = String.length cache_dir + 1 in
    let j = String.index_from path i '/' in
    substring path ~from: i ~until: j, substring path ~from: (j + 1)
  else
    invalid_arg "split_cache_path"

let shorten path =
  if is_prefix cache_dir path then
    substring path ~from: (String.length cache_dir + 1)
  else
    path

let () = try read config_file with Sys_error _ -> ()

let max_rate = get "$max_rate" ~default: "unlimited"
let max_redirects = get_int "$max_redirects" ~default: 5

let user = get "$user" ~default: "approx"
let group = get "$group" ~default: "approx"
let syslog = get "$syslog" ~default: "daemon"

let pdiffs = get_bool "$pdiffs" ~default: true
let offline = get_bool "$offline" ~default: false
let max_wait = get_int "$max_wait" ~default: 10 (* seconds *)

let debug = get_bool "$debug" ~default: false
let verbose = get_bool "$verbose" ~default: false || debug
