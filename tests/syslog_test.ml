(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Syslog

let msg =
  match Array.length Sys.argv with
  | 1 -> eprintf "Usage: %s message ...\n" Sys.argv.(0); exit 1
  | n -> String.concat " " (Array.to_list (Array.sub Sys.argv 1 (n - 1)))

let log = openlog Sys.argv.(0)
let () = syslog log `LOG_INFO msg
