(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix

let interface =
  match Array.length Sys.argv with
  | 1 -> "eth0"
  | 2 -> Sys.argv.(1)
  | _ -> eprintf "Usage: %s [interface]\n" Sys.argv.(0); exit 1

let () =
  try
    printf "%s: %s\n" interface
      (string_of_inet_addr (Interface.address interface))
  with Not_found ->
    eprintf "address of interface %s not found\n" interface;
    exit 1
