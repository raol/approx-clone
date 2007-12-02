(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

let file =
  match Array.length Sys.argv with
  | 2 -> Sys.argv.(1)
  | _ -> eprintf "Usage: %s pdiff\n" Sys.argv.(0); exit 1

let () =
  ignore (with_in_channel open_file file Patch.parse);
  printf "Parsed %s\n" file
