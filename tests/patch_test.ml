(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

let diff_file, file_to_patch =
  match Array.length Sys.argv with
  | 2 -> Sys.argv.(1), None
  | 3 -> Sys.argv.(1), Some Sys.argv.(2)
  | _ -> eprintf "Usage: %s pdiff [file]\n" Sys.argv.(0); exit 1

let cmds = with_in_channel open_file diff_file Patch.parse

let () =
  match file_to_patch with
  | Some file ->
      with_in_channel open_file file (fun chan -> Patch.apply cmds chan stdout)
  | None ->
      printf "Parsed %s\n" diff_file
