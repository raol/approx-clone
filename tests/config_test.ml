(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf

let file =
  match Array.length Sys.argv with
  | 2 -> Sys.argv.(1)
  | _ -> eprintf "Usage: %s config-file\n" Sys.argv.(0); exit 1

let () =
  Config_file.read file;
  Config_file.iter (fun k v -> printf "%s: %s\n" k v)
