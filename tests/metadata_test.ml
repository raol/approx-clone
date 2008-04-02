(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util
open Config

let files = ref []

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let arg = Sys.argv.(i) in
    if arg.[0] <> '-' then files := arg :: !files
    else (eprintf "Usage: %s [files]\n" Sys.argv.(0); exit 1)
  done

let files = List.rev !files

let check file =
  if Release.is_index file || Release.is_diff_index file then
    eprintf "%s: %s\n%!" file
      (if Release.valid_file file then "valid" else "invalid")

let () =
  if files <> [] then List.iter check files
  else iter_non_dirs check cache_dir
