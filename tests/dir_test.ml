(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix
open Util

let non_dirs, path =
  match Sys.argv with
  | [| _ |] -> false, "."
  | [| _; "-n" |] -> true, "."
  | [| _; dir |] -> false, dir
  | [| _; "-n"; dir |] -> true, dir
  | _ -> eprintf "Usage: %s [-n] [path]\n" Sys.argv.(0); exit 1

let foldf, metric =
  if non_dirs then fold_non_dirs, file_size
  else fold_dirs, fun f -> Int64.of_int (stat f).st_nlink

let bigger (path, n as orig) path' =
  let n' = metric path' in
  print_endline path';
  if n >= n' then orig else (path', n')

let () =
  let biggest, n = foldf bigger ("", 0L) path in
  printf "\n%Ld\t%s\n" n biggest
