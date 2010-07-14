(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
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
  | _ ->
      eprintf "Usage: %s [-n] [path]\n" Sys.argv.(0);
      exit 1

let () =
  if non_dirs then
    let bigger (path, n as orig) path' =
      let n' = file_size path' in
      if n >= n' then orig else (path', n')
    in
    let biggest, n = fold_non_dirs bigger ("", 0L) path in
    printf "%Ld\t%s\n" n biggest
  else
    let bigger (path, n as orig) path' =
      let n' = (stat path').st_nlink in
      if n >= n' then orig else (path', n')
    in
    let biggest, n = fold_dirs bigger ("", 0) path in
    printf "%d\t%s\n" n biggest
