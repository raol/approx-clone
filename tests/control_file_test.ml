(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

let file =
  match Array.length Sys.argv with
  | 2 -> Sys.argv.(1)
  | _ -> eprintf "Usage: %s control-file\n" Sys.argv.(0); exit 1

let capitalize_parts str =
  join '-' (List.map String.capitalize (split '-' str))

let print_line = function
  | "" -> printf " .\n"
  | line -> printf " %s\n" line

let print_pair (field, value) =
  printf "%s:" (capitalize_parts field);
  match split_lines value with
  | [] -> print_newline ()
  | "" :: rest ->
      print_newline ();
      List.iter print_line rest
  | lines ->
      List.iter print_line lines

let print_paragraph p =
  List.iter print_pair p;
  print_newline ()

let () =
  Control_file.iter print_paragraph file
