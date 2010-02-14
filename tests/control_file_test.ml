(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

let verbose = ref false

let file =
  match Sys.argv with
  | [| _; file |] -> file
  | [| _; "-v"; file |] | [| _; "--verbose"; file |] ->
      verbose := true;
      file
  | _ ->
      eprintf "Usage: %s [-v] control-file\n" Sys.argv.(0);
      exit 1

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
  if !verbose then printf "[%d]\n" (Control_file.line_number p);
  Control_file.iter_fields print_pair p;
  print_newline ()

let () =
  Control_file.iter print_paragraph file
