(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf

let string_of_uerror = function
  | err, str, "" -> sprintf "%s: %s" str (Unix.error_message err)
  | err, str, arg -> sprintf "%s: %s (%s)" str (Unix.error_message err) arg

let string_of_exception exc =
  match exc with
  | Failure str -> "Failure: " ^ str
  | Invalid_argument str -> "Invalid argument: " ^ str
  | Sys_error str -> str
  | Unix.Unix_error (err, str, arg)-> string_of_uerror (err, str, arg)
  | Control_file.Missing (par, field) ->
      sprintf "File %s, line %d: missing \"%s\" field"
        (Control_file.file_name par)
        (Control_file.line_number par)
        (String.capitalize field)
  | e -> Printexc.to_string e

let perform f x =
  try f x
  with e ->
    prerr_endline (string_of_exception e)

let main_program f x =
  try f x
  with e ->
    prerr_endline (string_of_exception e);
    Printexc.print_backtrace stderr;
    exit 1

let print fmt = ksprintf prerr_endline fmt
