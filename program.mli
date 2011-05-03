(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Return a descriptive message for an exception *)

val string_of_exception : exn -> string

(* Call a procedure and print, but otherwise ignore, any exception *)

val perform : ('a -> unit) -> 'a -> unit

(* Run the main function of a program and print any uncaught exception *)

val main_program : ('a -> unit) -> 'a -> unit

(* Print on stderr and append a newline *)

val print : ('a, unit, string, unit) format4 -> 'a

(* Print a filename followed by a message on stderr *)

val file_message : string -> string -> unit
