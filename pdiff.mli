(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Return the filename of the Packages or Sources file corresponding
   to a pdiff or a DiffIndex file *)

val index_file : string -> string

(* Update the given Packages or Sources file by applying any needed pdiffs *)

val update : string -> unit
