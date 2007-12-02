(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Return the Packages or Sources file corresponding to a DiffIndex *)

val file_of_diff_index : string -> string

(* Return the pdiff directory corresponding to a Packages or Sources file *)

val directory : string -> string

(* Apply the given pdiff to the corresponding Packages or Sources file *)

val apply : string -> unit

(* Update the given Packages or Sources file by applying any needed pdiffs *)

val update : string -> unit
