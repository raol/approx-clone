(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Return a list of files that have been invalidated by
   downloading the given file *)

val files_invalidated_by : string -> string list
