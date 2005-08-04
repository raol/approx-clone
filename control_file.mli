(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* The format of Debian control files is defined in
   http://www.debian.org/doc/debian-policy/ch-controlfields.html *)

(* A paragraph is represented as a list of (field, value) pairs *)

type paragraph = (string * string) list

(* Fold a function over each paragraph in a Debian control file *)

val fold : ('a -> paragraph -> 'a) -> 'a -> string -> 'a

(* Apply a procedure to each paragraph in a Debian control file *)

val iter : (paragraph -> unit) -> string -> unit

(* Map a function over each paragraph in a Debian control file *)

val map : (paragraph -> 'a) -> string -> 'a list

(* A more efficient alternative to map that builds the result list
   in reverse order *)

val rev_map : (paragraph -> 'a) -> string -> 'a list

(* Return a list of paragraphs satisfying a predicate *)

val filter : (paragraph -> bool) -> string -> paragraph list

(* A more efficient alternative to filter that builds the result list
   in reverse order *)

val rev_filter : (paragraph -> bool) -> string -> paragraph list
