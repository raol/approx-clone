(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* The format of Debian control files is defined in
   http://www.debian.org/doc/debian-policy/ch-controlfields.html *)

(* A paragraph is represented as a list of (field, value) pairs *)

type paragraph = (string * string) list

(* Fold a function over each paragraph in a Debian control file *)

val fold : ('a -> paragraph -> 'a) -> 'a -> string -> 'a

(* Apply a procedure to each paragraph in a Debian control file *)

val iter : (paragraph -> unit) -> string -> unit

(* Parse a Debian control file consisting of a single paragraph,
   like Release or Index files *)

val paragraph : string -> paragraph

(* Not used yet:

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

*)

(* Return the strongest checksum information in a paragraph,
   along with the corresponding checksum function *)

val get_checksum : paragraph -> string * (string -> string)

(* File information: checksum and size *)

type info = string * Int64.t

(* Parse a string consisting of checksum, size, and filename lines *)

val info_list : string -> (info * string) list

(* Check that a file matches its info *)

type validity =
  | Valid
  | Wrong_size of Int64.t
  | Wrong_checksum of string

val validate : ?checksum:(string -> string) -> info -> string -> validity
