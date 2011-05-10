(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* The format of Debian control files is defined in
   http://www.debian.org/doc/debian-policy/ch-controlfields.html *)

(* Abstract type respresenting a paragraph in a control file *)

type paragraph

(* Name of control file from which paragraph was read *)

val file_name : paragraph -> string

(* Line number at which paragraph starts *)

val line_number : paragraph -> int

(* Apply a procedure to each (field, value) pair *)

val iter_fields : (string * string -> unit) -> paragraph -> unit

(* Exception raised when a field lookup fails *)

exception Missing of paragraph * string

(* Check if a field is present *)

val defined : string -> paragraph -> bool

(* Find the value corresponding to a field name, or raise Missing *)

val lookup : string -> paragraph -> string

(* Fold a function over each paragraph in a Debian control file *)

val fold : ('a -> paragraph -> 'a) -> 'a -> string -> 'a

(* Apply a procedure to each paragraph in a Debian control file *)

val iter : (paragraph -> unit) -> string -> unit

(* Parse a Debian control file consisting of a single paragraph,
   such as a Release or DiffIndex file *)

val read : string -> paragraph

(* Return the strongest checksum information in a paragraph,
   along with the corresponding checksum function *)

val get_checksum : paragraph -> string * (string -> string)

(* File information: checksum and size *)

type info = string * int64

(* Parse a string consisting of checksum, size, and filename lines *)

val info_list : string -> (info * string) list

(* Apply info_list to the value of a field *)

val lookup_info : string -> paragraph -> (info * string) list

(* Read a single-paragraph control file and return a pair consisting of the
   list of ((checksum, size), filename) lines and the checksum function *)

val read_checksum_info : string -> (info * string) list * (string -> string)

(* Validate a file's checksum and size *)

type validity =
  | Valid
  | Wrong_size of int64
  | Wrong_checksum of string

val validate : ?checksum:(string -> string) -> info -> string -> validity

(* Check that a file matches its checksum and size *)

val valid : (string -> string) -> info -> string -> bool
