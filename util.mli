(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Extract substring s.[from] .. s.[until-1] *)

val substring : ?from:int -> ?until:int -> string -> string

(* Split a string at each occurrence of a separator *)

val split : char -> string -> string list

(* Split a string into lines *)

val split_lines : string -> string list

(* Split a pathname into a list of components.
   Initial and final "/" map to empty strings;
   "/" by itself maps to [""; ""] *)

val explode_path : string -> string list

(* Inverse of explode_path *)

val implode_path : string list -> string

(* Return a quoted string *)

val quoted_string : string -> string

(* Infix operator to concatenate two pathname components *)

val (^/) : string -> string -> string

(* Return the relative portion of a pathname *)

val relative_path : string -> string

(* Return the relative portion of a URL *)

val relative_url : string -> string

(* Extract the extension of a filename, excluding the '.' *)

val extension : string -> string option

(* Call a function making sure that a cleanup procedure is called
   before returning the result of the function or raising an exception *)

val unwind_protect : (unit -> 'a) -> (unit -> unit) -> 'a

(* Open an input channel and apply a function to the channel,
   using unwind_protect to ensure that the channel gets closed *)

val with_channel : ('a -> in_channel) -> 'a -> (in_channel -> 'b) -> 'b

(* Open a file for input, decompressing it if necessary *)

val open_file : string -> in_channel

(* Test if a file is a directory *)

val is_directory : string -> bool

(* Fold a function over each directory below a given path *)

val fold_dirs : ('a -> string -> 'a) -> 'a -> string -> 'a

val iter_dirs : (string -> unit) -> string -> unit

(* Fold a function over each non-directory below a given path *)

val fold_non_dirs : ('a -> string -> 'a) -> 'a -> string -> 'a

val iter_non_dirs : (string -> unit) -> string -> unit

(* Return the modification time of a file *)

val file_modtime : string -> float

(* Return the size of a file *)

val file_size : string -> int64

(* Return the MD5 digest of a file *)

val file_md5sum : string -> string

(* Return the SHA1 digest of a file *)

val file_sha1sum : string -> string

(* Return the SHA256 digest of a file *)

val file_sha256sum : string -> string

(* Drop privileges (user and group ID) to those of the specified name *)

val drop_privileges : string -> unit

(* Check whether a file is a Sources file *)

val is_sources : string -> bool

(* If a file is an index (Packages or Sources file),
   return the name and modification time of the most recent variant *)

val latest_index : string -> (string * float) option

(* Return a descriptive message for an exception *)

val string_of_exception : exn -> string

(* Run the main function of a program and print any uncaught exceptions *)

val main_program : ('a -> unit) -> 'a -> unit
