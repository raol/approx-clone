(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Create substring s.[from] .. s.[until-1] *)

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

(* Infix operator to concatenate two pathname components *)

val (^/) : string -> string -> string

(* Return the relative portion of a pathname *)

val relative_path : string -> string

(* Extract the extension of a filename, excluding the '.' *)

val extension : string -> string option

(* Call a function making sure that a cleanup procedure is called
   before returning the result of the function or raising an exception *)

val unwind_protect : (unit -> 'a) -> (unit -> unit) -> 'a

(* Open an input channel and apply a function to the channel,
   using unwind_protect to ensure that the channel gets closed *)

val with_channel : ('a -> in_channel) -> 'a -> (in_channel -> 'b) -> 'b

(* Applying a procedure to each non-directory below a given path
   in the filesystem tree *)

val treewalk : (string -> unit) -> string -> unit

(* Return the modification time of a file *)

val file_modtime : string -> float

(* Return the size of a file *)

val file_size : string -> int

(* Return the MD5 digest of a file *)

val file_md5sum : string -> string
