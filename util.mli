(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Create substring s.[from] .. s.[until-1] *)

val substring : ?from:int -> ?until:int -> string -> string

(* Split pathname into list of components.
   Initial and final "/" map to empty strings;
   "/" by itself maps to [""; ""] *)

val explode_path : string -> string list

(* Inverse of explode_path. *)

val implode_path : string list -> string

(* Infix operator to concatenate two pathname components. *)

val (^/) : string -> string -> string

(* Split absolute path into top-level directory and rest of path.
   Example: split_path "/a/b/c" = ("a", "b/c") *)

val split_path : string -> string * string
