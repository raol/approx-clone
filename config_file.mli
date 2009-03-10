(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val read : string -> unit

val mem : string -> bool

val get : ?default:string -> string -> string
val get_int : ?default:int -> string -> int
val get_bool : ?default:bool -> string -> bool

val set : string -> string -> unit

val fold : (string -> string -> 'a -> 'a) -> 'a -> 'a

val iter : (string -> string -> unit) -> unit
