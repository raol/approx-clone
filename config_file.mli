(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val read : string -> unit

val get : ?default:string -> string -> string
val get_int : ?default:int -> string -> int
val get_bool : ?default:bool -> string -> bool

val fold : (string -> string -> 'a -> 'a) -> 'a -> 'a

val iter : (string -> string -> unit) -> unit
