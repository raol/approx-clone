(* URL access in OCaml using Curl
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU Lesser General Public License *)

val iter : string -> ?header:string -> ?header_callback:(string -> unit) ->
	   (string -> unit) -> unit
