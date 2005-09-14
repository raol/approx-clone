(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val head : string -> (string -> unit) -> unit

val download :
  string ->
  ?headers:string list ->
  ?header_callback:(string -> unit) ->
  (string -> int -> int -> unit) -> unit
