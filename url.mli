(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val iter :
  string ->
  ?headers:string list ->
  ?header_callback:(string -> unit) ->
  (string -> unit) -> unit

val mod_time : string -> float
