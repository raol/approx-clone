(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

type url_method = HTTP | FTP | FILE

val method_of : string -> url_method

val head : string -> (string -> unit) -> unit

val download :
  string ->
  ?headers:string list ->
  ?header_callback:(string -> unit) ->
  (string -> int -> int -> unit) -> unit

val download_file : url:string -> file:string -> unit
