(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val main : user:string -> group:string -> interface:string -> int -> 'a Nethttpd_types.http_service -> unit
