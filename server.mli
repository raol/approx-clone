(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val main : user:string -> interface:string -> int -> 'a Nethttpd_types.http_service -> unit
