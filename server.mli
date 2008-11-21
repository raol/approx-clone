(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val bind : interface:string -> port:int -> Unix.file_descr list

val loop : Unix.file_descr list -> 'a Nethttpd_types.http_service -> unit
