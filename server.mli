(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

type t

val init : user:string -> group:string -> interface:string -> port:int -> t

val loop : t -> 'a Nethttpd_types.http_service -> unit

val remote_address : with_port:bool -> Unix.sockaddr -> string
