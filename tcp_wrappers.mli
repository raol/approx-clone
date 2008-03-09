(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val hosts_ctl :
  ?address:string -> ?host:string -> ?user:string -> string -> bool
