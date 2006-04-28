(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val config_file : string
val cache_dir : string

val interface : string
val port : int
val interval : int  (* minutes *)
val max_wait : int  (* seconds *)
val debug : bool
