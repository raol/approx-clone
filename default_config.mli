(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val config_file : string
val cache_dir : string

val interface : string
val port : int
val interval : int  (* minutes *)
val max_wait : int  (* seconds *)
val max_rate : string  (* bytes/second with optional K, M, or G suffix *)
val debug : bool
