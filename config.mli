(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val config_file : string
val cache_dir : string

val max_rate : string  (* bytes/second with optional K, M, or G suffix *)
val max_redirects : int

val user : string
val group : string
val syslog : string

val pdiffs : bool
val offline : bool
val max_wait : int     (* seconds *)

val verbose : bool
val debug : bool
