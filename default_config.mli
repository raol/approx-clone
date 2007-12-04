(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val config_file : string
val cache_dir : string

val interface : string
val port : int
val max_rate : string  (* bytes/second with optional K, M, or G suffix *)

val user : string
val group : string
val syslog : string

val pdiffs : bool
val offline : bool
val max_wait : int     (* seconds *)

val verbose : bool
val debug : bool

(* Print the configuration by applying the given function to each line *)

val print_config : (string -> unit) -> unit
