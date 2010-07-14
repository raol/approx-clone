(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val version : string

val arguments : string list (* without config file args *)

val cache_dir : string

val max_rate : string (* bytes/second with optional K, M, or G suffix *)
val max_redirects : int

val user : string
val group : string
val syslog : string

val pdiffs : bool
val offline : bool
val max_wait : int (* seconds *)

val verbose : bool
val debug : bool

val index : string (* simple HTML index for the server *)

(* Extract the distribution and relative filename
   from the absolute pathname of a file in the cache.
   Example: split_pathname "/var/cache/approx/debian/pool/main/..."
   returns ("debian", "pool/main/...") *)

val split_cache_path : string -> string * string

(* Remove cache directory prefix from a pathname, if present *)

val shorten : string -> string
