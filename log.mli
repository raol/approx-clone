(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

val error_message : ('a, unit, string, unit) format4 -> 'a
val info_message :  ('a, unit, string, unit) format4 -> 'a
val debug_message : ('a, unit, string, unit) format4 -> 'a

val log_to_stderr : unit -> unit (* default *)
val log_to_syslog : unit -> unit
