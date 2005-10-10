val use_syslog : unit -> unit

val error_message : ('a, unit, string, unit) format4 -> 'a
val info_message :  ('a, unit, string, unit) format4 -> 'a
val debug_message : ('a, unit, string, unit) format4 -> 'a
