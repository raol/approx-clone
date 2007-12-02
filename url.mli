(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Translate a request URL to the remote repository URL and
   return it together with the relative path for the cache *)

val translate_request : string -> string * string

(* Extract the distribution and relative filename
   from the absolute pathname of a file in the cache.
   Example: split_pathname "/var/cache/approx/debian/pool/main/..."
   returns ("debian", "pool/main/...") *)

val split_cache_path : string -> string * string

(* Find the remote URL corresponding to a given file in the cache *)

val translate_file : string -> string

type protocol = HTTP | FTP | FILE

val protocol : string -> protocol

(* Perform HTTP HEAD (or equivalent for FTP and FILE) on the given URL
   and apply a callback to each header that is returned *)

val head : string -> (string -> unit) -> unit

(* Download the specified URL with optional request headers,
   then apply callbacks to the headers and body chunks *)

val download :
  string ->
  ?headers:string list ->
  ?header_callback:(string -> unit) ->
  (string -> int -> int -> unit) -> unit

(* Download a file from a remote repository *)

val download_file : string -> unit

(* Format and parse HTTP-compliant times *)

val time_of_string : string -> float

val string_of_time : float -> string
