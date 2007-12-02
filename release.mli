(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Find the first directory in the path leading to the given file
   that contains a Release file, or raise Not_found *)

val find_directory : string -> string

(* Abstract type representing Release file contents *)

type t

(* Read the Release file corresponding to the given file, or raise Not_found *)

val read : string -> t

(* Validate a file using the information from a Release file *)

val validate : t -> string -> bool

(* Check if a file is valid according to the corresponding Release file *)

val valid_file : string -> bool

(* Check if a file is an index (Packages, Sources, or a compressed version) *)

val is_index : string -> bool

(* Check if a file is a Release file *)

val is_release : string -> bool

(* Check if a file is a DiffIndex *)

val is_diff_index : string -> bool

(* Check if a file is a pdiff *)

val is_pdiff : string -> bool

(* Check if a file is immutable (deb, source file, or pdiff) *)

val immutable : string -> bool
