(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Find the newest InRelease or Release file in the given directory
   or raise Not_found *)

val newest : string -> string

(* Check if a file is valid according to the corresponding Release file *)

val valid_file : string -> bool

(* Check if a file is a possibly-compressed Packages file *)

val is_packages_file : string -> bool

(* Check if a file is a possibly-compressed Sources file *)

val is_sources_file : string -> bool

(* Check if a file is an index (Packages, Sources, or a compressed version) *)

val is_index : string -> bool

(* Check if a file is an InRelease, Release, or Release.gpg file *)

val is_release : string -> bool

(* Check if a file is a DiffIndex *)

val is_diff_index : string -> bool

(* Check if a file is a pdiff *)

val is_pdiff : string -> bool

(* Check if a file is immutable (deb, source file, or pdiff) *)

val immutable : string -> bool
