(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Abstract type representing a sequence of ed commands
   (append, change, or delete) *)

(* Note that "diff --ed" also produces substitute commands of the form
   "s/.././" in the case where a "." text line is emitted as ".." and
   then modified. These are not handled here since valid Debian
   control files cannot contain "." lines. *)

type t

(* Parse a stream of ed commands *)

val parse : in_channel -> t

(* Apply a patch sequence *)

val apply : t -> in_channel -> out_channel -> unit
