(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

let file =
  match Array.length Sys.argv with
  | 2 -> Sys.argv.(1)
  | _ -> eprintf "Usage: %s file\n" Sys.argv.(0); exit 1

let get_info chan =
  let size = LargeFile.in_channel_length chan in
  let checksum = Sha1.to_hex (Sha1.channel chan (-1)) in
  printf "%s %Ld\n" checksum size

let () = with_in_channel open_file file get_info
