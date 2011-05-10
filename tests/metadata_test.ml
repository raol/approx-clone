(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Config
open Program

let check show_immutable file =
  let pr = file_message file in
  let pv msg =
    pr ((if Release.valid file then "valid" else "invalid") ^ " " ^ msg)
  in
  if not (Sys.file_exists file) then pr "not found"
  else if is_cached_nak file then pr "cached NAK"
  else if Release.immutable file then (if show_immutable then pr "immutable")
  else if Release.is_release file then pr "release"
  else if Release.is_index file then pv "index"
  else if Release.is_diff_index file then pv "diff_index"
  else if Release.is_i18n_index file then pv "i18n_index"
  else pr "unknown"

let () =
  if arguments = [] then iter_non_dirs (check false) cache_dir
  else List.iter (check true) arguments
