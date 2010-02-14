(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Config
open Log

type t = string * ((Control_file.info * string) list * (string -> string))

let find_directory file =
  let rec loop i =
    let dir = substring file ~until: i in
    if Sys.file_exists (dir ^/ "Release") then dir
    else loop (String.index_from file (i + 1) '/')
  in
  (* if pathname is absolute, start relative to the cache directory *)
  let start =
    if file.[0] <> '/' then 0
    else if is_prefix cache_dir file then String.length cache_dir + 1
    else invalid_arg "Release.find_directory"
  in
  loop start

let read file =
  let rdir = find_directory file in
  rdir, Control_file.read_checksum_info (rdir ^/ "Release")

let validate (rdir, (info_list, checksum)) file =
  Sys.file_exists file &&
  let rfile = substring file ~from: (String.length rdir + 1) in
  try
    let info = fst (List.find (fun (_, name) -> name = rfile) info_list) in
    Control_file.is_valid checksum info file
  with Not_found ->
    if Filename.dirname file <> rdir then
      debug_message "%s: not found in %s/Release" (shorten file) (shorten rdir);
    false

let valid_file file = try validate (read file) file with Not_found -> false

let is_variant variants file = List.mem (Filename.basename file) variants

let is_packages_file = is_variant (compressed_versions "Packages")

let is_sources_file = is_variant (compressed_versions "Sources")

let is_index file = is_packages_file file || is_sources_file file

let is_release file =
  match Filename.basename file with
  | "Release" | "Release.gpg" -> true
  | _ -> false

let diff_index_dir file =
  Filename.check_suffix (Filename.dirname file) ".diff"

let is_diff_index file =
  Filename.basename file = "Index" && diff_index_dir file

let is_pdiff file =
  Filename.basename file <> "Index" && diff_index_dir file

let immutable_suffixes =
  [".deb"; ".udeb"; ".dsc"; ".diff.gz";
   ".tar.gz"; ".tar.bz2"; ".tar.lzma"; ".tar.xz"]

let immutable file =
  List.exists (Filename.check_suffix file) immutable_suffixes || is_pdiff file
