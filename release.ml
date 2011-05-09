(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Config
open Log

(* Find the newest InRelease or Release file in the given directory
   or raise Not_found *)

let newest dir = newest_file [dir ^/ "InRelease"; dir ^/ "Release"]

(* Find the Release file for the given file or raise Not_found *)

let find file =
  (* start relative to the cache directory *)
  let path =
    if file.[0] <> '/' then file
    else if is_prefix cache_dir file then
      substring file ~from: (String.length cache_dir + 1)
    else invalid_arg "Release.find"
  in
  match explode_path path with
  | dist :: "dists" :: suite :: _ -> newest (dist ^/ "dists" ^/ suite)
  | _ -> raise Not_found

let read file =
  let release = find file in
  release, Control_file.read_checksum_info release

let validate (release, (info_list, checksum)) file =
  Sys.file_exists file &&
  let rdir = Filename.dirname release in
  let rfile = substring file ~from: (String.length rdir + 1) in
  try
    let info = fst (List.find (fun (_, name) -> name = rfile) info_list) in
    Control_file.is_valid checksum info file
  with Not_found ->
    if Filename.dirname file <> rdir then
      debug_message "%s: not found in %s" (shorten file) (shorten release);
    false

let valid_file file =
  try validate (read file) file
  with Not_found | Control_file.Missing _ -> false

let is_variant variants file = List.mem (Filename.basename file) variants

let is_packages_file = is_variant (compressed_versions "Packages")

let is_sources_file = is_variant (compressed_versions "Sources")

let is_index file = is_packages_file file || is_sources_file file

let is_release file =
  match Filename.basename file with
  | "InRelease" | "Release" | "Release.gpg" -> true
  | _ -> false

let diff_index_dir file =
  Filename.check_suffix (Filename.dirname file) ".diff"

let is_diff_index file =
  Filename.basename file = "Index" && diff_index_dir file

let is_pdiff file =
  Filename.basename file <> "Index" && diff_index_dir file

let is_i18n_index file =
  Filename.basename file = "Index" &&
  Filename.basename (Filename.dirname file) = "i18n"

let immutable_suffixes =
  [".deb"; ".udeb"; ".dsc"; ".diff.gz"] @ compressed_versions ".tar"

let immutable file =
  List.exists (Filename.check_suffix file) immutable_suffixes || is_pdiff file
