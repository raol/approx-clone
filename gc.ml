(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Garbage-collect the approx cache using a mark-sweep algorithm *)

open Config
open Program
open Release
open Util

let usage () =
  print "Usage: approx-gc [options]
Garbage-collect the approx cache
Options:
    -f|--fast     do not validate checksums
    -k|--keep|-s|--simulate
                  do not remove files
    -q|--quiet    do not print file names
    -v|--verbose  print reason for removal";
  exit 1

let no_checksum = ref false
let simulate = ref false
let quiet = ref false
let verbose = ref false

let () =
  List.iter
    (function
       | "-f" | "--fast" -> no_checksum := true
       | "-k" | "--keep" | "-s" | "--simulate" -> simulate := true
       | "-q" | "--quiet" -> quiet := true
       | "-v" | "--verbose" -> verbose := true
       | _ -> usage ())
    arguments

let no_checksum = !no_checksum
let simulate = !simulate
let quiet = !quiet
let verbose = !verbose

(* The cache is probably only a small subset of all the files in the
   Debian archive, so we start with a table of filenames actually
   present in this cache, then check their validity as we process the
   Packages and Sources files *)

let file_table = Hashtbl.create 4096
let get_status = Hashtbl.find file_table
let set_status = Hashtbl.replace file_table
let iter_status proc = Hashtbl.iter proc file_table

(* The known distributions are the first-level directories in the cache *)

let distributions =
  List.filter
    (fun f -> Sys.is_directory (cache_dir ^/ f))
    (Array.to_list (Sys.readdir cache_dir))

(* Check if a file is part of a known distribution *)

let dist_is_known file =
  List.mem (fst (split_cache_path file)) distributions

(* Check if a Release file is no more than 5 minutes older
   than an InRelease file in the same directory, or vice versa *)

let is_current_release file =
  let current_with other =
    let dir = Filename.dirname file in
    let file' = dir ^/ other in
    not (Sys.file_exists file') || is_cached_nak file' ||
    file_modtime file' -. file_modtime file < 300.
  in
  not (is_cached_nak file) &&
  match Filename.basename file with
  | "Release" -> current_with "InRelease"
  | "InRelease" -> current_with "Release"
  | "Release.gpg" -> true
  | _ -> false

(* Scan the cache and add candidates for garbage collection to the
   status table. If a file is not in this table, it will not be
   removed.

   Packages and Sources files are collected and returned in the list
   of roots for the marking phase, but are not added to the table
   themselves.

   DiffIndex files are also returned in the list of roots, so that
   pdiff files will be marked, and similarly for TranslationIndex files.

   Since Release files are unreachable from the roots and would
   otherwise be removed, they are added to the table only if
   there is a newer version. *)

let scan_files () =
  let scan roots file =
    let add () = set_status file None; roots in
    let skip_root () = file :: roots in
    let skip () = roots in
    if not (dist_is_known file) then
      add ()
    else if is_index file || is_diff_index file || is_i18n_index file then
      skip_root ()
    else if is_current_release file then
      skip ()
    else
      add ()
  in
  fold_non_dirs scan [] cache_dir

(* Handle the case of filename fields of the form ./path *)

let canonical path =
  if String.length path >= 2 && path.[0] = '.' && path.[1] = '/' then
    substring path ~from: 2
  else
    path

(* If a file is present in the status table, mark it with the result
   of checking its size and checksum against the given information *)

let mark_generic pf vf checksum (info, file) =
  let path = pf (canonical file) in
  try
    match get_status path with
    | None ->
        if is_cached_nak path then begin
          if minutes_old (file_ctime path) <= interval then
            (* keep it since it's reachable and current *)
            set_status path (Some Control_file.Valid)
        end else
          let status = vf path (Control_file.validate ?checksum info) in
          set_status path (Some status)
    | Some _ -> (* already marked *) ()
  with
    Not_found -> ()

let mark_file prefix = mark_generic ((^/) prefix) (fun f k -> k f)

let mark_package prefix fields =
  let filename = Control_file.lookup "filename" fields in
  let size = Int64.of_string (Control_file.lookup "size" fields) in
  let sum, func = Control_file.get_checksum fields in
  let checksum = if no_checksum then None else Some func in
  mark_file prefix checksum ((sum, size), filename)

let mark_source prefix fields =
  let dir = Control_file.lookup "directory" fields in
  let info = Control_file.lookup_info "files" fields in
  let checksum = if no_checksum then None else Some file_md5sum in
  List.iter (mark_file (prefix ^/ dir) checksum) info

(* Like mark_file, but deals with the complication that
   the DiffIndex file refers only to uncompressed pdiffs  *)

let mark_pdiff prefix =
  mark_generic (fun f -> prefix ^/ f ^ ".gz") with_decompressed

let mark_diff_index prefix index =
  let items = Control_file.read index in
  let pdiffs = Control_file.lookup_info "sha1-patches" items in
  let checksum = if no_checksum then None else Some file_sha1sum in
  List.iter (mark_pdiff prefix checksum) pdiffs

let mark_i18n_index prefix index =
  let items = Control_file.read index in
  let translations = Control_file.lookup_info "sha1" items in
  let checksum = if no_checksum then None else Some file_sha1sum in
  List.iter (mark_file prefix checksum) translations

let mark_index index =
  if verbose then print "[ %s ]" (shorten index);
  if is_index index then
    let dist, _ = split_cache_path index in
    let prefix = cache_dir ^/ dist in
    if is_packages_file index then
      Control_file.iter (mark_package prefix) index
    else if is_sources_file index then
      Control_file.iter (mark_source prefix) index
    else
      file_message index "not a Packages or Sources file"
  else if is_diff_index index then
    let prefix = Filename.dirname index in
    mark_diff_index prefix index
  else if is_i18n_index index then
    let prefix = Filename.dirname index in
    mark_i18n_index prefix index
  else
    file_message index "unexpected index file"

let mark () =
  let roots = scan_files () in
  let mark_root r =
    if not (is_cached_nak r) then mark_index r
  in
  List.iter mark_root roots

let status_suffix = function
  | None -> ""
  | Some (Control_file.Wrong_size _) -> ": incorrect size"
  | Some (Control_file.Wrong_checksum _) -> ": incorrect checksum"
  | Some Control_file.Valid -> assert false

let print_gc file status =
  if not quiet then
    print "%s%s" (shorten file) (if verbose then status_suffix status else "")

let inactive file =
  Unix.time () -. file_modtime file > 300. (* 5 minutes *)

let sweep () =
  let gc file = function
    | Some Control_file.Valid -> ()
    | status ->
        if inactive file then
          (print_gc file status;
           if not simulate then perform Sys.remove file)
        else if verbose then
          file_message file "not old enough to remove"
  in
  iter_status gc

let empty_dirs =
  let collect_empty list dir =
    try
      if Sys.readdir dir = [||] then dir :: list else list
    with e ->
      print "%s" (string_of_exception e);
      list
  in
  fold_dirs collect_empty []

let remove_dir dir =
  if not quiet then
    print "%s%s" (shorten dir) (if verbose then ": empty directory" else "/");
  (* any exception raised by rmdir will terminate the pruning loop *)
  if not simulate then perform Unix.rmdir dir

let rec prune () =
  match empty_dirs cache_dir with
  | [] -> ()
  | [dir] when dir = cache_dir -> () (* don't remove cache dir *)
  | list -> List.iter remove_dir list; if not simulate then prune ()

let garbage_collect () =
  if not simulate then drop_privileges ~user ~group;
  mark ();
  sweep ();
  prune ()

let () = main_program garbage_collect ()
