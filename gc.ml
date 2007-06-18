(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Garbage-collect the approx cache using a mark-sweep algorithm.
   Any file in the cache whose name, size, and checksum match an entry
   in a Packages file is assumed to be valid, and kept.
   Anything else, other than an index file from a known distribution,
   is assumed to be invalid, and removed. *)

open Util
open Default_config

let usage () =
  prerr_endline "Usage: gc_approx [options]
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
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--fast" -> no_checksum := true
    | "-k" | "--keep" | "-s" | "--simulate" -> simulate := true
    | "-q" | "--quiet" -> quiet := true
    | "-v" | "--verbose" -> verbose := true
    | _ -> usage ()
  done

let no_checksum = !no_checksum
let simulate = !simulate
let quiet = !quiet
let verbose = !verbose

let print_if yes fmt =
  Printf.ksprintf (fun str -> if yes then print_endline str) fmt

(* The cache is probably only a small subset of all the files in
   the Debian archive, so we start with a table of filenames
   actually present in this cache, and mark them as we process
   the Packages files *)

type file_status =
  | Unmarked
  | Valid
  | Wrong_size
  | Wrong_checksum

let files = Hashtbl.create 4096
let get_status = Hashtbl.find files
let set_status = Hashtbl.replace files
let iter_status proc = Hashtbl.iter proc files

(* Extract the distribution and relative filename
   from the absolute pathname of a file in the cache.
   Example: split_pathname "/var/cache/approx/debian/pool/main/..."
   returns ("debian", "pool/main/...") *)

let split_pathname path =
  let i = String.length cache_dir + 1 in
  let j = String.index_from path i '/' in
  substring path ~from: i ~until: j, substring path ~from: (j + 1)

(* Check if a file is part of a known distribution *)

let dist_is_known file =
  try
    let dist, _ = split_pathname file in
    ignore (Config.get dist);
    true
  with Not_found ->
    false

(* Check whether a file is a Release file *)

let is_release_file file =
  match Filename.basename file with
  | "Release" | "Release.gpg" -> true
  | _ -> false

(* Check if a file is an index (Packages or Sources file) and
   is the newest of any compressed variants of it *)

let newest_index_variant file =
  match latest_index file with
  | Some (name, _) -> name = file
  | None -> false

(* Handle the case of filename fields of the form ./path  *)

let canonical path =
  if String.length path >= 2 && path.[0] = '.' && path.[1] = '/' then
    substring path ~from: 2
  else
    path

(* We mark a file as live if its size and checksum
   match those specified in an index file *)

let mark_file checksum prefix (info, file) =
  let path = prefix ^/ canonical file in
  try
    if get_status path = Unmarked then
      match Control_file.validate ?checksum info path with
      | Control_file.Valid -> set_status path Valid
      | Control_file.Wrong_size _ -> set_status path Wrong_size
      | Control_file.Wrong_checksum _ -> set_status path Wrong_checksum
  with
    Not_found -> ()

let mark_package prefix fields =
  let filename = List.assoc "filename" fields in
  let size = Int64.of_string (List.assoc "size" fields) in
  let sum, func = Control_file.get_checksum fields in
  let checksum = if no_checksum then None else Some func in
  mark_file checksum prefix ((sum, size), filename)

let mark_source prefix fields =
  let dir = List.assoc "directory" fields in
  let files = List.assoc "files" fields in
  let info = Control_file.info_list files in
  let checksum = if no_checksum then None else Some file_md5sum in
  List.iter (mark_file checksum (prefix ^/ dir)) info

let mark_index index =
  print_if verbose "# %s" index;
  let dist, file = split_pathname index in
  let prefix = cache_dir ^/ dist in
  if is_sources index then
    Control_file.iter (mark_source prefix) index
  else
    Control_file.iter (mark_package prefix) index

(* Scan cache, collecting the list of roots (index files) and
   initializing the set of candidates for garbage collection *)

let scan_files () =
  let scan roots file =
    if dist_is_known file then
      if newest_index_variant file then
	file :: roots
      else
	(if not (is_release_file file) then set_status file Unmarked;
	 roots)
    else
      (set_status file Unmarked; roots)
  in
  fold_non_dirs scan [] cache_dir

let mark () =
  List.iter mark_index (scan_files ())

let status_prefix = function
  | Unmarked -> "  "
  | Wrong_size -> "= "
  | Wrong_checksum -> "! "
  | Valid -> assert false

let print_gc file status =
  let prefix = if verbose then status_prefix status else "" in
  print_if (not quiet) "%s%s" prefix file

let inactive file =
  Unix.time () -. file_modtime file > 300.  (* 5 minutes *)

let sweep () =
  let gc file = function
    | Valid -> ()
    | status ->
	if inactive file then
	  begin
	    print_gc file status;
	    if not simulate then Sys.remove file
	  end
	else
	  print_if verbose "%s is not old enough to remove" file
  in
  iter_status gc

let garbage_collect () =
  drop_privileges ~user ~group;
  mark ();
  sweep ()

let () = main_program garbage_collect ()
