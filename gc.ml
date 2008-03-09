(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Garbage-collect the approx cache using a mark-sweep algorithm.
   Any file in the cache whose name, size, and checksum match an entry
   in a Packages or Sources file is assumed to be valid, and kept.
   Anything else, other than a release or index file from a known
   distribution, is assumed to be invalid, and removed. *)

open Util
open Default_config
open Control_file

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
  Printf.ksprintf (fun str -> if yes then prerr_endline str) fmt

(* The cache is probably only a small subset of all the files in the
   Debian archive, so we start with a table of filenames actually
   present in this cache, then check their validity as we process the
   Packages and Sources files *)

let files = Hashtbl.create 4096
let get_status = Hashtbl.find files
let set_status = Hashtbl.replace files
let iter_status proc = Hashtbl.iter proc files

(* Check if a file is part of a known distribution *)

let dist_is_known file =
  try ignore (Url.translate_file file); true
  with Not_found -> false

(* Scan the cache, initializing the status of candidates for garbage
   collection to None.  Packages and Sources files should not be
   removed unless there is a newer version, so they are not entered
   into the status table.  Instead they are returned as a list of
   roots for the marking phase.  Release and Release.gpg files are
   also omitted since they are unreachable from the roots and would
   otherwise be removed. *)

let scan_files () =
  let scan roots file =
    if dist_is_known file then
      if Release.is_index file && file = newest_version file then file :: roots
      else if Release.is_release file then roots
      else (set_status file None; roots)
    else (set_status file None; roots)
  in
  fold_non_dirs scan [] cache_dir

(* Handle the case of filename fields of the form ./path  *)

let canonical path =
  if String.length path >= 2 && path.[0] = '.' && path.[1] = '/' then
    substring path ~from: 2
  else
    path

(* If a file is present in the status table, mark it with the result
   of checking its size and checksum against the given information *)

let mark_file checksum prefix (info, file) =
  let path = prefix ^/ canonical file in
  try
    match get_status path with
    | None -> set_status path (Some (validate ?checksum info path))
    | Some _ -> (* already marked *) ()
  with
    Not_found -> ()

let mark_package prefix fields =
  let filename = List.assoc "filename" fields in
  let size = Int64.of_string (List.assoc "size" fields) in
  let sum, func = get_checksum fields in
  let checksum = if no_checksum then None else Some func in
  mark_file checksum prefix ((sum, size), filename)

let mark_source prefix fields =
  let dir = List.assoc "directory" fields in
  let files = List.assoc "files" fields in
  let info = info_list files in
  let checksum = if no_checksum then None else Some file_md5sum in
  List.iter (mark_file checksum (prefix ^/ dir)) info

let mark_index index =
  print_if verbose "# %s" index;
  let dist, _ = Url.split_cache_path index in
  let prefix = cache_dir ^/ dist in
  if Release.is_sources_file index then
    Control_file.iter (mark_source prefix) index
  else
    Control_file.iter (mark_package prefix) index

let mark () =
  List.iter mark_index (scan_files ())

let status_prefix = function
  | None -> "  "
  | Some (Wrong_size _) -> "= "
  | Some (Wrong_checksum _) -> "! "
  | Some Valid -> assert false

let print_gc file status =
  let prefix = if verbose then status_prefix status else "" in
  print_if (not quiet) "%s%s" prefix file

let inactive file =
  Unix.time () -. file_modtime file > 300.  (* 5 minutes *)

let sweep () =
  let gc file = function
    | Some Valid -> ()
    | status ->
        if inactive file then begin
          print_gc file status;
          if not simulate then Sys.remove file
        end else
          print_if verbose "%s is not old enough to remove" file
  in
  iter_status gc

let empty_dirs =
  let collect_empty list dir =
    if Sys.readdir dir = [||] then dir :: list else list
  in
  fold_dirs collect_empty []

let remove_dir dir =
  print_if (not quiet) "%s%s" (if verbose then "  " else "") dir;
  (* any exception raised by rmdir will terminate the pruning loop *)
  if not simulate then Unix.rmdir dir

let rec prune () =
  match empty_dirs cache_dir with
  | [] -> ()
  | [dir] when dir = cache_dir -> ()  (* don't remove cache dir *)
  | list -> List.iter remove_dir list; if not simulate then prune ()

let garbage_collect () =
  drop_privileges ~user ~group;
  mark ();
  sweep ();
  prune ()

let () = main_program garbage_collect ()
