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
    -f|--fast     do not validate MD5 checksums
    -k|--keep     do not remove files
    -q|--quiet    do not print file names
    -v|--verbose  print reason for removal";
  exit 1

let no_checksum = ref false
let keep = ref false
let quiet = ref false
let verbose = ref false

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--fast" -> no_checksum := true
    | "-k" | "--keep" -> keep := true
    | "-q" | "--quiet" -> quiet := true
    | "-v" | "--verbose" -> verbose := true
    | _ -> usage ()
  done

let no_checksum = !no_checksum
let keep = !keep
let quiet = !quiet
let verbose = !verbose

let roots = ref []

let find_roots () =
  let find file =
    match Filename.basename file with
    | "Packages" | "Packages.gz" | "Packages.bz2" -> roots := file :: !roots
(*XXX* add Sources to roots and trace them *) 
    | _ -> ()
  in
  Config.iter (fun dir _ -> treewalk find (cache_dir ^/ dir))

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

(* Check whether a file is a Release, Packages, or Sources index file *)

let is_index_file file =
  match Filename.basename file with
  | "Release" | "Release.gpg"
  | "Packages" | "Packages.gz" | "Packages.bz2"
  | "Sources" | "Sources.gz" | "Sources.bz2" -> true
  | _ -> false

(* Check if a file is part of a known distribution *)

let known_dist file =
  let dist, _ = Cache.split_pathname file in
  try ignore (Config.get dist); true
  with Not_found -> false

(* Determine whether a file is a candidate for garbage collection *)

let is_candidate file =
  not (is_index_file file && known_dist file)

let record_files () =
  treewalk (fun f -> if is_candidate f then set_status f Unmarked) cache_dir

(* Handle the case of filename fields of the form ./path  *)

let canonical path =
  if String.length path >= 2 && path.[0] = '.' && path.[1] = '/' then
    substring path ~from: 2
  else
    path

(* We mark a file as live if its size and MD5 checksum
   match those specified in a Packages file *)  (*XXX or Sources file*)

let mark_file prefix fields =
  let file = canonical (List.assoc "filename" fields) in
  let size = Int64.of_string (List.assoc "size" fields) in
  let md5sum = List.assoc "md5sum" fields in
  let path = prefix ^/ file in
  try
    if get_status path = Unmarked then
      if file_size path = size then
	if no_checksum || file_md5sum path = md5sum then
	  set_status path Valid
	else
	  set_status path Wrong_checksum
      else
	set_status path Wrong_size
  with
    Not_found -> ()

let mark_package package =
  if verbose then (print_string "# "; print_endline package);
  let dist, file = Cache.split_pathname package in
  let prefix = cache_dir ^/ dist in
  try Control_file.iter (mark_file prefix) package
  with Failure "decompress" ->
    (* corrupt Packages file: download it and try again *)
    Cache.download package;
    Control_file.iter (mark_file prefix) package

let mark () =
  find_roots ();
  record_files ();
  List.iter mark_package !roots

let print_status status =
  let code =
    match status with
    | Unmarked -> ' '
    | Wrong_size -> '='
    | Wrong_checksum -> '!'
    | Valid -> assert false
  in
  print_char code;
  print_char ' '

let message file status =
  if verbose then print_status status;
  print_endline file

let inactive file =
  Unix.time () -. file_modtime file > 300.  (* 5 minutes *)

let sweep () =
  let gc file status =
    if status <> Valid then
      if inactive file then
	begin
	  if not quiet then message file status;
	  if not keep then Sys.remove file
	end
      else
	Printf.eprintf "%s is not old enough to remove\n%!" file
  in
  iter_status gc

let garbage_collect () =
  mark ();
  sweep ()

let () = garbage_collect ()
