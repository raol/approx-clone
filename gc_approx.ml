(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config

let usage () =
  prerr_endline "Usage: gc_approx [options]";
  prerr_endline "Garbage-collect the approx cache.";
  prerr_endline "Options:";
  prerr_endline "    -f|--fast     do not validate MD5 checksums";
  prerr_endline "    -k|--keep     do not remove files";
  prerr_endline "    -q|--quiet    do not print file names";
  exit 1

let checksum = ref true
let verbose = ref true
let remove = ref true

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-f" | "--fast" -> checksum := false
    | "-k" | "--keep" -> remove := false
    | "-q" | "--quiet" -> verbose := false
    | _ -> usage ()
  done

let rec treewalk proc path =
  let visit name =
    let path = path ^/ name in
    if (Unix.stat path).Unix.st_kind = Unix.S_DIR then
      treewalk proc path
    else
      proc path
  in
  Array.iter visit (try Sys.readdir path with Sys_error _ -> [||])

let packages = ref []

let find_roots () =
  let find file =
    match Filename.basename file with
    | "Packages" | "Packages.gz" -> packages := file :: !packages
    | _ -> ()
  in
  Config.iter (fun dir _ -> treewalk find (cache ^/ dir))

let files = Hashtbl.create 4096

let record_files () =
  let record file =
    match Filename.basename file with
    | "Packages" | "Packages.gz"
    | "Release" | "Release.gz"
    | "Sources" | "Sources.gz" ->
	() (* never consider these for garbage collection *)
    | _ ->
	Hashtbl.replace files file false
  in
  treewalk record cache

let dist_prefix path =
  let n = String.length cache in
  try substring path ~until: (String.index_from path (n+1) '/')
  with Not_found -> failwith ("unexpected pathname: " ^ path)

let canonical path =
  if String.length path >= 2 && path.[0] = '.' && path.[1] = '/' then
    substring path ~from: 2
  else
    path

let mark_file prefix fields =
  let file = canonical (List.assoc "filename" fields) in
  let size = int_of_string (List.assoc "size" fields) in
  let md5sum = List.assoc "md5sum" fields in
  let path = prefix ^/ file in
  let check_size () =
    (Unix.stat path).Unix.st_size = size
  in
  let check_md5sum () =
    not !checksum || Digest.to_hex (Digest.file path) = md5sum
  in
  try
    if check_size () && check_md5sum () then
      Hashtbl.replace files path true
  with
    Unix.Unix_error (Unix.ENOENT, "stat", _) -> ()

let mark_package package =
  Package.iter (mark_file (dist_prefix package)) package

let mark () =
  find_roots ();
  record_files ();
  List.iter mark_package !packages

let sweep () =
  let garbage_collect file marked =
    if not marked then
      begin
	if !verbose then print_endline file;
	if !remove then Sys.remove file
      end
  in
  Hashtbl.iter garbage_collect files

let () =
  mark ();
  sweep ()
