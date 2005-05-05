(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Garbage-collect the approx cache using a mark-sweep algorithm.
   Any file in the cache whose name, size, and checksum match an entry
   in a Packages file is assumed to be valid, and kept.
   Anything else (other than Packages, Release, and Source files)
   is assumed to be invalid, and removed. *)

open Util
open Default_config
open Printf

let usage () =
  prerr_endline "Usage: gc_approx [options]";
  prerr_endline "Garbage-collect the approx cache";
  prerr_endline "Options:";
  prerr_endline "    -f|--fast     do not validate MD5 checksums";
  prerr_endline "    -k|--keep     do not remove files";
  prerr_endline "    -q|--quiet    do not print file names";
  prerr_endline "    -v|--verbose  print reason for removal";
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

(* Recursively descend the filesystem tree starting at [path],
   applying [proc] to each leaf (non-directory). *)

let rec treewalk proc path =
  let visit name =
    let path = path ^/ name in
    if (Unix.stat path).Unix.st_kind = Unix.S_DIR then
      treewalk proc path
    else
      proc path
  in
  Array.iter visit (try Sys.readdir path with Sys_error _ -> [||])

let roots = ref []

let find_roots () =
  let find file =
    match Filename.basename file with
    | "Packages" | "Packages.gz" -> roots := file :: !roots
    | _ -> ()
  in
  Config.iter (fun dir _ -> treewalk find (cache_dir ^/ dir))

(* Extract the distribution and relative filename
   from the absolute pathname of a file in the cache.
   Example:
       dist_prefix "/var/cache/approx/debian/pool/main/..."
   returns
       ("debian", "pool/main/...") *)

let split_cache_pathname path =
   split_path (substring path ~from: (String.length cache_dir))

(* The cache is probably only a small subset of all the files in
   the Debian archive, so  we start with a table of filenames
   actually present in this cache, and mark them as we process
   the Packages files. *)

type file_status =
  | Unmarked
  | Valid
  | Wrong_size
  | Wrong_checksum

let files = Hashtbl.create 4096
let get_status = Hashtbl.find files
let set_status = Hashtbl.replace files
let iter_status proc = Hashtbl.iter proc files

(* Determine whether a file is a candidate for garbage collection. *)

let is_candidate file =
  match Filename.basename file with
    | "Packages" | "Packages.gz"
    | "Release" | "Release.gz"
    | "Sources" | "Sources.gz" ->
	let dist, _ = split_cache_pathname file in
	(try ignore (Config.get dist); false
	 with Not_found -> true (* not part of a known distribution *))
    | _ ->
	true

let record_files () =
  let record file =
    if is_candidate file then set_status file Unmarked
  in
  treewalk record cache_dir

(* Handle the case of filename fields of the form ./path  *)

let canonical path =
  if String.length path >= 2 && path.[0] = '.' && path.[1] = '/' then
    substring path ~from: 2
  else
    path

(* We mark a file as live if its size and MD5 checksum
   match those specified in a Packages file. *)

let mark_file prefix fields =
  let file = canonical (List.assoc "filename" fields) in
  let size = int_of_string (List.assoc "size" fields) in
  let md5sum = List.assoc "md5sum" fields in
  let path = prefix ^/ file in
  let check_size () =
    (Unix.stat path).Unix.st_size = size
  in
  let check_md5sum () =
    !no_checksum || Digest.to_hex (Digest.file path) = md5sum
  in
  try
    if get_status path = Unmarked then
      if check_size () then
	if check_md5sum () then
	  set_status path Valid
	else
	  set_status path Wrong_checksum
      else
	set_status path Wrong_size
  with
    Not_found -> ()

let download dist file package =
  let url = Config.get dist ^/ file in
  let package' = package ^ ".tmp" in
  let cmd = sprintf "/usr/bin/wget -q -O %s %s" package' url in
  prerr_string "downloading "; prerr_endline url;
  if Sys.command cmd = 0 then
    Sys.rename package' package
  else
    failwith ("cannot download " ^ url)

let mark_package package =
  if !verbose then (print_string "# "; print_endline package);
  let dist, file = split_cache_pathname package in
  let prefix = cache_dir ^/ dist in
  try
    Package.iter (mark_file prefix) package
  with Failure "decompress" ->
    (* corrupt Packages.gz file: download it and try again *)
    download dist file package;
    Package.iter (mark_file prefix) package

let mark () =
  find_roots ();
  record_files ();
  List.iter mark_package !roots

let message file status =
  if !verbose then
    let code =
      match status with
      | Unmarked -> ' '
      | Wrong_size -> '='
      | Wrong_checksum -> '!'
      | Valid -> assert false
    in
    print_char code;
    print_char ' '
  else
    ();
  print_endline file

(* Remove a file along with any parent directories that have been emptied. *)

let remove file =
  Sys.remove file;
  let dir = Filename.dirname file in
  (* rmdir complains about non-directory (i.e. symlink) parents,
     so we redirect its output to /dev/null *)
  let cmd = sprintf "/bin/rmdir -p %s >/dev/null 2>&1" dir in
  ignore (Sys.command cmd)

let sweep () =
  let gc file status =
    if status <> Valid then
      begin
	if not !quiet then message file status;
	if not !keep then remove file
      end
  in
  iter_status gc

let garbage_collect () =
  mark ();
  sweep ()

let () = garbage_collect ()
