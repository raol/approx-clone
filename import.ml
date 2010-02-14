(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Import local files into the approx cache *)

open Util
open Config
open Program

let usage () =
  print "Usage: approx-import [options] file ...
Import local files into the approx cache
Options:
    -s|--simulate   scan but do not actually import any files
    -q|--quiet      do not print the file names that are imported
    -v|--verbose    print information about each file";
  exit 1

let simulate = ref false
let quiet = ref false
let verbose = ref false
let files = ref []

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-k" | "--keep" | "-s" | "--simulate" -> simulate := true
    | "-q" | "--quiet" -> quiet := true
    | "-v" | "--verbose" -> verbose := true
    | arg ->
        if arg.[0] = '-' then usage ()
        else files := arg :: !files
  done

let simulate = !simulate
let quiet = !quiet
let verbose = !verbose
let files = if !files <> [] then List.rev !files else usage ()

(* Import status of an individual file *)

type import_status =
  | Not_seen
  | Exists of string
  | Imported of string

let imported = function
  | Imported _ -> true
  | _ -> false

let string_of_import_status = function
  | Not_seen -> "not referenced by any Packages file"
  | Exists loc -> "already cached as " ^ loc
  | Imported loc -> "imported to " ^ loc

(* Information about a package that can be extracted
   from its filename, size, and md5sum *)

type package =
  { name : string;
    epoch : string;
    version : string;
    arch : string;
    size : int64;
    file : string;
    base : string;
    md5sum : string;
    mutable status : import_status }

(* Regular expression for matching package filenames *)

let file_re = Pcre.regexp "^([^_]+)_(?:(\\d+)%3a)?([^_]+)_(.+)\\.deb$"

let package_of_file file =
  let base = Filename.basename file in
  match Pcre.extract ~rex: file_re ~full_match: false base with
  | [| name; epoch; version; arch |] ->
      { name = name;
        epoch = epoch;
        version = version;
        arch = arch;
        size = file_size file;
        file = file;
        base = base;
        md5sum = file_md5sum file;
        status = Not_seen }
  | _ -> raise Not_found

let without_epoch version =
  try substring ~from: (String.index version ':' + 1) version
  with Not_found -> version

let packages = Hashtbl.create (List.length files)

let add_package pkg =
  (try
     let q = Hashtbl.find packages pkg.md5sum in
     if pkg.name <> q.name then
       print "%s: MD5 collision with %s" pkg.base q.base
   with Not_found -> ());
  Hashtbl.replace packages pkg.md5sum pkg

let scan_files () =
  let add_file name =
    if Sys.file_exists name then begin
      try add_package (package_of_file name)
      with Not_found ->
        if verbose then print "%s: ignored" (Filename.basename name)
    end else
      print "%s: not found" name
  in
  let n = List.length files in
  if n > 1 && verbose then print "[ scanning %d files ]" n;
  List.iter add_file files;
  if Hashtbl.length packages = 0 then begin
    if not quiet then print "%s" "no .deb files specified";
    exit 1
  end

let import_package pkg dst =
  let target = cache_dir ^/ dst in
  if Sys.file_exists target then
    pkg.status <- Exists dst
  else begin
    pkg.status <- Imported dst;
    if not simulate then begin
      make_directory (Filename.dirname target);
      ignore (Sys.command (Printf.sprintf "cp -p %s %s" pkg.file target))
    end
  end

let maybe_import pkg fields dist =
  let mismatch kind =
    if verbose then
      print "%s: %s mismatch (should be %s)"
        pkg.base kind (Control_file.lookup kind fields)
  in
  if not (imported pkg.status) then
    if pkg.version = without_epoch (Control_file.lookup "version" fields) then
      if pkg.arch = Control_file.lookup "architecture" fields then
        if pkg.size = Int64.of_string (Control_file.lookup "size" fields) then
          import_package pkg (dist ^/ Control_file.lookup "filename" fields)
        else mismatch "size"
      else mismatch "architecture"
    else mismatch "version"

let index_seen = ref false

let import_files index =
  if Release.is_packages_file index then
    let dist, path = split_cache_path index in
    let check_package fields =
      try
        let md5sum = Control_file.lookup "md5sum" fields in
        maybe_import (Hashtbl.find packages md5sum) fields dist
      with Not_found -> ()
    in
    index_seen := true;
    if verbose then print "[ %s/%s ]" dist path;
    Control_file.iter check_package index

let print_package { base = base; status = status } =
  if verbose || imported status then
    print "%s: %s" base (string_of_import_status status)

let print_status () =
  let pkgs = Hashtbl.fold (fun _ pkg list -> pkg :: list) packages [] in
  let cmp p q = String.compare p.base q.base in
  List.iter print_package (List.sort cmp pkgs)

let import () =
  if not simulate then drop_privileges ~user ~group;
  scan_files ();
  iter_non_dirs import_files cache_dir;
  if not !index_seen then begin
    print "%s" "There are no Packages files in the approx cache.\n\
                Please run \"apt-get update\" first.";
    exit 1
  end;
  if not quiet then print_status ()

let () = main_program import ()
