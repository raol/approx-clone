(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Import local files into the approx cache *)

open Util
open Config
open Log

let usage () =
  prerr_endline "Usage: approx-import [options] file ...
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

(* Information about a package that can be extracted from its filename *)

type package =
  { name : string;
    version : string;
    arch : string;
    size : int64;
    file : string;
    base : string }

(* Regular expression for matching package filenames *)

let file_re = Pcre.regexp "^([^_]+)_([^_]+)_(.+)\\.deb$"

let package_of_file file =
  let base = Filename.basename file in
  match Pcre.extract ~rex: file_re ~full_match: false base with
  | [| name; version; arch |] ->
      { name = name;
        version = Pcre.qreplace ~pat: "%3a" ~templ: ":" version;
        arch = arch;
        size = file_size file;
        file = file;
        base = base }
  | _ -> raise Not_found

let packages = Hashtbl.create (List.length files)

let scan_files () =
  let add_file name =
    try
      let pkg = package_of_file name in
      Hashtbl.replace packages pkg.name pkg
    with Not_found ->
      print_if verbose "%s: ignored\n" (Filename.basename name)
  in
  List.iter add_file files

let print fmt = print_if true fmt

let import_package pkg dst =
  let target = cache_dir ^/ dst in
  if Sys.file_exists target then begin
    print_if verbose "%s: present as %s" pkg.base target;
    Hashtbl.remove packages pkg.name  (* don't process it again *)
  end else begin
    if not simulate then begin
      let cmd =
        Printf.sprintf "cp -p %s %s %s"
          (if quiet then "" else "-v") pkg.file target
      in
      ignore (Sys.command cmd)
    end;
    print_if verbose "%s: copied to %s" pkg.base target
  end

let import_files index =
  if Release.is_packages_file index then
    let dist, path = Url.split_cache_path index in
    let check_package fields =
      try
        let name = List.assoc "package" fields in
        let pkg = Hashtbl.find packages name in
        if pkg.version = List.assoc "version" fields &&
          pkg.arch = List.assoc "architecture" fields &&
          pkg.size = Int64.of_string (List.assoc "size" fields) then
            import_package pkg (dist ^/ List.assoc "filename" fields)
      with Not_found -> ()
    in
    Control_file.iter check_package index

let import () =
  if not simulate then drop_privileges ~user ~group;
  scan_files ();
  iter_non_dirs import_files cache_dir

let () = main_program import ()
