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
    if Sys.file_exists name then
      try
        let pkg = package_of_file name in
        Hashtbl.replace packages pkg.name pkg
      with Not_found ->
        print_if verbose "%s: ignored" (Filename.basename name)
    else
      print_if true "%s: not found" name
  in
  List.iter add_file files;
  if Hashtbl.length packages = 0 then exit 1

let index_seen = ref false
let current_index = ref None

let import_package pkg dst =
  (match !current_index with
   | Some (dist, path) ->
       print_if verbose "# %s/%s" dist path;
       current_index := None
   | None -> ());
  let target = cache_dir ^/ dst in
  if Sys.file_exists target then begin
    print_if verbose "%s: present as %s" pkg.base target;
    Hashtbl.remove packages pkg.name  (* don't process it again *)
  end else begin
    print_if (simulate || not quiet) "%s: copying to %s" pkg.base target;
    if not simulate then begin
      make_directory (Filename.dirname target);
      ignore (Sys.command (Printf.sprintf "cp -p %s %s" pkg.file target))
    end;
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
    index_seen := true;
    current_index := Some (dist, path);
    Control_file.iter check_package index

let import () =
  (* import must run as the approx user even in simulate mode,
     because index files are decompressed in the cache *)
  drop_privileges ~user ~group;
  scan_files ();
  iter_non_dirs import_files cache_dir;
  print_if (not !index_seen) "%s"
    "There are no Packages files in the approx cache.\n\
     Please run \"apt-get update\" first."

let () = main_program import ()
