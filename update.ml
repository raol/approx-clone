(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Update the Packages and Sources files in the approx cache *)

open Util
open Config
open Log

let usage () =
  prerr_endline "Usage: approx-update [options] [files]
Update the approx cache
Options:
    -k|--keep|-s|--simulate
                    do not modify or download any files
    -q|--quiet      do not print information about updates and removals
    -v|--verbose    print the status of each Packages or Sources file";
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
let files = List.rev !files

let print fmt = print_if true fmt

let remove_pdiffs dir =
  match Filename.basename dir with
  | "Packages.diff" | "Sources.diff" ->
      print_if (not quiet) "Removing %s" dir;
      if not simulate then begin
        iter_non_dirs (perform Sys.remove) dir;
        perform Unix.rmdir dir
      end
  | _ -> invalid_arg (dir ^ " is not a pdiff directory")

let update_valid file =
  print_if verbose "%s: valid" file;
  let dir = Pdiff.directory file in
  if directory_exists dir then remove_pdiffs dir

let update_invalid file =
  print_if verbose "%s: invalid" file;
  let diff_index = Pdiff.directory file ^/ "Index" in
  if Sys.file_exists diff_index then begin
    print_if (not quiet) "Applying pdiffs to %s" file;
    if not simulate then Pdiff.update file
  end

let update_file file =
  if not (Sys.file_exists file) then
    print "%s does not exist" file
  else if not (Release.is_index file) then
    print_if (files <> []) "%s is not a Packages or Sources file" file
  else if extension file <> ".gz" then
    print_if (files <> []) "%s is not gzip (.gz) file" file
  else
    try
      if Release.valid_file file then update_valid file
      else begin
        update_invalid file;
        (* should now be valid *)
        if Release.valid_file file then update_valid file
      end
    with
    | Not_found -> print_if (not quiet) "Cannot find Release file for %s" file
    | e -> print "%s: %s" file (string_of_exception e)

let update_cache () =
  if not simulate then drop_privileges ~user ~group;
  if files <> [] then List.iter update_file files
  else iter_non_dirs update_file cache_dir

let () = main_program update_cache ()
