(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

(* Update the Packages and Sources files in the approx cache *)

open Util
open Config

let usage () =
  print "Usage: approx-update [options]
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

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "-k" | "--keep" | "-s" | "--simulate" -> simulate := true
    | "-q" | "--quiet" -> quiet := true
    | "-v" | "--verbose" -> verbose := true
    | _ -> usage ()
  done

let simulate = !simulate
let quiet = !quiet
let verbose = !verbose

let remove_pdiffs dir =
  match Filename.basename dir with
  | "Packages.diff" | "Sources.diff" ->
      if not quiet then print "[ removing %s ]" (shorten dir);
      if not simulate then begin
        iter_non_dirs (perform Sys.remove) dir;
        perform Unix.rmdir dir
      end
  | _ -> invalid_arg (shorten dir ^ " is not a pdiff directory")

let update_valid file =
  if verbose then print "%s: valid" (shorten file);
  let dir = Pdiff.directory file in
  if directory_exists dir then remove_pdiffs dir

let update_invalid file =
  if verbose then print "%s: invalid" (shorten file);
  let diff_index = Pdiff.directory file ^/ "Index" in
  if Sys.file_exists diff_index then begin
    if not quiet then print "[ applying pdiffs to %s ]" (shorten file);
    if not simulate then Pdiff.update file
  end

let update_file file =
  if Release.is_index file && extension file = ".gz" then
    try
      if Release.valid_file file then update_valid file
      else begin
        update_invalid file;
        (* should now be valid *)
        if Release.valid_file file then update_valid file
      end
    with
    | Not_found ->
        if not quiet then print "%s: cannot find Release file" (shorten file)
    | e ->
        print "%s: %s" (shorten file) (string_of_exception e)

let update_cache () =
  if not simulate then drop_privileges ~user ~group;
  iter_non_dirs update_file cache_dir

let () = main_program update_cache ()
