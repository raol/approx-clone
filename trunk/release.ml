(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util

(* Return a list of files that do not match the size or MD5 checksum
   specified for them in a Release file *)

let release_file file =
  let path = Filename.dirname file in
  let cons_if_invalid invalid line =
    let check_file md5sum size filename =
      let file = path ^/ filename in
      if Sys.file_exists file &&
	 (file_size file <> int_of_string size || file_md5sum file <> md5sum)
      then
	file :: invalid
      else
	invalid
    in
    if line <> "" then
      Scanf.sscanf line "%s %s %s" check_file
    else
      invalid
  in
  let check_release invalid fields =
    try
      let lines = split_lines (List.assoc "md5sum" fields) in
      List.fold_left cons_if_invalid invalid lines
    with Not_found -> invalid
  in
  Control_file.fold check_release [] file

(* Assume a Release file is stale if it is more than 5 minutes older
   than the Release.gpg file *)

let signature_file file =
  let release = Filename.chop_suffix file ".gpg" in
  if Sys.file_exists release &&
    (file_modtime file -. file_modtime release > 300.) then
    [release]
  else
    []

let files_invalidated_by file =
  match Filename.basename file with
  | "Release" -> release_file file
  | "Release.gpg" -> signature_file file
  | _ -> []
