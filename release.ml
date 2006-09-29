(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util

(* Return a list of files that do not match the size or checksum
   specified for them in a Release file *)

let release_file file =
  let path = Filename.dirname file in
  let cons_if_invalid checksum invalid_files (info, filename) =
    let file = path ^/ filename in
    if Sys.file_exists file then
      match Control_file.validate ~checksum info file with
      | Control_file.Valid -> invalid_files
      | _ -> file :: invalid_files
    else
      invalid_files
  in
  try
    let fields = Control_file.paragraph file in
    let lines, sum = Control_file.get_checksum fields in
    let info = Control_file.info_list lines in
    List.fold_left (cons_if_invalid sum) [] info
  with Not_found -> []

(* Assume a Release file is stale if it is more than 5 minutes older
   than the Release.gpg file *)

let signature_file file =
  let release = Filename.chop_suffix file ".gpg" in
  if Sys.file_exists release &&
    (file_modtime file -. file_modtime release > 300.) then [release]
  else []

let files_invalidated_by file =
  match Filename.basename file with
  | "Release" -> release_file file
  | "Release.gpg" -> signature_file file
  | _ -> []
