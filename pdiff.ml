(* approx: proxy server for Debian archive files
   Copyright (C) 2013  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config
open Log
open Util

let index_file path =
  Filename.chop_suffix (Filename.dirname path) ".diff" ^ ".gz"

let read_diff_index dir =
  let diff_index = dir ^/ "Index" in
  if not (Sys.file_exists diff_index) then
    failwith (diff_index ^ " does not exist");
  let items = Control_file.read diff_index in
  let current = Control_file.lookup "sha1-current" items ^ " current" in
  let current_info =
    match Control_file.info_list current with
    | [info, "current"] -> info
    | _ -> failwith ("unexpected SHA1-Current entry: " ^ current)
  in
  let combine (index_info, name) (patch_info, name') =
    if name <> name' then failwith (diff_index ^ " is inconsistent");
    (index_info, dir ^/ name, patch_info)
  in
  let history = Control_file.lookup_info "sha1-history" items in
  let patches = Control_file.lookup_info "sha1-patches" items in
  List.map2 combine history patches, current_info

let rec find_tail p = function
  | x :: rest as list -> if p x then list else find_tail p rest
  | [] -> []

(* Pdiff application must result in a Packages or Sources file
   that is identical to the one in the official archive,
   so this function must use the same gzip parameters that dak does.
   See http://ftp-master.debian.org/git/dak.git *)

let compress ~src ~dst =
  let cmd = Printf.sprintf "/bin/gzip -9cn --rsyncable < %s > %s" src dst in
  debug_message "Compressing: %s" cmd;
  if Sys.command cmd <> 0 then failwith "compress";
  if debug && not (Release.valid dst) then
    debug_message "Compressed file %s is invalid" dst

(* Apply a parsed pdiff to the given file *)

let apply_patch cmds file =
  let file' =
    with_in_channel open_in file
      (fun chan -> with_temp_file file (Patch.apply cmds chan))
  in
  Sys.rename file' file

let valid_file info = Control_file.valid file_sha1sum info

let apply_pdiffs file pdiffs final index =
  let patch (index_info, name, pdiff_info) =
    let pdiff = name ^ ".gz" in
    let valid_index = valid_file index_info in
    let valid_pdiff = valid_file pdiff_info in
    let check_and_apply pdiff' =
      if valid_pdiff pdiff' then begin
        debug_message "Applying %s" pdiff;
        let cmds = with_in_channel open_in pdiff' Patch.parse in
        if valid_index file then apply_patch cmds file
        else (debug_message "Invalid index %s" file; raise Exit)
      end else (debug_message "Invalid pdiff %s" pdiff; raise Exit)
    in
    if not (Sys.file_exists pdiff) then Url.download_file pdiff;
    with_decompressed pdiff check_and_apply
  in
  try
    List.iter patch pdiffs;
    if valid_file final file then begin
      info_message "Updated %s" index;
      compress ~src: file ~dst: index
    end else error_message "Invalid update of %s" index
  with Exit -> ()

let update index =
  info_message "Updating %s" index;
  if not (Filename.check_suffix index ".gz") then
    invalid_string_arg "Pdiff.update" index;
  if not (Sys.file_exists index) then
    Url.download_file index;
  let dir = Filename.chop_suffix index ".gz" ^ ".diff" in
  let diffs, final = read_diff_index dir in
  let update_index file =
    let info = (file_sha1sum file, file_size file) in
    if info = final then debug_message "%s is current" index
    else
      match find_tail (fun (i, _, _) -> i = info) diffs with
      | [] -> failwith (index ^ " not found in DiffIndex")
      | list -> apply_pdiffs file list final index
  in
  decompress_and_apply update_index index
