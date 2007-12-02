(* approx: proxy server for Debian archive files
   Copyright (C) 2007  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Default_config
open Log
open Control_file

let file_of_diff_index diff_index =
  Filename.chop_suffix (Filename.dirname diff_index) ".diff"

let directory file = without_extension file ^ ".diff"

let read_diff_index dir =
  let diff_index = dir ^/ "Index" in
  if not (Sys.file_exists diff_index) then
    failwith (diff_index ^ " does not exist");
  let items = read diff_index in
  let current = List.assoc "sha1-current" items ^ " current" in
  let current_info =
    match info_list current with
    | [ info, "current" ] -> info
    | _ -> failwith ("unexpected SHA1-Current entry: " ^ current)
  in
  let combine (index_info, name) (patch_info, name') =
    if name <> name' then failwith (diff_index ^ " is inconsistent");
    (index_info, dir ^/ name, patch_info)
  in
  let history = info_list (List.assoc "sha1-history" items) in
  let patches = info_list (List.assoc "sha1-patches" items) in
  List.map2 combine history patches, current_info

let rec find_tail p = function
  | x :: rest as list -> if p x then list else find_tail p rest
  | [] -> []

let find_pdiff pdiff (diffs, final) =
  let check_pdiff (index_info, _, pdiff_info) next =
    let check pdiff' =
      if is_valid file_sha1sum pdiff_info pdiff' then
	begin
	  if debug then debug_message "Parsing %s" pdiff;
	  let cmds = with_in_channel open_in pdiff' Patch.parse in
	  Some (index_info, cmds, next)
	end
      else
	begin
	  if debug then debug_message "Removing invalid %s" pdiff;
	  Sys.remove pdiff;
	  None
	end
    in
    with_decompressed pdiff check
  in
  let id = without_extension pdiff in
  match find_tail (fun (_, name, _) -> name = id) diffs with
  | cur :: (next, _, _) :: _ -> check_pdiff cur next
  | [ cur ] -> check_pdiff cur final
  | [] -> None

let compress ~src ~dst =
  let cmd = Printf.sprintf "/bin/gzip -9 --no-name --stdout %s > %s" src dst in
  if debug then debug_message "Compressing: %s" cmd;
  if Sys.command cmd <> 0 then failwith "compress"

(* Apply a pdiff to the given file *)

let apply_pdiff cmds file =
  let file' =
    with_in_channel open_in file
      (fun chan -> with_temp_file file (Patch.apply cmds chan))
  in
  Sys.rename file' file

let apply pdiff =
  let dir = Filename.dirname pdiff in
  match find_pdiff pdiff (read_diff_index dir) with
  | None -> if debug then debug_message "%s not found in DiffIndex" pdiff
  | Some (info, cmds, info') ->
      let index = Filename.chop_suffix dir ".diff" ^ ".gz" in
      let patch file =
	if is_valid file_sha1sum info file then
	  begin
	    apply_pdiff cmds file;
	    if is_valid file_sha1sum info' file then
	      begin
		if debug then debug_message "Applied %s" pdiff;
		compress ~src: file ~dst: index;
		Sys.remove pdiff
	      end
	    else (if debug then debug_message "Invalid result from %s" pdiff)
	  end
	else (if debug then debug_message "Cannot apply %s" pdiff)
      in
      if Sys.file_exists index then decompress_and_apply patch index
      else (if debug then debug_message "Index %s not found" index)

let remove_pdiffs pdiffs =
  List.iter (fun (_, file, _) -> rm (file ^ ".gz"))  pdiffs

let apply_pdiffs file pdiffs final index =
  let patch (index_info, name, pdiff_info) =
    let pdiff = name ^ ".gz" in
    let check_and_apply pdiff' =
      if is_valid file_sha1sum pdiff_info pdiff' then
	(if debug then debug_message "Parsing %s" pdiff;
	 let cmds = with_in_channel open_in pdiff' Patch.parse in
	 if is_valid file_sha1sum index_info file then apply_pdiff cmds file
	 else (if debug then debug_message "Invalid index %s" file; raise Exit))
      else (if debug then debug_message "Invalid pdiff %s" pdiff; raise Exit)
    in
    if not (Sys.file_exists pdiff) then Url.download_file pdiff;
    with_decompressed pdiff check_and_apply
  in
  try
    List.iter patch pdiffs;
    if is_valid file_sha1sum final file then
      begin
	if debug then debug_message "Updated %s" index;
	compress ~src: file ~dst: index;
	remove_pdiffs pdiffs
      end
    else (if debug then debug_message "Invalid update of %s" index)
  with Exit -> ()

let update index =
  if not (Filename.check_suffix index ".gz") then invalid_arg "Pdiff.update";
  let diffs, final = read_diff_index (directory index) in
  let update_index file =
    let info = (file_sha1sum file, file_size file) in
    if info = final then (if debug then debug_message "%s is current" index)
    else
      match find_tail (fun (i, _, _) -> i = info) diffs with
      | [] ->
	  if debug then debug_message "%s not found in DiffIndex" index;
	  raise Not_found
      | list -> apply_pdiffs file list final index
  in
  decompress_and_apply update_index index
