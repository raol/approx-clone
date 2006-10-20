(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Util

type paragraph = (string * string) list

let trim_left s i =
  let n = String.length s in
  let rec loop i =
    if i < n && (s.[i] = ' ' || s.[i] = '\t') then loop (i + 1)
    else i
  in
  loop i

let rec trim_right s i =
  let rec loop i =
    if i > 0 && (s.[i - 1] = ' ' || s.[i - 1] = '\t') then loop (i - 1)
    else i
  in
  loop i

let parse line =
  try
    let i = String.index line ':' in
    let name =
      String.lowercase (substring line ~until: (trim_right line i))
    in
    let info =
      substring line ~from: (trim_left line (i + 1))
    in
    name, info
  with _ -> failwith ("malformed line: " ^ line)

let read_paragraph chan =
  let trim s =
    substring s ~until: (trim_right s (String.length s))
  in
  let rec loop lines =
    let next =
      try Some (trim (input_line chan))
      with End_of_file -> None
    in
    match next with
    | None when lines = [] -> raise End_of_file
    | None | Some "" -> lines
    | Some line when line.[0] = ' ' || line.[0] = '\t' ->
	(match lines with
	| last :: others ->
	    let line =
	      if line = " ." then ""
	      else substring line ~from: 1
	    in
	    loop ((last ^ "\n" ^ line) :: others)
	| [] -> failwith ("leading white space: " ^ line))
    | Some line -> loop (line :: lines)
  in
  List.rev_map parse (loop [])

let read f init chan =
  let next () =
    try Some (read_paragraph chan)
    with End_of_file -> None
  in
  let rec loop x =
    match next () with
    | Some p -> loop (f x p)
    | None -> x
  in
  loop init

let fold f init file = with_channel open_file file (read f init)

let iter proc = fold (fun () p -> proc p) ()

let paragraph file =
  let once prev p =
    match prev with
    | None -> Some p
    | Some _ -> failwith (file ^ " contains more than one paragraph")
  in
  match fold once None file with
  | Some p -> p
  | None -> failwith (file ^ " contains no paragraphs")

(* Not used yet:

let rev_map f = fold (fun acc p -> f p :: acc) []

let map f file = List.rev (rev_map f file)

let rev_filter f = fold (fun acc p -> if f p then p :: acc else acc) []

let filter f file = List.rev (rev_filter f file)

*)

let get_checksum fields =
  try List.assoc "sha256" fields, file_sha256sum
  with Not_found ->
    try List.assoc "sha1" fields, file_sha1sum
    with Not_found ->
      List.assoc "md5sum" fields, file_md5sum

type info = string * Int64.t

let info_list data =
  let lines =
    match split_lines data with
    | "" :: lines -> lines
    | lines -> lines
  in
  List.map
    (fun line ->
       Scanf.sscanf line "%s %Ld %s" (fun sum size file -> (sum, size), file))
    lines

type validity =
  | Valid
  | Wrong_size of Int64.t
  | Wrong_checksum of string

let validate ?checksum (sum, size) file =
  let n = file_size file in
  if n <> size then Wrong_size n
  else
    match checksum with
    | Some file_checksum ->
	let s = file_checksum file in
	if s <> sum then Wrong_checksum s
	else Valid
    | None ->
	Valid
