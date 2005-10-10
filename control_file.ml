(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util

type paragraph = (string * string) list

let trim_left s i =
  let n = String.length s in
  let rec loop i =
    if i < n && s.[i] = ' ' then loop (i + 1)
    else i
  in
  loop i

let rec trim_right s i =
  if i > 0 && s.[i - 1] = ' ' then trim_right s (i - 1)
  else i

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
    | Some line when line.[0] = ' ' ->
	(match lines with
	| last :: others ->
	    let line =
	      if line = " ." then ""
	      else String.sub line 1 (String.length line - 1)
	    in
	    loop ((last ^ "\n" ^ line) :: others)
	| [] -> failwith ("leading space: " ^ line))
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

(* Return a channel for reading a compressed file.
   We decompress it to a temporary file first,
   rather than reading from a pipe or using the CamlZip library,
   so that we detect corrupted files before partially processing them.
   This is also significantly faster than using CamlZip. *)

let decompressors =
  [ "gz", "gunzip --stdout";
    "bz2", "bunzip2 --stdout" ]

let decompress prog file =
  let tmp = Filename.temp_file "approx" "" in
  let cmd = Printf.sprintf "%s %s > %s" prog file tmp in
  unwind_protect
    (fun () ->
      if Sys.command cmd = 0 then open_in tmp
      else failwith "decompress")
    (fun () ->
      Sys.remove tmp)

let open_file file =
  match extension file with
  | Some ext -> decompress (List.assoc ext decompressors) file
  | None -> open_in file

let fold f init file = with_channel open_file file (read f init)

let iter proc = fold (fun () p -> proc p) ()

let rev_map f = fold (fun acc p -> f p :: acc) []

let map f file = List.rev (rev_map f file)

let rev_filter f = fold (fun acc p -> if f p then p :: acc else acc) []

let filter f file = List.rev (rev_filter f file)
