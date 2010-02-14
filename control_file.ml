(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util
open Config
open Log

type paragraph = { file : string; line : int; fields : (string * string) list }

exception Missing of paragraph * string

let defined name par = List.mem_assoc name par.fields

let lookup name par =
  try List.assoc name par.fields
  with Not_found -> raise (Missing (par, name))

let file_name par = par.file

let line_number par = par.line

let iter_fields proc par = List.iter proc par.fields

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

let read_paragraph file n chan =
  let trim s =
    substring s ~until: (trim_right s (String.length s))
  in
  let rec loop lines i j =
    let next =
      try Some (trim (input_line chan))
      with End_of_file -> None
    in
    match next with
    | None ->
        if lines <> [] then lines, i, j + 1
        else raise End_of_file
    | Some "" ->
        if lines <> [] then lines, i, j + 1
        else loop [] (i + 1) (j + 1)
    | Some line ->
        if line.[0] = ' ' || line.[0] = '\t' then
          match lines with
          | last :: others ->
              let line =
                if line = " ." then ""
                else substring line ~from: 1
              in
              loop ((last ^ "\n" ^ line) :: others) i (j + 1)
          | [] -> failwith ("leading white space: " ^ line)
        else
          loop (line :: lines) i (j + 1)
  in
  let fields, i, j = loop [] n n in
  { file = file; line = i; fields = List.rev_map parse fields }, j

let fold f init file =
  let read_file chan =
    let next n =
      try Some (read_paragraph file n chan)
      with End_of_file -> None
    in
    let rec loop x n =
      match next n with
      | Some (p, n') -> loop (f x p) n'
      | None -> x
    in
    loop init 1
  in
  with_in_channel open_file file read_file

let iter = iter_of_fold fold

let read file =
  let once prev p =
    match prev with
    | None -> Some p
    | Some _ -> failwith (file ^ " contains more than one paragraph")
  in
  match fold once None file with
  | Some p -> p
  | None -> failwith (file ^ " contains no paragraphs")

(* Not used yet:

(* A more efficient alternative to map that builds the result list
   in reverse order *)

let rev_map f = fold (fun acc p -> f p :: acc) []

(* Map a function over each paragraph in a Debian control file *)

let map f file = List.rev (rev_map f file)

(* A more efficient alternative to filter that builds the result list
   in reverse order *)

val rev_filter : (paragraph -> bool) -> string -> paragraph list

let rev_filter f = fold (fun acc p -> if f p then p :: acc else acc) []

(* Return a list of paragraphs satisfying a predicate *)

let filter f file = List.rev (rev_filter f file)

*)

let get_checksum par =
  if defined "sha256" par then
    lookup "sha256" par, file_sha256sum
  else if defined "sha1" par then
    lookup "sha1" par, file_sha1sum
  else
    lookup "md5sum" par, file_md5sum

type info = string * int64

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

let read_checksum_info file =
  let lines, checksum = get_checksum (read file) in
  info_list lines, checksum

let lookup_info field par = info_list (lookup field par)

type validity =
  | Valid
  | Wrong_size of int64
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
    | None -> Valid

let is_valid checksum ((s, n) as info) file =
  match validate ~checksum info file with
  | Valid -> true
  | Wrong_size n' ->
      debug_message "%s: size %Ld should be %Ld" (shorten file) n' n;
      false
  | Wrong_checksum s' ->
      debug_message "%s: checksum %s should be %s" (shorten file) s' s;
      false
