(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

let uncompress file =
  let command = Printf.sprintf "/bin/gzip --decompress --stdout %s" file in
  Unix.open_process_in command

let with_package_file file proc =
  let input =
    if Filename.check_suffix file ".gz" then
      uncompress file
    else
      open_in file
  in
  try
    proc input;
    close_in input
  with e ->
    close_in input;
    raise e

let parse line =
  let rec lskip i =
    if line.[i] <> ' ' then i else lskip (i + 1)
  in
  let rec rskip i =
    if line.[i] <> ' ' then i else rskip (i - 1)
  in
  let i = String.index line ':' in
  let name = String.lowercase (String.sub line 0 i) in
  let i = lskip (i + 1) in
  let j = rskip (String.length line - 1) in
  let info = String.sub line i (j - i + 1) in
  name, info

let read_paragraph input =
  let rec loop lines =
    let line = input_line input in
    if line = "" then lines
    else if line.[0] = ' ' then
      (* line with leading space should be concatenated with previous line
	 but we just ignore it here *)
      loop lines
    else
      loop (parse line :: lines)
  in
  loop []  (* reverse order doesn't matter *)

let read proc input =
  let next () =
    try Some (read_paragraph input)
    with End_of_file -> None
  in
  let rec loop () =
    match next () with
    | Some p -> proc p; loop ()
    | None -> ()
  in
  loop ()

let iter proc file =
  with_package_file file (read proc)
