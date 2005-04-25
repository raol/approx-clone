(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

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

let read_paragraph chan =
  let rec loop lines =
    let line = input_line chan in
    if line = "" then lines
    else if line.[0] = ' ' then
      (* line with leading space should be concatenated with previous line
	 but we just ignore it here *)
      loop lines
    else
      loop (parse line :: lines)
  in
  loop []  (* reverse order doesn't matter *)

let read proc chan =
  let next () =
    try Some (read_paragraph chan)
    with End_of_file -> None
  in
  let rec loop () =
    match next () with
    | Some p -> proc p; loop ()
    | None -> ()
  in
  loop ()

(* Return a temporary file name.
   Assumes the caller is single-threaded. *)

let tmp_file () =
  Printf.sprintf "/tmp/gc_approx.%d" (Unix.getpid ())

(* Return a channel for reading a compressed file.
   To detect corrupted .gz files, we first decompress it
   to a temporary file. *)

let decompress file =
  let tmp = tmp_file () in
  let cmd = Printf.sprintf "/bin/gunzip --stdout %s > %s" file tmp in
  if Sys.command cmd <> 0 then
    begin
      Sys.remove tmp;
      failwith "decompress"
    end;
  let chan = open_in tmp in
  Sys.remove tmp;
  chan

let with_open_file file proc =
  let chan =
    if Filename.check_suffix file ".gz" then
      decompress file
    else
      open_in file
  in
  try
    proc chan;
    close_in chan
  with e ->
    close_in chan;
    raise e

let iter proc file =
  with_open_file file (read proc)
