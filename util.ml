(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Unix

let substring ?from ?until str =
  let aux ?(from=0) ?(until=String.length str) () =
    if from = 0 && until = String.length str then str
    else String.sub str from (until - from)
  in
  aux ?from ?until ()

let split sep str =
  let next i =
    try Some (String.index_from str i sep)
    with Not_found -> None
  in
  let rec loop acc i =
    match next i with
    | Some j -> loop (substring str ~from: i ~until: j :: acc) (j + 1)
    | None -> substring str ~from: i :: acc
  in
  List.rev (loop [] 0)

let split_lines = split '\n'

let explode_path = split '/'

let implode_path = String.concat "/"

let quoted_string str = "\"" ^ String.escaped str ^ "\""

let (^/) = Filename.concat

let relative_path name =
  let n = String.length name in
  let rec loop i =
    if i = n then "."
    else if name.[i] <> '/' then String.sub name i (n - i)
    else loop (i + 1)
  in
  loop 0

let extension file =
  let base = Filename.basename file in
  try Some (substring base ~from: (String.rindex base '.' + 1))
  with Not_found -> None

let unwind_protect body post =
  try let result = body () in post (); result
  with e -> post (); raise e

let with_channel openf x f =
  let chan = openf x in
  unwind_protect
    (fun () -> f chan)
    (fun () -> close_in chan)

let rec treewalk proc path =
  let visit name =
    let path = path ^/ name in
    if (stat path).st_kind = S_DIR then
      treewalk proc path
    else
      proc path
  in
  Array.iter visit (try Sys.readdir path with Sys_error _ -> [||])

let file_modtime file = (stat file).st_mtime

let file_size file = (LargeFile.stat file).LargeFile.st_size

let file_md5sum file = Digest.to_hex (Digest.file file)
