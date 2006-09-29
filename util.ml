(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
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

let relative_path path =
  let n = String.length path in
  let rec loop i =
    if i = n then "."
    else if path.[i] <> '/' then String.sub path i (n - i)
    else loop (i + 1)
  in
  loop 0

let relative_url path =
  try
    if path.[0] = '/' then relative_path path
    else
      let i = String.index path ':' in
      if path.[i + 1] = '/' && path.[i + 2] = '/' && path.[i + 3] <> '/' then
	let j = String.index_from path (i + 3) '/' in
	relative_path (substring path ~from: j)
      else
	raise Exit
  with _ ->
    failwith ("malformed URL: " ^ path)

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

(* Return a channel for reading a possibly compressed file.
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

let is_directory file =
  try (stat file).st_kind = S_DIR
  with Unix_error (ENOENT, "stat", _) -> false

let rec fold_dirs f init path =
  let visit acc name =
    fold_dirs f acc (path ^/ name)
  in
  if is_directory path then
    Array.fold_left visit (f init path) (Sys.readdir path)
  else
    init

let iter_dirs proc = fold_dirs (fun () -> proc) ()

let rec fold_non_dirs f init path =
  let visit acc name =
    fold_non_dirs f acc (path ^/ name)
  in
  if is_directory path then
    Array.fold_left visit init (Sys.readdir path)
  else if Sys.file_exists path then
    f init path
  else
    init

let iter_non_dirs proc = fold_non_dirs (fun () -> proc) ()

let file_modtime file = (stat file).st_mtime

let file_size file = (LargeFile.stat file).LargeFile.st_size

module type MD =
  sig
    type t
    val file : string -> t
    val to_hex : t -> string
  end

module FileDigest (MsgDigest : MD) =
  struct
    let sum file = MsgDigest.to_hex (MsgDigest.file file)
  end

let file_md5sum = let module F = FileDigest(Digest) in F.sum
let file_sha1sum = let module F = FileDigest(Sha1) in F.sum
let file_sha256sum = let module F = FileDigest(Sha256) in F.sum

let drop_privileges name =
  setgid (getgrnam name).gr_gid;
  setuid (getpwnam name).pw_uid

let packages_variants = [ "Packages"; "Packages.gz"; "Packages.bz2" ]

let sources_variants = [ "Sources"; "Sources.gz"; "Sources.bz2" ]

let is_sources file =
  List.mem (Filename.basename file) sources_variants

let latest_index file =
  let check variants =
    let files = List.map ((^/) (Filename.dirname file)) variants in
    let latest (name, modtime as orig) f =
      try
	let t = file_modtime f in
	if t > modtime then (f, t)
	else orig
      with _ -> orig  (* variant f does not exist *)
    in
    if List.mem file files then
      try Some (List.fold_left latest (file, file_modtime file) files)
      with _ -> None
    else
      raise Not_found
  in
  try check packages_variants
  with Not_found ->
    try check sources_variants
    with Not_found ->
      None

let string_of_exception exc =
  match exc with
  | Failure str -> String.capitalize str
  | Invalid_argument str -> "Invalid argument: " ^ str
  | Sys_error str -> str
  | Unix.Unix_error (err, str, arg) ->
      let msg = Printf.sprintf "%s: %s" str (Unix.error_message err) in
      if arg <> "" then Printf.sprintf "%s (%s)" msg arg
      else msg
  | e -> Printexc.to_string e

let main_program f x =
  try f x with e -> prerr_endline (string_of_exception e); exit 1
