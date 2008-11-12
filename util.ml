(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Printf
open Unix
open Unix.LargeFile

let is_prefix pre str =
  let prefix_len = String.length pre and string_len = String.length str in
  let rec loop i =
    if i = prefix_len then true
    else if i = string_len || pre.[i] <> str.[i] then false
    else loop (i + 1)
  in
  loop 0

let substring ?(from=0) ?until str =
  let n = String.length str in
  let until = match until with Some i -> i | None -> n in
  if from = 0 && until = n then str
  else String.sub str from (until - from)

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

let join sep list =
  String.concat (String.make 1 sep) list

let split_lines = split '\n'

let explode_path = split '/'

let implode_path = join '/'

let (^/) = Filename.concat

let make_directory path =
  (* Create a directory component in the path.  Since it might be
     created concurrently, we have to ignore the Unix EEXIST error --
     simply testing for existence first introduces a race condition. *)
  let make_dir name =
    try mkdir name 0o755
    with Unix_error (EEXIST, _, _) ->
      if not (Sys.is_directory name) then
        failwith ("file " ^ name ^ " is not a directory")
  in
  let rec loop cwd = function
    | dir :: rest ->
        let name = cwd ^/ dir in
        make_dir name;
        loop name rest
    | [] -> ()
  in
  match explode_path path with
  | "" :: dirs -> loop "/" dirs
  | dirs -> loop "." dirs

let quoted_string = sprintf "%S"

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
      if path.[i + 1] = '/' && path.[i + 2] = '/' then
        let j = String.index_from path (i + 3) '/' in
        relative_path (substring path ~from: j)
      else
        raise Exit
  with _ ->
    failwith ("malformed URL: " ^ path)

let split_extension file =
  let base = Filename.basename file in
  (* look for '.' in basename only, not parent directories *)
  try
    let i = String.rindex base '.' in
    (Filename.dirname file ^/ substring ~until: i base, substring ~from: i base)
  with Not_found -> (file, "")

let without_extension file = fst (split_extension file)

let extension file = snd (split_extension file)

let the = function Some x -> x | None -> raise Not_found

(* private exception to wrap any exception raised during cleanup action *)

exception Unwind of exn

let unwind_protect body post =
  try
    let result = body () in
    try post (); result with e -> raise (Unwind e)
  with
  | Unwind e -> raise e    (* assume cleanup has been done *)
  | e -> post (); raise e

let with_resource release acquire x f =
  let res = acquire x in
  unwind_protect
    (fun () -> f res)
    (fun () -> release res)

let with_in_channel openf = with_resource close_in openf

let with_out_channel openf = with_resource close_out openf

let with_process ?error cmd =
  let close chan =
    if Unix.close_process_in chan <> Unix.WEXITED 0 then
      failwith (match error with
                | None -> cmd
                | Some msg -> msg)
  in
  with_resource close Unix.open_process_in cmd

let gensym str =
  sprintf "%s.%d.%09.0f"
    (without_extension str)
    (getpid ())
    (fst (modf (gettimeofday ())) *. 1e9)

let rm file = try Sys.remove file with _ -> ()

let decompressors =
  [(".gz", "/bin/gunzip --stdout"); (".bz2", "/bin/bunzip2 --stdout")]

let is_compressed file =
  match extension file with
  | "" -> false
  | ext -> List.mem_assoc ext decompressors

let decompress file =
  match extension file with
  | "" -> invalid_arg "decompress"
  | ext ->
      let prog = List.assoc ext decompressors in
      let tmp = gensym file in
      let cmd = sprintf "%s %s > %s" prog file tmp in
      if Sys.command cmd = 0 then tmp
      else (rm tmp; failwith "decompress")

let with_decompressed file = with_resource rm decompress file

let decompress_and_apply f file =
  if is_compressed file then with_decompressed file f
  else f file

(* Return a channel for reading a possibly compressed file.
   We decompress it to a temporary file first,
   rather than reading from a pipe or using the CamlZip library,
   so that we detect corrupted files before partially processing them.
   This is also significantly faster than using CamlZip. *)

let open_file = decompress_and_apply open_in

let compressed_versions name =
  if is_compressed name then invalid_arg "compressed_versions";
  name :: List.map (fun (ext, _) -> name ^ ext) decompressors

let file_modtime file = (stat file).st_mtime

let newest_version file =
  let newest cur name =
    try
      let modtime = file_modtime name in
      match cur with
      | None -> Some (name, modtime)
      | Some (f, t) ->
          if modtime > t || (modtime = t && name = file) then
            (* return the original file if it is tied for newest *)
            Some (name, modtime)
          else cur
    with Unix.Unix_error (Unix.ENOENT, "stat", _) -> cur
  in
  let versions = compressed_versions (without_extension file) in
  match List.fold_left newest None versions with
  | Some (f, _) -> f
  | None -> raise Not_found

let copy_channel src dst =
  let len = 4096 in
  let buf = String.create len in
  let rec loop () =
    match input src buf 0 len with
    | 0 -> ()
    | n -> output dst buf 0 n; loop ()
  in
  loop ()

let open_out_excl file =
  out_channel_of_descr (openfile file [O_CREAT; O_WRONLY; O_EXCL] 0o644)

let with_temp_file name proc =
  let file = gensym name in
  with_out_channel open_out_excl file proc;
  file

let update_ctime name =
  try
    let stats = stat name in
    utimes name stats.st_atime stats.st_mtime
  with Unix_error (ENOENT, "stat", _) -> ()

let directory_exists dir =
  Sys.file_exists dir && Sys.is_directory dir

let rec fold_dirs f init path =
  let visit acc name =
    fold_dirs f acc (path ^/ name)
  in
  if directory_exists path then
    Array.fold_left visit (f init path) (try Sys.readdir path with _ -> [||])
  else
    init

let rec fold_non_dirs f init path =
  let visit acc name =
    fold_non_dirs f acc (path ^/ name)
  in
  if directory_exists path then
    Array.fold_left visit init (try Sys.readdir path with _ -> [||])
  else if Sys.file_exists path then
    f init path
  else
    init

let iter_of_fold fold proc = fold (fun () -> proc) ()

let iter_dirs = iter_of_fold fold_dirs

let iter_non_dirs = iter_of_fold fold_non_dirs

let file_size file = (stat file).st_size

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

let drop_privileges ~user ~group =
  (* change group first, since we must still be privileged to change user *)
  (try setgid (getgrnam group).gr_gid
   with Not_found -> failwith ("unknown group " ^ group));
  (try setuid (getpwnam user).pw_uid
   with Not_found -> failwith ("unknown user " ^ user))

let string_of_uerror = function
  | (err, str, "") -> sprintf "%s: %s" str (error_message err)
  | (err, str, arg) -> sprintf "%s: %s (%s)" str (error_message err) arg

let string_of_exception exc =
  match exc with
  | Failure str -> "Failure: " ^ str
  | Invalid_argument str -> "Invalid argument: " ^ str
  | Sys_error str -> str
  | Unix_error (err, str, arg)-> string_of_uerror (err, str, arg)
  | e -> Printexc.to_string e

let perform f x =
  try f x
  with e ->
    prerr_endline (string_of_exception e)

let main_program f x =
  try f x
  with e ->
    prerr_endline (string_of_exception e);
    exit 1

let print_if cond = ksprintf (fun str -> if cond then prerr_endline str)
