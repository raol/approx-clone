(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Default_config
open Util

let fsck_release_files () =
  let find file =
    List.iter print_endline (Release.files_invalidated_by file)
  in
  Config.iter (fun dir _ -> treewalk find (cache_dir ^/ dir))

let () = fsck_release_files ()
