(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

let substring ?(from=0) ?until str =
  let i = from in
  let j =
    match until with
    | Some n -> n
    | None -> String.length str
  in
  String.sub str i (j-i)

let explode_path path =
  let rec loop acc i =
    try
      let j = String.index_from path i '/' in
      loop (substring path ~from: i ~until: j :: acc) (j+1)
    with Not_found ->
      substring path ~from: i :: acc
  in
  List.rev (loop [] 0)

let implode_path = String.concat "/"

let (^/) = Filename.concat
