(* approx: proxy server for Debian archive files
   Copyright (C) 2015  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open OUnit
open Printf

let p_bool = sprintf "%b"
let p_chr = sprintf "%C"
let p_str = sprintf "%S"
let p_pair pf1 pf2 (x, y) = sprintf "(%s, %s)" (pf1 x) (pf2 y)
let p_str2 = p_pair p_str p_str
let p_list pf x = "[" ^ String.concat "; " (List.map pf x) ^ "]"
let p_int = sprintf "%d"
let p_int64 = sprintf "%Ld"
let p_opt pf = function | Some x -> pf x | None -> "-"
let p_exn = Printexc.to_string

let tear_down _ _ = ()

let assert_invalid ?msg f =
  let result =
    try f (); None
    with e -> Some e
  in
  match result with
  | None ->
      assert_failure "expected Invalid_argument exception, but no exception was raised."
  | Some (Invalid_argument _) -> ()
  | Some e -> assert_failure ("expected Invalid_argument exception, but " ^ p_exn e ^ " was raised.")
