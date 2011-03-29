(* approx: proxy server for Debian archive files
   Copyright (C) 2011  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Ocamlbuild_plugin
open Command
open Pathname

let libraries =
  ["pcre"; "sha"; "syslog";
   "netsys_oothr"; "netsys"; "equeue";
   "netstring"; "netcgi2:netcgi"; "nethttpd"]

let split str =
  let i = String.index str ':' in
  String.sub str 0 i, String.sub str (i + 1) (String.length str - (i + 1))

let add_library lib =
  let inc, lib =
    if String.contains lib ':' then split lib
    else lib, lib
  in
  ocaml_lib ~extern: true ~dir: ("+" ^ inc) lib

let custom_rules () =
  List.iter add_library libraries

let () = dispatch (function After_rules -> custom_rules () | _ -> ())
