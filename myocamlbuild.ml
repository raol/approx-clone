(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Ocamlbuild_plugin
open Command
open Pathname

let libraries =
  ["pcre"; "sha"; "syslog";
   "netsys"; "netstring"; "netcgi2:netcgi"; "nethttpd-for-netcgi2"]

let split str =
  let i = String.index str ':' in
  String.sub str 0 i, String.sub str (i+1) (String.length str - (i+1))

let add_library lib =
  let inc, lib =
    if String.contains lib ':' then split lib
    else lib, lib
  in
  ocaml_lib ~extern: true ~dir: ("+" ^ inc) lib

let custom_rules () =
  rule "Use mkversion to generate version.ml"
    ~dep: "debian/changelog" ~prod: "version.ml"
    (fun _ _ -> Cmd (S [P (concat pwd "mkversion"); Sh "> version.ml"]));
  dep ["compile"; "ocaml"; "file:log.ml"] ["version.ml"];
  dep ["link"; "ocaml"; "netstubs"] ["netstubs.o"];
  dep ["link"; "ocaml"; "libwrap"] ["libwrap.o"];
  flag ["link"; "ocaml"; "libwrap"] & S [A "-cclib"; A "-lwrap"];
  flag ["link"; "ocaml"; "byte"] & A "-custom";
  flag ["c"; "compile"] & S [A "-ccopt"; A "-Wall"];
  List.iter add_library libraries

let () = dispatch (function After_rules -> custom_rules () | _ -> ())
