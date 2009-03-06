(* approx: proxy server for Debian archive files
   Copyright (C) 2009  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Ocamlbuild_plugin
open Command
open Pathname

let libraries =
  ["pcre"; "sha"; "syslog";
   "netsys"; "netstring"; "netcgi2:netcgi"; "nethttpd-for-netcgi2"]

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
  flag ["ocamlmklib"; "c"] & S [A "-lwrap"];
  dep ["link"; "ocaml"; "use_libapprox"] ["libapprox.a"];
  flag ["link"; "ocaml"; "byte"; "use_libapprox"] & S [A "-dllib"; A "-lapprox"];
  flag ["link"; "ocaml"; "native"; "use_libapprox"] & S [A "-cclib"; A "-lwrap"];
  List.iter add_library libraries

let () = dispatch (function After_rules -> custom_rules () | _ -> ())
