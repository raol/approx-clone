(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

external interface_address : string -> Unix.inet_addr = "interface_address"

external set_ipv6_only : Unix.file_descr -> bool -> unit = "set_ipv6_only"
