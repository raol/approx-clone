(* approx: proxy server for Debian archive files
   Copyright (C) 2006  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

external address_of_interface : string -> Unix.inet_addr = "inet_addr_of_interface"
