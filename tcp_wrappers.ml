(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

external wrap_hosts_ctl : string -> string -> string -> string -> bool
  = "wrap_hosts_ctl"

let hosts_ctl ?(address="unknown") ?(host="unknown") ?(user="unknown") daemon =
  wrap_hosts_ctl daemon host address user
