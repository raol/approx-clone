(* approx: proxy server for Debian archive files
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Curl

let conn =
  global_init CURLINIT_GLOBALALL;
  let conn = init () in
  set_followlocation conn true;
  set_connecttimeout conn 10;
  conn

(* Since the following functions share the same Curl handle,
   each one must reset any options the others might have set *)

let iter url ?(headers=[]) ?(header_callback=ignore) callback =
  set_httpget conn true;
  set_nobody conn false;
  set_url conn url;
  set_httpheader conn headers;
  set_headerfunction conn header_callback;
  set_writefunction conn callback;
  set_filetime conn false;
  perform conn

let mod_time url =
  set_httpget conn false;
  set_nobody conn true;
  set_url conn url;
  set_httpheader conn [];
  set_headerfunction conn ignore;
  set_writefunction conn ignore;
  set_filetime conn true;
  perform conn;
  get_filetime conn
