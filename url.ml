(* URL access in OCaml using Curl
   Copyright (C) 2005  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU Lesser General Public License *)

let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let iter url ?header ?header_callback callback =
  let connection = Curl.init () in
  Curl.set_followlocation connection true;
  Curl.set_connecttimeout connection 10;
  Curl.set_url connection url;
  (match header with
  | Some str -> Curl.set_httpheader connection [str]
  | None -> ());
  (match header_callback with
  | Some proc -> Curl.set_headerfunction connection proc
  | None -> ());
  Curl.set_writefunction connection callback;
  try
    Curl.perform connection;
    Curl.cleanup connection
  with Curl.CurlException (_, _, msg) ->
    Curl.cleanup connection;
    failwith msg
