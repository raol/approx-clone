(* approx: proxy server for Debian archive files
   Copyright (C) 2014  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config_file
open Util

let version = "5.5"

let default_config = "/etc/approx/approx.conf"

let extract_config_files () =
  let rec loop configs args = function
    | "-c" :: f :: rest | "--config" :: f :: rest ->
        loop (f :: configs) args rest
    | x :: rest ->
        loop configs (x :: args) rest
    | [] -> List.rev configs, List.rev args
  in
  loop [default_config] [] (List.tl (Array.to_list Sys.argv))

let config_files, arguments = extract_config_files ()

let server_config =
  ["version", version;
   "host", Unix.gethostname ();
   "config", String.concat " " config_files]

let () =
  List.iter (fun file -> try read file with Sys_error _ -> ()) config_files

let params = []

let cache_dir =
  let dir = remove_trailing '/' (get "$cache" ~default: "/var/cache/approx") in
  let n = String.length dir in
  if n > 0 && dir.[0] = '/' then dir
  else invalid_arg "$cache"

let params = ("$cache", cache_dir) :: params

let split_cache_path path =
  let err () = invalid_string_arg "split_cache_path" path in
  let dir = cache_dir ^ "/" in
  if is_prefix dir path then
    let i = String.length dir in
    let rest = remove_leading '/' (substring path ~from: i) in
    let j = try String.index rest '/' with Not_found -> err () in
    match (substring rest ~until: j,
           remove_leading '/' (substring rest ~from: (j + 1))) with
    | ("", _) | (_, "") -> err ()
    | pair -> pair
  else
    err ()

let shorten path =
  let dir = cache_dir ^ "/" in
  if is_prefix dir path then
    match remove_leading '/' (substring path ~from: (String.length dir)) with
    | "" -> path
    | str -> str
  else
    path

let interval = get_int "$interval" ~default: 60
let params = ("$interval", string_of_int interval) :: params

let max_rate = get "$max_rate" ~default: "unlimited"
let params = ("$max_rate", max_rate) :: params

let max_redirects = get_int "$max_redirects" ~default: 5
let params = ("$max_redirects", string_of_int max_redirects) :: params

let user = get "$user" ~default: "approx"
let params = ("$user", user) :: params

let group = get "$group" ~default: "approx"
let params = ("$group", group) :: params

let syslog = get "$syslog" ~default: "daemon"
let params = ("$syslog", syslog) :: params

let pdiffs = get_bool "$pdiffs" ~default: true
let params = ("$pdiffs", string_of_bool pdiffs) :: params

let offline = get_bool "$offline" ~default: false
let params = ("$offline", string_of_bool offline) :: params

let max_wait = get_int "$max_wait" ~default: 10 (* seconds *)
let params = ("$max_wait", string_of_int max_wait) :: params

let debug = get_bool "$debug" ~default: false
let params = ("$debug", string_of_bool debug) :: params

let verbose = get_bool "$verbose" ~default: false || debug
let params = ("$verbose", string_of_bool verbose) :: params

let repos = fold (fun k v l -> if k.[0] <> '$' then (k, v) :: l else l) []

let sort_config = List.sort (fun x y -> compare (fst x) (fst y))

let section str =
  "<tr><td colspan=\"2\"><h2>" ^ str ^ "</h2></td></tr>\n"

let rows fmt items =
  String.concat ""
    (List.map (fun (k, v) -> "<tr>" ^ fmt k ^ fmt v ^ "</tr>\n")
       (sort_config items))

let repository_table =
  rows (fun x -> "<td><a href=\"" ^ x ^ "\">" ^ x ^ "</a></td>")

let parameter_table = rows (fun x -> "<td>" ^ x ^ "</td>")

let css =
  "body { margin: 24pt }\n\
   td { padding-right: 18pt }\n\
   td h2 { padding-top: 18pt }\n"

let index =
  "<html>\n\
     <head>\n\
       <title>approx server</title>\n\
       <style type=\"text/css\">\n" ^
         css ^
      "</style>\n\
     </head>\n\
     <body>\n\
       <table>\n" ^
         section "approx server" ^ parameter_table server_config ^
         section "Repository Mappings" ^ repository_table repos ^
         section "Configuration Parameters" ^ parameter_table params ^
      "</table>\n\
     </body>\n\
   </html>"
