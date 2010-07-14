(* approx: proxy server for Debian archive files
   Copyright (C) 2010  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Config_file
open Util

let version = "4.4"

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

let cache_dir = get "$cache" ~default: "/var/cache/approx"
let params = ("$cache", cache_dir) :: params

let split_cache_path path =
  if is_prefix cache_dir path then
    let i = String.length cache_dir + 1 in
    let j = String.index_from path i '/' in
    substring path ~from: i ~until: j, substring path ~from: (j + 1)
  else
    invalid_arg "split_cache_path"

let shorten path =
  if is_prefix cache_dir path then
    substring path ~from: (String.length cache_dir + 1)
  else
    path

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

let repository_table items =
  String.concat ""
    (List.map
       (fun (k, v) ->
          "<tr><td>" ^ k ^ "</td>\
               <td><a href=\"" ^ v ^ "\">" ^ v ^ "</a></td></tr>\n")
       (sort_config items))

let parameter_table items =
  String.concat ""
    (List.map
       (fun (k, v) -> "<tr><td>" ^ k ^ "</td><td>" ^ v ^ "</td></tr>\n")
       (sort_config items))

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
