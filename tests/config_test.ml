(* approx: proxy server for Debian archive files
   Copyright (C) 2014  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open OUnit2
open List
open Printf
open Testlib

let suite = [

  "cache_dir_test" >::
  (fun _ -> assert_equal ~printer: p_str "/var/cache/approx" Config.cache_dir);

  "split_cache_path_tests" >:::
  map (fun (str, res) ->
    sprintf "(split_cache_path %s)" (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str2 res (Config.split_cache_path str)))
    ["/var/cache/approx/abc/def/ghi",   ("abc", "def/ghi");
     "/var/cache/approx//abc/def/ghi",  ("abc", "def/ghi");
     "/var/cache/approx///abc/def/ghi", ("abc", "def/ghi")]
  @
  (let bad s = (s, Invalid_argument ("split_cache_path: " ^ s)) in
   map (fun (str, e) ->
     sprintf "(split_cache_path %s)" (p_str str) >::
     (fun _ -> assert_raises e (fun () -> Config.split_cache_path str)))
     [bad "abc";
      bad "/abc/def/ghi/jkl";
      bad "/var/cache/approx";
      bad "/var/cache/approx/";
      bad "/var/cache/approx/abc";
      bad "/var/cache/approx/abc/";
      bad "/var/cache/approximately/abc/def/ghi"]);

  "shorten_tests" >:::
  map (fun (str, res) ->
    sprintf "(shorten %s)" (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str res (Config.shorten str)))
    ["/var/cache/approx/abc/def/ghi",   "abc/def/ghi";
     "/var/cache/approx//abc/def/ghi",  "abc/def/ghi";
     "/var/cache/approx///abc/def/ghi", "abc/def/ghi";
     "abc", "abc";
     "/abc/def/ghi/jkl",       "/abc/def/ghi/jkl";
     "/var/cache/approx",      "/var/cache/approx";
     "/var/cache/approx/",     "/var/cache/approx/";
     "/var/cache/approx/abc",  "abc";
     "/var/cache/approx/abc/", "abc/";
     "/var/cache/approximately/abc/def/ghi", "/var/cache/approximately/abc/def/ghi"]

]
