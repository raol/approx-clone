(* approx: proxy server for Debian archive files
   Copyright (C) 2015  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open OUnit2
open List
open Printf
open Testlib

let create_empty_file ctx =
  bracket
    (fun ctx ->
      let file, chan = bracket_tmpfile ctx in
      close_out chan;
      file)
    tear_down ctx

let create_non_empty_file ctx =
  bracket
    (fun ctx ->
      let file, chan = bracket_tmpfile ctx in
      for _ = 1 to 100 do
	output_string chan "All work and no play makes Jack a dull boy\n"
      done;
      close_out chan;
      file)
    tear_down ctx

let create_tree ctx =
  bracket
    (fun ctx ->
      let root = bracket_tmpdir ctx in
      with_bracket_chdir ctx root
	(fun _ ->
	  close_out (open_out "a");
	  Unix.mkdir "b" 0o755;
	  Unix.mkdir "c" 0o755;
	  close_out (open_out "c/d"));
      root)
    tear_down ctx

let cons lst x = x :: lst

let suite = [

  "is_prefix_tests" >:::
  map (fun (x, y, res) ->
    sprintf "(is_prefix %s %s)" (p_str x) (p_str y) >::
    (fun _ -> assert_equal ~printer: p_bool res (Util.is_prefix x y)))
    ["ban", "banana", true;
     "bar", "banana", false;
     "",    "",       true;
     "",    "abc",    true;
     "abc", "",       false];

  "substring_tests" >:::
  map (fun (from, until, str, res) ->
    sprintf "(substring %s %s %s)"
      (p_opt p_int from) (p_opt p_int until) (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str res (Util.substring ?from ?until str)))
    [None,   None,   "",       "";
     None,   None,   "abcdef", "abcdef";
     Some 0, None,   "abcdef", "abcdef";
     None,   Some 6, "abcdef", "abcdef";
     Some 0, Some 6, "abcdef", "abcdef";
     Some 1, None,   "abcdef", "bcdef";
     Some 1, Some 6, "abcdef", "bcdef";
     None,   Some 5, "abcdef", "abcde";
     Some 0, Some 5, "abcdef", "abcde";
     Some 1, Some 5, "abcdef", "bcde";
     Some 2, Some 4, "abcdef", "cd";
     Some 3, Some 3, "abcdef", "";
     Some 6, None,   "abcdef", "";
     Some 6, Some 6, "abcdef", ""]
  @
  map (fun (from, until, str) ->
    sprintf "(substring %s %s %s)"
      (p_opt p_int from) (p_opt p_int until) (p_str str) >::
    (fun _ -> assert_invalid (fun () -> Util.substring ?from ?until str)))
    [None,   Some 7, "abcdef";
     Some 0, Some 7, "abcdef";
     Some 1, None,   "";
     Some 7, None,   "abcdef";
     Some 4, Some 3, "abcdef"];

  "split_tests" >:::
  map (fun (c, str, res) ->
    sprintf "(split %s %s)" (p_chr c) (p_str str) >::
    (fun _ -> assert_equal ~printer: (p_list p_str) res (Util.split c str)))
    ['/', "abc",    ["abc"];
     '/', "/a/b/c", [""; "a"; "b"; "c"];
     '/', "a/b/c/", ["a"; "b"; "c"; ""];
     '/', "/",      [""; ""]];

  "join_tests" >:::
  map (fun (c, strs, res) ->
    sprintf "(join %s %s)" (p_chr c) (p_list p_str strs) >::
    (fun _ -> assert_equal ~printer: p_str res (Util.join c strs)))
    ['/', ["abc"],             "abc";
     '/', [""; "a"; "b"; "c"], "/a/b/c";
     '/', ["a"; "b"; "c"; ""], "a/b/c/";
     '/', [""; ""],            "/"];

  "relative_path_tests" >:::
  map (fun (str, res) ->
    sprintf "(relative_path %s)" (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str res (Util.relative_path str)))
    ["/a/b/c",             "a/b/c";
     "/abc",               "abc";
     "/abc/",              "abc/";
     "/",                  ".";
     "//",                 ".";
     "",                   "."];

  "relative_url_tests" >:::
  map (fun (str, res) ->
    sprintf "(relative_url %s)" (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str res (Util.relative_url str)))
    ["http://x.y.z/a/b/c",  "a/b/c";
     "http://x.y.z/a/b/c/", "a/b/c/";
     "http://x.y.z/",       "."]
  @
  map (fun (str, e) ->
    sprintf "(relative_url %s)" (p_str str) >::
    (fun _ -> assert_raises e (fun () -> (Util.relative_url str))))
    ["http://x.y.z",      Failure "malformed URL: http://x.y.z";
     "http:/x.y.z/a/b/c", Failure "malformed URL: http:/x.y.z/a/b/c"];

  "split_extension_tests" >:::
  map (fun (str, res) ->
    sprintf "(split_extension %s)" (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str2 res (Util.split_extension str)))
    ["abc.def",          ("abc", ".def");
     "abc.def.ghi",      ("abc.def", ".ghi");
     "abc.",             ("abc", ".");
     ".abc",             ("", ".abc");
     "abc",              ("abc", "");
     "",                 ("", "");
     "/abc.def/ghi.jkl", ("/abc.def/ghi", ".jkl");
     "/abc.def/ghi.",    ("/abc.def/ghi", ".");
     "/abc.def/.ghi",    ("/abc.def/", ".ghi");
     "/abc.def/ghi",     ("/abc.def/ghi", "");
     "/abc.def/.",       ("/abc.def/", ".");
     "/abc.def/",        ("/abc.def/", "");
     "/.",               ("/", ".");
     "/",                ("/", "")];

  "remove_leading_tests" >:::
  map (fun (c, str, res) ->
    sprintf "(remove_leading %s %s)" (p_chr c) (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str res (Util.remove_leading c str)))
    ['/', "abc",     "abc";
     '/', "/abc",    "abc";
     '/', "///abc",  "abc";
     '/', "abc/",    "abc/";
     '/', "/abc/",   "abc/";
     '/', "///abc/", "abc/";
     '/', "/",        "";
     '/', "///",      "";
     '/', "",         ""];

  "remove_trailing_tests" >:::
  map (fun (c, str, res) ->
    sprintf "(remove_trailing %s %s)" (p_chr c) (p_str str) >::
    (fun _ -> assert_equal ~printer: p_str res (Util.remove_trailing c str)))
    ['/', "abc",     "abc";
     '/', "abc/",    "abc";
     '/', "abc///",  "abc";
     '/', "/abc",    "/abc";
     '/', "/abc/",   "/abc";
     '/', "/abc///", "/abc";
     '/', "/",       "";
     '/', "///",     "";
     '/', "",        ""];

  "file_size_tests" >:::
  map (fun (name, creator, size) ->
    sprintf "(file_size %s)" (p_str name) >::
    (fun ctx ->
      let file = creator ctx in
      assert_equal ~printer: p_int64 size (Util.file_size file)))
    ["empty", create_empty_file, 0L;
     "non-empty", create_non_empty_file, 4300L];

  "file_md5sum_tests" >:::
  map (fun (name, creator, md5sum) ->
    sprintf "(file_md5sum %s)" (p_str name) >::
    (fun ctx ->
      let file = creator ctx in
      assert_equal ~printer: p_str md5sum (Util.file_md5sum file)))
    ["empty", create_empty_file, "d41d8cd98f00b204e9800998ecf8427e";
     "non-empty", create_non_empty_file, "e273eb02272f516abfad1bfdfb51caf0"];

  "file_sha1sum_tests" >:::
  map (fun (name, creator, sha1sum) ->
    sprintf "(file_sha1sum %s)" (p_str name) >::
    (fun ctx ->
      let file = creator ctx in
      assert_equal ~printer: p_str sha1sum (Util.file_sha1sum file)))
    ["empty", create_empty_file, "da39a3ee5e6b4b0d3255bfef95601890afd80709";
     "non-empty", create_non_empty_file, "adf46c7e67d75cc73a5b99d7838b3b18f9a4f66d"];

  "file_sha256sum_tests" >:::
  map (fun (name, creator, sha256sum) ->
    sprintf "(file_sha256sum %s)" (p_str name) >::
    (fun ctx ->
      let file = creator ctx in
      assert_equal ~printer: p_str sha256sum (Util.file_sha256sum file)))
    ["empty", create_empty_file, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
     "non-empty", create_non_empty_file, "0d43abb19c4f6fa228c0e577568a99cc6b3768d3ca0f0700e75377d0e08e8793"];

  "fold_dirs_test" >::
  (fun ctx ->
    let root = create_tree ctx in
    let expected = root :: map (Filename.concat root) ["b"; "c"] in
    let got = sort String.compare (Util.fold_dirs cons [] root) in
    assert_equal ~printer: (p_list p_str) expected got);

  "fold_non_dirs_test" >::
  (fun ctx ->
    let root = create_tree ctx in
    let expected = map (Filename.concat root) ["a"; "c/d"] in
    let got = sort String.compare (Util.fold_non_dirs cons [] root) in
    assert_equal ~printer: (p_list p_str) expected got);

]
