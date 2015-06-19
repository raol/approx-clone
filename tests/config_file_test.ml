(* approx: proxy server for Debian archive files
   Copyright (C) 2015  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open OUnit2
open List
open Printf
open Testlib

let bad_line = "one two three"

let create_bad ctx =
  let file, chan = bracket_tmpfile ctx in
  output_string chan (bad_line ^ "\n");
  close_out chan;
  file

let test_bindings =
  ["$debug", "true";
   "$interval", "120";
   "$user", "approx"]

let create_good ctx =
  let file, chan = bracket_tmpfile ctx in
  let print_binding (k, v) =
    output_string chan "\n";
    output_string chan ("# binding " ^ k ^ " = " ^ v ^ "\n");
    output_string chan (k ^ " " ^ v ^ "\n")
  in
  iter print_binding test_bindings;
  close_out chan;
  file

let cleanup () ctx = Config_file.reset ()

let read_good ctx =
  bracket
    (fun ctx ->
      Config_file.read (create_good ctx))
    cleanup ctx

let suite = [

  "read_tests" >:::
  ["(read \"good\")" >::
   (fun ctx ->
      let file = bracket create_good tear_down ctx in
      assert_equal () (Config_file.read file));
   "(read \"bad\")" >::
   (fun ctx ->
     let file = bracket create_bad tear_down ctx in
     assert_raises (Failure ("malformed line in " ^ file ^ ": " ^ bad_line))
       (fun () -> Config_file.read file))];

  "get_tests" >:::
  map (fun (key, default, res) ->
    sprintf "(get %s %s)" (p_str key) (p_opt p_str default) >::
    (fun ctx ->
      read_good ctx;
      assert_equal ~printer: p_str res (Config_file.get key ?default)))
    ["$user", None, "approx";
     "$syslog", Some "daemon", "daemon"];

  "get_bool_tests" >:::
  map (fun (key, default, res) ->
    sprintf "(get_bool %s %s)" (p_str key) (p_opt p_bool default) >::
    (fun ctx ->
      read_good ctx;
      assert_equal ~printer: p_bool res (Config_file.get_bool key ?default)))
    ["$debug", None, true;
     "$verbose", Some false, false];

  "get_int_tests" >:::
  map (fun (key, default, res) ->
    sprintf "(get_int %s %s)" (p_str key) (p_opt p_int default) >::
    (fun ctx ->
      read_good ctx;
      assert_equal ~printer: p_int res (Config_file.get_int key ?default)))
    ["$interval", None, 120;
     "$percent", Some 50, 50];

  "fold_test" >::
  (fun ctx ->
    read_good ctx;
    let collect_binding key value acc = (key, value) :: acc in
    assert_equal ~printer: (p_list p_str2) test_bindings
      (Config_file.fold collect_binding []));

]
