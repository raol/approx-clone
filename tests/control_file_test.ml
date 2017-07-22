(* approx: proxy server for Debian archive files
   Copyright (C) 2015  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open OUnit2
open Testlib

let bad_line = "one two three"

let create_bad ctx =
  let file, chan = bracket_tmpfile ctx in
  output_string chan (bad_line ^ "\n");
  close_out chan;
  file

let test_contents =
  "Origin: Debian\n\
Label: Debian\n\
Suite: stable\n\
Version: 8.1\n\
Codename: jessie\n\
Date: Sat, 06 Jun 2015 11:09:34 UTC\n\
Description: Debian 8.1 Released 06 June 2015\n\
MD5Sum:\n\
\ a2ff86b08a2f114d6f0594ff69ef5c4d 14019410 main/binary-all/Packages\n\
\ 9539760c49756bcaaf8640fd903ccbcf       92 main/binary-all/Release\n\
SHA1:\n\
\ 6b8b6dde32d863a7cde06b0c457b7ee4fb36bdbf 14019410 main/binary-all/Packages\n\
\ 98fcd7b597b05f3f86acb0ec07c4d11ddcb670c4       92 main/binary-all/Release\n\
SHA256:\n\
\ 299181e362caae665aa68399bacde59f439a41b900e903c7104feea7a8377af1 14019410 main/binary-all/Packages\n\
\ 84caeff910de244e607524c9b5fd370f064cbb849d3e67a8dac658cc21bba35c       92 main/binary-all/Release\n\
"

let test_paragraph =
  ["origin", "Debian";
   "label", "Debian";
   "suite", "stable";
   "version", "8.1";
   "codename", "jessie";
   "date", "Sat, 06 Jun 2015 11:09:34 UTC";
   "description", "Debian 8.1 Released 06 June 2015";
   "md5sum", "\n\
     a2ff86b08a2f114d6f0594ff69ef5c4d 14019410 main/binary-all/Packages\n\
     9539760c49756bcaaf8640fd903ccbcf       92 main/binary-all/Release";
   "sha1", "\n\
     6b8b6dde32d863a7cde06b0c457b7ee4fb36bdbf 14019410 main/binary-all/Packages\n\
     98fcd7b597b05f3f86acb0ec07c4d11ddcb670c4       92 main/binary-all/Release";
   "sha256", "\n\
     299181e362caae665aa68399bacde59f439a41b900e903c7104feea7a8377af1 14019410 main/binary-all/Packages\n\
     84caeff910de244e607524c9b5fd370f064cbb849d3e67a8dac658cc21bba35c       92 main/binary-all/Release"]

let test_info_list =
  [("299181e362caae665aa68399bacde59f439a41b900e903c7104feea7a8377af1", 14019410L), "main/binary-all/Packages";
   ("84caeff910de244e607524c9b5fd370f064cbb849d3e67a8dac658cc21bba35c", 92L), "main/binary-all/Release"]

let p_info = p_pair (p_pair p_str p_int64) p_str

let create_good ctx =
  let file, chan = bracket_tmpfile ctx in
  output_string chan test_contents;
  close_out chan;
  file

let read_good ctx =
  bracket
    (fun ctx ->
      let file = create_good ctx in
      let p = Control_file.read file in
      p, file)
    tear_down ctx

let read_info ctx =
  bracket
    (fun ctx -> Control_file.read_checksum_info (create_good ctx))
    tear_down ctx

let suite = [

  "read_tests" >:::
  ["(read \"good\")" >::
     (fun ctx ->
      let file = bracket create_good tear_down ctx in
      ignore (Control_file.read file));
   "(read \"bad\")" >::
   (fun ctx ->
     let file = bracket create_bad tear_down ctx in
     assert_raises (Failure ("malformed line: " ^ bad_line))
       (fun () -> (Control_file.read file)))];

  "file_name_test" >::
  (fun ctx ->
    let p, file = read_good ctx in
    assert_equal ~printer: p_str file (Control_file.file_name p));

  "line_number_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    assert_equal ~printer: p_int 1 (Control_file.line_number p));

  "iter_fields_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    let fields_read = ref [] in
    let collect_field pair =
      fields_read := pair :: !fields_read
    in
    Control_file.iter_fields collect_field p;
    let fields = List.rev !fields_read in
    assert_equal ~printer: (p_list p_str2) test_paragraph fields);

  "defined_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    assert_equal ~printer: p_bool false (Control_file.defined "unknown" p));

  "missing_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    assert_raises (Control_file.Missing (p, "unknown"))
      (fun () -> Control_file.lookup "unknown" p));

  "lookup_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    assert_equal ~printer: p_str "jessie" (Control_file.lookup "codename" p));

  "get_checksum_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    let info = List.assoc "sha256" test_paragraph in
    assert_equal ~printer: p_str info (fst (Control_file.get_checksum p)));

  "lookup_info_test" >::
  (fun ctx ->
    let p, _ = read_good ctx in
    assert_equal ~printer: (p_list p_info) test_info_list (Control_file.lookup_info "sha256" p));

  "read_checksum_info_test" >::
  (fun ctx ->
    let info, _ = read_info ctx in
    assert_equal ~printer: (p_list p_info) test_info_list info);

]
