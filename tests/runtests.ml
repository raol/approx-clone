(* approx: proxy server for Debian archive files
   Copyright (C) 2014  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open OUnit2

let tests = List.concat
    [Util_test.suite;
     Config_file_test.suite;
     Config_test.suite;
     Control_file_test.suite]

let () = run_test_tt_main (test_list tests)
