(* approx: proxy server for Debian archive files
   Copyright (C) 2008  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU General Public License *)

open Util

let get_line chan = try Some (input_line chan) with End_of_file -> None

let output_line chan line = output_string chan line; output_char chan '\n'

(* Apply a function to lines m through n *)

let iter_lines proc m n chan =
  let rec loop i =
    if i <= n then
      match get_line chan with
      | Some line -> proc line; loop (i + 1)
      | None -> failwith ("EOF while scanning to line " ^ string_of_int n)
  in
  loop m

(* Apply a function to all remaining lines *)

let iter_eof proc chan =
  let rec loop () =
    match get_line chan with
    | Some line -> proc line; loop ()
    | None -> ()
  in
  loop ()

let copy_lines m n ic oc = iter_lines (output_line oc) m n ic

let delete_lines = iter_lines ignore

(* The following operators implement the corresponding ed commands
   and update the current input line number *)

let append lines n ic oc cur =
  copy_lines cur n ic oc;
  List.iter (output_line oc) lines;
  n + 1

let change lines m n ic oc cur =
  copy_lines cur (m - 1) ic oc;
  delete_lines m n ic;
  List.iter (output_line oc) lines;
  n + 1

let delete = change []

let copy_tail ic oc cur =
  iter_eof (output_line oc) ic;
  0

(* Collect lines until a terminating "." line is seen *)

let get_lines chan =
  let rec loop lines =
    match get_line chan with
    | Some "." -> lines
    | Some line -> loop (line :: lines)
    | None -> failwith "EOF occurred before terminating \".\""
  in
  List.rev (loop [])

let range_of_string str =
  try
    let i = String.index str ',' in
    let start = int_of_string (substring str ~until: i) in
    let stop = int_of_string (substring str ~from: (i + 1)) in
    start, stop
  with Not_found ->
    let n = int_of_string str in
    n, n

(* Ed commands are represented as operators on the
   input channel, output channel, and current line number.
   When applied, each operator returns the updated line number. *)

type t = in_channel -> out_channel -> int -> int

(* Translate an ed command into an operator.
   Additional lines are consumed from the channel
   in the case of append and change commands. *)

let parse_line chan line =
  let last = String.length line - 1 in
  try
    let (m, n) = range_of_string (String.sub line 0 last) in
    match line.[last] with
    | 'a' -> assert (m = n); append (get_lines chan) m
    | 'c' -> change (get_lines chan) m n
    | 'd' -> delete m n
    | _ -> raise Exit
  with _ -> failwith ("malformed ed command: " ^ line)

(* Parse an input channel containing ed commands.
   "diff --ed" produces commands in decreasing line-number order;
   this function effectively reverses that order as it composes
   the operators. *)

let parse chan =
  let compose f g ic oc cur = g ic oc (f ic oc cur) in
  let rec loop op =
    match get_line chan with
    | Some line -> loop (compose (parse_line chan line) op)
    | None -> op
  in
  loop copy_tail

let apply cmds ic oc = ignore (cmds ic oc 1)
