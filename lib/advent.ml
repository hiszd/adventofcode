open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let rec find_all_matches (i : int) (regexp : Str.regexp) (s : string) =
  match Str.search_forward regexp s i with
  | i ->
    let m = Str.matched_string s in
    let len = String.length m in
    i :: find_all_matches (i + len) regexp s
  | exception _ -> []
;;

let rec get_all_matches (regexp : Str.regexp) (s : string) (start : int) =
  let i =
    try Str.search_forward regexp s start with
    | Stdlib.Not_found -> -1
    | Not_found_s _ -> -1
  in
  match i with
  | -1 -> []
  | x when x > -1 ->
    let m = Str.matched_string s in
    (m, x, String.length m) :: get_all_matches regexp s x
  | _ -> []
;;
