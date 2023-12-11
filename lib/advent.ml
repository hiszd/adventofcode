open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let rec find_all_matches (i : int) (regexp : Str.regexp) (s : string) =
  match Str.search_forward regexp s i with
  | i -> i :: find_all_matches (i + 1) regexp s
  | exception _ -> []
;;

let get_all_matches (regexp : Str.regexp) (s : string) =
  let x = Str.full_split regexp s in
  List.fold ~init:[] x ~f:(fun acc y ->
    match y with
    | Str.Delim y -> y :: acc
    | _ -> acc)
;;
