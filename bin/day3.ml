open Core

let input = Advent.read_lines "./inputs/03-test.txt"
let () = List.iter input ~f:print_endline

let get_nums (s : string) =
  Advent.get_all_matches (Str.regexp "[0-9]+") s
;;

let get_syms (s : string) =
  Advent.get_all_matches (Str.regexp "[\\_<\\$#\\+*?\\_>]+") s
;;

    let nums = List.fold input ~init:[] ~f:(fun acc line ->
      get_nums line @acc);;
    let syms = List.fold input ~init:[] ~f:(fun acc line ->
      get_syms line @acc);;
let () =
  let total = List.fold input ~init:0 ~f:(fun acc line ->
    (* get the numbers from the previous line, current line, and next line *)
  acc + 1);;
