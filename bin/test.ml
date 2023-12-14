let input = Advent.read_lines "./inputs/03-test.txt"
let () = List.iter print_endline input
let get_nums (s : string) = Advent.get_all_matches (Str.regexp "[0-9]+") s 0
let all_nums = List.map (fun line -> get_nums line) input

let () =
  Core.List.iteri all_nums ~f:(fun i x ->
    Core.printf " %s" (Int.to_string i ^ ": ");
    Core.List.iteri x ~f:(fun j (st, loc, len) ->
      Core.printf
        " %s"
        (Int.to_string j
        ^ ": ("
        ^ st
        ^ ", "
        ^ Int.to_string loc
        ^ ", "
        ^ Int.to_string len
        ^ ")"));
    Core.printf "\n")
;;
