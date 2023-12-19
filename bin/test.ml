let input = Advent.read_lines "./inputs/03-prod.txt"

let () =
  Core.List.iteri input ~f:(fun i x ->
    let nums = Str.split (Str.regexp {|[^0-9]|}) x in
    Core.printf "%d" i;
    Core.printf
      " %s"
      (Core.List.fold nums ~init:"" ~f:(fun acc y ->
         if acc = "" && y <> ""
         then y
         else if y = "" && acc <> ""
         then acc
         else if y = "" && acc = ""
         then ""
         else acc ^ ", " ^ y));
    Core.printf "\n")
;;
