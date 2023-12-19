let input = Advent.read_lines "./inputs/04-test.txt"

let () =
  let total =
    Core.List.fold input ~init:0 ~f:(fun lacc x ->
      let split_start = Core.String.split_on_chars ~on:[ ':' ] x in
      let split_nums =
        Core.String.split_on_chars ~on:[ '|' ] (List.nth split_start 1)
      in
      let _ =
        Str.search_forward (Str.regexp {|[0-9]+|}) (List.nth split_start 0) 0
      in
      let ln = Str.matched_string (List.nth split_start 0) in
      let winning_numbers =
        Core.String.split_on_chars
          ~on:[ ' ' ]
          (Str.global_replace
             (Str.regexp "  ")
             " "
             (String.trim (List.nth split_nums 0)))
        |> List.map (fun x -> Core.Int.of_string x)
      in
      let posessed_numbers =
        Core.String.split_on_chars
          ~on:[ ' ' ]
          (Str.global_replace
             (Str.regexp "  ")
             " "
             (String.trim (List.nth split_nums 1)))
        |> List.map (fun x -> Core.Int.of_string x)
      in
      let () =
        Core.printf
          "%s: [%s] [%s]\n"
          ln
          (Core.List.fold winning_numbers ~init:"" ~f:(fun acc x ->
             let st = "'" ^ Core.Int.to_string x ^ "'" in
             if acc = "" then st else acc ^ " " ^ st))
          (Core.List.fold posessed_numbers ~init:"" ~f:(fun acc x ->
             let st = "'" ^ Core.Int.to_string x ^ "'" in
             if acc = "" then st else acc ^ " " ^ st))
      in
      Core.List.fold winning_numbers ~init:0 ~f:(fun acc x ->
        let b = Core.List.mem posessed_numbers x ~equal:( = ) in
        if b then if acc = 0 then 1 else acc * acc else acc)
      |> ( + ) lacc)
  in
  Core.printf "\ntotal: %d\n" total
;;