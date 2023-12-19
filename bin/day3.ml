let input = Advent.read_lines "./inputs/03-prod.txt"
let get_nums (s : string) = Advent.get_numbers_from_string s
let get_syms (s : string) = Advent.get_symbols_from_string s
let all_nums = List.map (fun line -> get_nums line) input
let all_syms = List.map (fun line -> get_syms line) input

let () =
  let total =
    Core.List.foldi all_nums ~init:0 ~f:(fun ln acc num ->
      (* num is the list of numbers for this line *)
      (* create list of posible symbol matches *)
      let syms =
        if ln = 0
        then List.nth all_syms ln @ List.nth all_syms (ln + 1)
        else if ln = List.length all_syms - 1
        then List.nth all_syms (ln - 1) @ List.nth all_syms ln
        else
          List.nth all_syms (ln - 1)
          @ List.nth all_syms ln
          @ List.nth all_syms (ln + 1)
      in
      let valid_nums =
        Core.List.fold num ~init:0 ~f:(fun acc x ->
          (* this is where we check the numbers to see proximity to symbols *)
          let st, loc, len = x in
          let mtch =
            Core.List.fold syms ~init:false ~f:(fun acc (sloc, sst) ->
              let minloc = loc - 1 in
              let maxloc = loc + len in
              let mch =
                if sloc >= minloc && sloc <= maxloc then true else acc
              in
              let _ =
                print_endline
                  (Core.Int.to_string ln
                  ^ "("
                  ^ st
                  ^ ", "
                  ^ Core.Char.to_string sst
                  ^ ") "
                  ^ " min: "
                  ^ Int.to_string minloc
                  ^ " max: "
                  ^ Int.to_string maxloc
                  ^ " sloc: "
                  ^ Int.to_string sloc
                  ^ " loc: "
                  ^ Int.to_string loc
                  ^ " "
                  ^ Core.Bool.to_string mch)
              in
              mch)
          in
          if mtch then Core.Int.of_string st + acc else acc)
      in
      valid_nums + acc)
  in
  print_endline (Core.Int.to_string total)
;;

let () = print_endline "\nPart 2\n"

let () =
  let total =
    Core.List.foldi all_syms ~init:0 ~f:(fun ln acc sym ->
      (* num is the list of numbers for this line *)
      (* create list of posible symbol matches *)
      let nums =
        let lst = all_nums in
        if ln = 0
        then List.nth lst ln @ List.nth lst (ln + 1)
        else if ln = List.length lst - 1
        then List.nth lst (ln - 1) @ List.nth lst ln
        else List.nth lst (ln - 1) @ List.nth lst ln @ List.nth lst (ln + 1)
      in
      let valid_syms =
        Core.List.fold sym ~init:0 ~f:(fun acc x ->
          (* this is where we check the numbers to see proximity to symbols *)
          let loc, st = x in
          (* let _ = *)
          (*   Core.List.iter nums ~f:(fun (sst, sloc, _) -> *)
          (*     print_endline (sst ^ " " ^ Core.Int.to_string sloc)) *)
          (* in *)
          (* let _ = print_endline "\n" in *)
          let mtchs =
            Core.List.fold nums ~init:[] ~f:(fun acc (sst, nloc, nlen) ->
              let minloc = loc - 1 in
              let maxloc = loc + 1 in
              let mch =
                if nloc < minloc
                   && nloc + (nlen - 1) >= minloc
                   && nloc + (nlen - 1) <= maxloc
                then (sst, 1) :: acc
                else if nloc > minloc && nloc + nlen <= maxloc
                then (sst, 2) :: acc
                else if nloc >= minloc && nloc <= maxloc
                then (sst, 3) :: acc
                else acc
              in
              let _ =
                print_endline
                  (Core.Int.to_string ln
                  ^ "("
                  ^ Core.Char.to_string st
                  ^ ", "
                  ^ sst
                  ^ ") "
                  ^ " min: "
                  ^ Int.to_string minloc
                  ^ " max: "
                  ^ Int.to_string maxloc
                  ^ " sloc: "
                  ^ Int.to_string nloc
                  ^ " loc: "
                  ^ Int.to_string loc
                  ^ " ["
                  ^ Core.List.fold mch ~init:"" ~f:(fun acc (a, b) ->
                      if acc = ""
                      then " (" ^ a ^ ", " ^ Core.Int.to_string b ^ ")"
                      else acc ^ ", (" ^ a ^ ", " ^ Core.Int.to_string b ^ ")")
                  ^ " ]")
              in
              mch)
          in
          if st = '*'
          then
            if List.length mtchs = 2
            then (
              let _ =
                Core.printf
                  "%s"
                  ("Char "
                  ^ Core.Char.to_string st
                  ^ " at ("
                  ^ Core.Int.to_string ln
                  ^ ", "
                  ^ Core.Int.to_string loc
                  ^ ") ")
              in
              let d =
                Core.List.fold mtchs ~init:0 ~f:(fun acc (a, _) ->
                  if acc = 0
                  then (
                    let _ = Core.printf "%s" a in
                    Core.Int.of_string a)
                  else (
                    let _ = Core.printf ", %s" a in
                    acc * Core.Int.of_string a))
              in
              let _ = Core.printf "\n" in
              d + acc)
            else acc
          else (
            let _ =
              Core.printf
                "%s"
                ("Char "
                ^ Core.Char.to_string st
                ^ " at ("
                ^ Core.Int.to_string ln
                ^ ", "
                ^ Core.Int.to_string loc
                ^ ") ")
            in
            let _ =
              Core.List.iteri mtchs ~f:(fun i (a, _) ->
                if i = 0 then Core.printf "%s" a else Core.printf ", %s" a)
            in
            let _ = Core.printf "\n" in
            acc))
      in
      valid_syms + acc)
  in
  print_endline ("\n" ^ Core.Int.to_string total)
;;
