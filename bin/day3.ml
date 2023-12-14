let input = Advent.read_lines "./inputs/03-test.txt"
let () = List.iter print_endline input
let get_nums (s : string) = Advent.get_all_matches (Str.regexp "[0-9]+") s 0

let get_syms (s : string) =
  Advent.get_all_matches (Str.regexp "[\\_<\\$#\\+*?\\_>]+") s 0
;;

let all_nums = List.map (fun line -> get_nums line) input
let all_syms = List.map (fun line -> get_syms line) input

let _ =
  List.iteri
    (fun i x ->
      Core.printf " %s" (Int.to_string i ^ ": ");
      List.iteri
        (fun j (st, loc, len) ->
          Core.printf
            " %s"
            (Int.to_string j
            ^ ": ("
            ^ st
            ^ ", "
            ^ Int.to_string loc
            ^ ", "
            ^ Int.to_string len
            ^ ")"))
        x;
      Core.printf "\n")
    all_syms
;;

(* let () = *)
(*   (* let total = List.fold input ~init:0 ~f:(fun acc line -> *) *)
(*   let nlen = *)
(*     List.fold all_nums ~init:0 ~f:(fun acc num -> *)
(*       let _ = printf " %d " acc in *)
(*       let _ = List.iter num ~f:(fun (nmst, _, _) -> printf "%s " nmst) in *)
(*       if List.length num = 0 then acc else acc + 1) *)
(*   in *)
(*   let _ = printf "\n" in *)
(*   let slen = *)
(*     List.fold all_syms ~init:1 ~f:(fun acc sym -> *)
(*       let _ = printf " a: %d " acc in *)
(*       let _ = List.iter sym ~f:(fun (nmst, _, _) -> printf "%s " nmst) in *)
(*       if List.length sym = 0 then acc else acc + 1) *)
(*   in *)
(*   let _ = printf "\n" in *)
(*   print_endline ("nums: " ^ Int.to_string nlen); *)
(*   print_endline ("syms: " ^ Int.to_string slen); *)
(*   () *)
(* ;; *)

let () =
  (* let total = List.fold input ~init:0 ~f:(fun acc line -> *)
  let valid_num_list =
    Core.List.foldi all_nums ~init:[] ~f:(fun ln acc num ->
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
          @ List.nth all_syms (ln - 1)
      in
      let valid_nums =
        Core.List.fold num ~init:[] ~f:(fun acc x ->
          (* this is where we check the numbers to see proximity to symbols *)
          let st, loc, len = x in
          let mtch =
            Core.List.fold syms ~init:false ~f:(fun acc (_, sloc, _) ->
              let minloc = loc - 1 in
              let maxloc = loc + len + 1 in
              let _ =
                print_endline
                  (st
                  ^ " min: "
                  ^ Int.to_string minloc
                  ^ " max: "
                  ^ Int.to_string maxloc
                  ^ " sloc: "
                  ^ Int.to_string sloc
                  ^ " loc: "
                  ^ Int.to_string loc)
              in
              if sloc >= minloc && sloc <= maxloc then true else acc)
          in
          if mtch then [ Core.Int.of_string st ] :: acc else acc)
      in
      (ln, valid_nums) :: acc)
  in
  Core.List.iter valid_num_list ~f:(fun (ln, nums) ->
    if List.length nums <> 0
    then (
      let _ = Core.printf "%s " ("line: " ^ Int.to_string ln) in
      let _ =
        Core.printf
          " %s "
          (Core.List.fold nums ~init:"" ~f:(fun acc x ->
             if acc = ""
             then Core.List.to_string ~f:Int.to_string x
             else acc ^ ", " ^ Core.List.to_string ~f:Int.to_string x))
      in
      let _ = Core.printf "\n" in
      ())
    else ())
;;
