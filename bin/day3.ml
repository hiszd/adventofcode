open Core

let input = Advent.read_lines "./inputs/03-test.txt"
let () = List.iter input ~f:print_endline
let get_nums (s : string) = Advent.get_all_matches (Str.regexp "[0-9]+") s

let get_syms (s : string) =
  Advent.get_all_matches (Str.regexp "[\\_<\\$#\\+*?\\_>]+") s
;;

let nums = List.map input ~f:(fun line -> get_nums line)
let syms = List.map input ~f:(fun line -> get_syms line)

let () =
  (* let total = List.fold input ~init:0 ~f:(fun acc line -> *)
  let _ =
    List.fold nums ~init:0 ~f:(fun acc num ->
      let _ =
        List.fold num ~init:0 ~f:(fun acc (nmst, nmloc, nmlen) ->
          (* get the numbers from the previous line, current line, and next line *)
          let syms =
            match acc with
            | 0 -> List.nth_exn syms acc @ List.nth_exn syms (acc + 1)
            | x when x >= List.length input - 1 ->
              List.nth_exn syms acc @ List.nth_exn syms (acc - 1)
            | _ ->
              List.nth_exn syms (acc - 1)
              @ List.nth_exn syms acc
              @ List.nth_exn syms (acc + 1)
          in
          let _ = print_endline (Int.to_string (List.length syms)) in
          let nums =
            List.fold syms ~init:[] ~f:(fun acc (smst, smloc, _) ->
              let _ =
                print_endline
                  (smst
                  ^ " loc: "
                  ^ Int.to_string smloc
                  ^ " bot: "
                  ^ Int.to_string (nmloc - 1)
                  ^ " top: "
                  ^ Int.to_string (nmloc + 1 + nmlen))
              in
              if smloc > nmloc - 1 && smloc < nmloc + 1 + nmlen
              then [ nmst ] @ acc
              else acc)
          in
          let _ = List.iter nums ~f:(fun x -> print_endline x) in
          acc + 1)
      in
      acc + 1)
  in
  ()
;;
