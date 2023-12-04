open Core

let input = Advent.read_lines "./inputs/02-prod.txt"

let games =
  List.map input ~f:(fun line ->
    let clnloc = Str.search_forward (Str.regexp ":") line 0 in
    let numstr = String.sub line ~pos:5 ~len:(clnloc - 5) in
    let ind = Int.of_string numstr in
    let strsub1 =
      String.sub line ~pos:(clnloc + 1) ~len:(String.length line - (clnloc + 1))
    in
    let sets =
      String.split_on_chars ~on:[ ';' ] strsub1
      |> List.map ~f:(fun set ->
           let sts =
             String.split_on_chars ~on:[ ',' ] (Stdlib.String.trim set)
           in
           List.map sts ~f:(fun x ->
             let y = String.split_on_chars ~on:[ ' ' ] (Stdlib.String.trim x) in
             List.nth_exn y 1, Int.of_string (List.nth_exn y 0)))
    in
    ind, sets)
;;

(* let () = List.iter input ~f:print_endline *)

(* let () = *)
(*   List.iter games ~f:(fun (ind, sets) -> *)
(*     print_endline ("Game " ^ Int.to_string ind); *)
(*     List.iter sets ~f:(fun set -> *)
(*       print_endline "["; *)
(*       List.iter set ~f:(fun x -> *)
(*         print_endline (fst x ^ " " ^ Int.to_string (snd x)))); *)
(*     print_endline "[") *)
(* ;; *)

let redmax = 12
let grnmax = 13
let blumax = 14

let () =
  let total =
    List.fold games ~init:0 ~f:(fun acc (ind, sets) ->
      let possible =
        List.fold sets ~init:true ~f:(fun acc set ->
          let fldset =
            List.fold set ~init:true ~f:(fun acc set ->
              if acc
              then (
                match fst set with
                | "red" -> snd set <= redmax
                | "green" -> snd set <= grnmax
                | "blue" -> snd set <= blumax
                | _ -> acc)
              else acc)
          in
          match fldset with
          | true -> acc
          | false -> false)
      in
      if possible then acc + ind else acc)
  in
  print_endline ("Total: " ^ Int.to_string total)
;;

let () =
  let power =
    List.fold games ~init:0 ~f:(fun acc (_, sets) ->
      let powers =
        List.fold sets ~init:(1, 1, 1) ~f:(fun acc nam ->
          List.fold nam ~init:acc ~f:(fun acc (clr, num) ->
            let r, g, b = acc in
            match clr with
            | "red" -> if num > 0 && r > 1 then max num r, g, b else num, g, b
            | "green" -> if num > 0 && g > 1 then r, max num g, b else r, num, b
            | "blue" -> if num > 0 && b > 1 then r, g, max num b else r, g, num
            | _ -> acc))
      in
      let r, g, b = powers in
      acc + (r * g * b))
  in
  print_endline ("\nPower: " ^ Int.to_string power)
;;
