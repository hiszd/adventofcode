open Core

let matches =
  [ "one", "1"
  ; "two", "2"
  ; "three", "3"
  ; "four", "4"
  ; "five", "5"
  ; "six", "6"
  ; "seven", "7"
  ; "eight", "8"
  ; "nine", "9"
  ]
;;

exception NoMatch of string

let num_word_to_num (s : string) =
  match List.find matches ~f:(fun (k, _) -> String.equal k s) with
  | Some (_, v) -> v
  | None -> raise (NoMatch s)
;;

let rec get_all_matches (i : int) (regexp : Str.regexp) (s : string) =
  match Str.search_forward regexp s i with
  | i -> i :: get_all_matches (i + 1) regexp s
  | exception _ -> []
;;

(* let print_list (l : int list) = *)
(*   printf "["; *)
(*   List.iteri l ~f:(fun i x -> *)
(*     if i = List.length l - 1 *)
(*     then printf "%d" x *)
(*     else printf "%s" (Int.to_string x ^ ", ")); *)
(*   printf "]\n" *)
(* ;; *)

let replace_matches (s : string) =
  let m =
    List.map matches ~f:(fun (k, v) ->
      let mtchs = get_all_matches 0 (Str.regexp_string k) s in
      k, mtchs @ get_all_matches 0 (Str.regexp_string v) s)
  in
  let maxes =
    List.map m ~f:(fun (k, v) ->
      if List.length v > 0
      then (
        let max = List.fold v ~init:0 ~f:(fun acc x -> max acc x) in
        k, max)
      else k, -1)
  in
  let mx =
    List.fold
      maxes
      ~init:(Option.value_exn (List.nth maxes 0))
      ~f:(fun acc (k, v) ->
        let k1, v1 = acc in
        if v > v1 then k, v else k1, v1)
  in
  let k, _ = mx in
  let last_num = num_word_to_num k in
  let mins =
    List.map m ~f:(fun (k, v) ->
      if List.length v > 0
      then (
        let min =
          List.fold v ~init:(-1) ~f:(fun acc x ->
            if x < acc || (x >= 0 && acc = -1) then x else acc)
        in
        k, min)
      else k, -1)
  in
  let mn =
    List.fold
      mins
      ~init:(Option.value_exn (List.nth mins 0))
      ~f:(fun acc (k, v) ->
        let k1, v1 = acc in
        if (v < v1 && v > -1) || v1 = -1 then k, v else k1, v1)
  in
  let kn, _ = mn in
  let first_num = num_word_to_num kn in
  first_num ^ last_num
;;

let () =
  let lines = Advent.read_lines "./inputs/01-prod.txt" in
  let total =
    List.fold lines ~init:0 ~f:(fun acc line ->
      let numstr = replace_matches line in
      let num = Int.of_string numstr in
      acc + num)
  in
  print_endline (Int.to_string total)
;;

(* let find_matches (s : string list) : string list = *)
(*   List.map s ~f:(fun ss -> replace_matches ss) *)
(* ;; *)

(* let lines = find_matches (Advent.read_lines "./inputs/01-test2.txt");; *)
(***)
(* let lines = Advent.read_lines "./inputs/01-test2.txt";; *)
(***)
(* List.iter lines ~f:(fun line -> print_endline line) *)
(***)
(* let rec find_all_digits (str : char list) : char list = *)
(*   match str with *)
(*   | [] -> [] *)
(*   | first_char :: rest -> *)
(*     if Char.is_digit first_char *)
(*     then first_char :: find_all_digits rest *)
(*     else find_all_digits rest *)
(* ;; *)
(***)
(* let concat (c : string) : string = *)
(*   let chars = String.to_list c in *)
(*   if List.length chars > 1 *)
(*   then ( *)
(*     let first = List.nth chars 0 in *)
(*     let second = List.nth chars (List.length chars - 1) in *)
(*     String.of_char_list [ Option.value_exn first; Option.value_exn second ]) *)
(*   else ( *)
(*     let first = List.nth chars 0 in *)
(*     let second = List.nth chars 0 in *)
(*     String.of_char_list [ Option.value_exn first; Option.value_exn second ]) *)
(* ;; *)
(***)
(* let bob = *)
(*   List.map lines ~f:(fun line -> *)
(*     let digits = find_all_digits (String.to_list line) in *)
(*     Int.of_string (concat (String.of_char_list digits))) *)
(* ;; *)
(***)
(* let final = List.fold bob ~init:0 ~f:(fun acc x -> acc + x);; *)
(***)
(* print_endline (Int.to_string final) *)
