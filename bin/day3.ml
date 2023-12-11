open Core

let input = Advent.read_lines "./inputs/03-test.txt"
let () = List.iter input ~f:print_endline

let _ =
  List.fold input ~init:0 ~f:(fun nac line ->
    (* let num_finds = Advent.get_all_matches (Str.regexp "[0-9]+") line in *)
    let sym_finds =
      Advent.get_all_matches (Str.regexp "[\\_<\\$#\\+*?\\_>]+") line
    in
    (* let num_loc = *)
    (*   List.fold ~init:[] num_finds ~f:(fun acc find -> *)
    (*     (find, Str.search_forward (Str.regexp find) line 0) :: acc) *)
    (* in *)
    let sym_loc =
      List.mapi sym_finds ~f:(fun i find ->
        let n = (find, Str.search_forward (Str.regexp find) line 0)
        in
        let lines = [i-1; i; i+1] in
      )
    in
    (* let tot = List.fold ~init:[] sym_loc ~f:(fun acc sym ->  *)
    (*   ) in *)
    (* List.iter num_loc ~f:(fun find -> *)
    (*   print_endline *)
    (*     ("num: (" ^ Int.to_string (snd find + 1) ^ ", " ^ fst find ^ ")")); *)
    List.iter sym_loc ~f:(fun find ->
      print_endline
        ("sym: (" ^ Int.to_string (snd find + 1) ^ ", " ^ fst find ^ ")"));
    (* let fin = *)
    (*   List.fold ~init:"" find_loc ~f:(fun axc find -> *)
    (*     axc ^ " (" ^ fst find ^ ", " ^ Int.to_string (snd find + 1) ^ ")") *)
    (* in *)
    (* if String.length fin > 0 then print_endline (Int.to_string acc ^ ": " ^ fin); *)
    nac + 1)
;;
