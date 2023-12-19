open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let rec find_all_matches (i : int) (regexp : Str.regexp) (s : string) =
  match Str.search_forward regexp s i with
  | i ->
    let m = Str.matched_string s in
    let len = String.length m in
    i :: find_all_matches (i + len) regexp s
  | exception _ -> []
;;

let get_symbols_from_string (s : string) =
  let characters = String.to_list s in
  List.filter_mapi characters ~f:(fun col ch ->
    match ch with
    | '0' .. '9' -> None
    | '.' -> None
    | x -> Some (col, x))
;;

let get_numbers_from_string (s : string) =
  let make_num (start : int) (finish : int) =
    let st = String.slice s start (finish + 1) in
    let len = String.length st in
    st, start, len
  in
  let rec aux start finish idx chars acc =
    match start, finish, chars with
    | None, None, '0' .. '9' :: rest ->
      aux (Some idx) (Some idx) (idx + 1) rest acc
    | None, None, _ :: rest -> aux None None (idx + 1) rest acc
    | Some start, _, '0' .. '9' :: rest ->
      aux (Some start) (Some idx) (idx + 1) rest acc
    | Some start, Some finish, _ :: rest ->
      let part = make_num start finish in
      aux None None (idx + 1) rest (part :: acc)
    | Some start, _, [] ->
      let part = make_num start (String.length s - 1) in
      part :: acc
    | _, _, [] -> acc
    (*  Might have missed a case OMEGALUL *)
    | _ -> assert false
  in
  aux None None 0 (String.to_list s) []
;;

(* let rec get_all_matches (regexp : Str.regexp) (s : string) (start : int) = *)
(*   let i = *)
(*     try Str.search_forward regexp s start with *)
(*     | Stdlib.Not_found -> -1 *)
(*     | Not_found_s _ -> -1 *)
(*   in *)
(*   match i with *)
(*   | -1 -> [] *)
(*   | x when x >= 0 -> *)
(*     let m = Str.matched_string s in *)
(*     let offset = i + String.length m in *)
(*     if offset >= String.length s *)
(*     then [] *)
(*     else (m, i, String.length m) :: get_all_matches regexp s offset *)
(*   | _ -> [] *)
(* ;; *)
