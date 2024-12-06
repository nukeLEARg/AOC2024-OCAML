open Core
open Advent

let filter_used_pages pages order =
  List.filter order ~f:(fun (p1, p2) ->
    List.mem pages p1 ~equal:Int.equal && List.mem pages p2 ~equal:Int.equal)
;;

let rec check_pages pages (o1, o2) =
  match pages with
  | [] | [ _ ] -> true
  | x :: rest ->
    if x = o2 then false else if x = o1 then true else check_pages rest (o1, o2)
;;

let check_pages_alt pages order =
  let order = filter_used_pages pages order in
  let checked = List.map order ~f:(fun order -> check_pages pages order) in
  List.for_all checked ~f:(fun b -> b)
;;

let rec swap_if_needed (p1, p2) acc lst swap =
  match lst with
  | [] -> List.rev acc
  | x :: rest when x = p2 && List.mem rest p1 ~equal:Int.equal ->
    swap_if_needed (p1, p2) (p1 :: acc) rest true
  | x :: rest when x = p1 && swap -> List.rev_append acc (p2 :: rest)
  | x :: rest -> swap_if_needed (p1, p2) (x :: acc) rest swap
;;

let rec fix_page pages order orig =
  match order with
  | [] -> if check_pages_alt pages orig then pages else fix_page pages orig orig
  | x :: rest ->
    let pages = swap_if_needed x [] pages false in
    fix_page pages rest orig
;;

let () =
  let lines = read_lines "./inputs/d5input.txt" in
  let order, pages = split_on_empty [] lines in
  let order =
    List.map order ~f:(fun s ->
      match String.split s ~on:'|' |> List.map ~f:int_of_string with
      | [] | [ _ ] -> 0, 0
      | x :: y :: _ -> x, y)
  in
  let pages =
    List.map pages ~f:(fun s -> String.split s ~on:',' |> List.map ~f:int_of_string)
  in
  let valid_pages =
    List.filter pages ~f:(fun pages ->
      let order = filter_used_pages pages order in
      let checked = List.map order ~f:(fun order -> check_pages pages order) in
      List.for_all checked ~f:(fun b -> b))
  in
  (*answer:6051*)
  let res =
    List.fold valid_pages ~init:0 ~f:(fun acc pages ->
      acc + List.nth_exn pages (List.length pages / 2))
  in
  let invalid_pages =
    List.filter pages ~f:(fun pages ->
      let order = filter_used_pages pages order in
      let checked = List.map order ~f:(fun order -> check_pages pages order) in
      not (List.for_all checked ~f:(fun b -> b)))
  in
  let fixed_pages =
    List.map invalid_pages ~f:(fun pages ->
      let order = filter_used_pages pages order in
      fix_page pages order order)
  in
  (*answer:5093*)
  let res2 =
    List.fold fixed_pages ~init:0 ~f:(fun acc pages ->
      acc + List.nth_exn pages (List.length pages / 2))
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
