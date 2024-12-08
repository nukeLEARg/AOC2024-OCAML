open Core
open Advent

let find_ants grid =
  let points = ref [] in
  List.iteri grid ~f:(fun y row ->
    String.iteri row ~f:(fun x c ->
      if Char.is_alphanum c then points := (x, y, c) :: !points));
  !points
;;

let make_nodes (x1, y1) locs =
  List.map locs ~f:(fun (x2, y2) ->
    let dx = x1 - x2 in
    let dy = y1 - y2 in
    x1 + dx, y1 + dy)
;;

let rec find_antinodes front back acc =
  match back with
  | [] -> acc
  | loc :: rest -> find_antinodes (loc :: front) rest (make_nodes loc (rest @ front) @ acc)
;;

let find_max_nodes dx dy mX mY =
  if dx = 0 && dy = 0
  then 1
  else if dx = 0
  then abs mY / dy
  else if dy = 0
  then abs mX / dx
  else min (abs (mX / dx)) (abs (mY / dy))
;;

let make_pt2_nodes (x1, y1) locs mX mY =
  let rec aux dx dy max_nodes acc =
    match max_nodes with
    | 0 -> acc
    | m -> aux dx dy (max_nodes - 1) ((x1 + (dx * m), y1 + (dy * m)) :: acc)
  in
  List.fold locs ~init:[] ~f:(fun acc (x2, y2) ->
    let dx = x1 - x2 in
    let dy = y1 - y2 in
    let max_nodes = find_max_nodes dx dy mX mY in
    aux dx dy max_nodes [] @ acc)
;;

let find_pt2_antinodes ant_locs mX mY =
  let rec aux front back acc =
    match back with
    | [] -> acc
    | loc :: rest -> aux (loc :: front) rest (make_pt2_nodes loc ant_locs mX mY @ acc)
  in
  aux [] ant_locs []
;;

let () =
  let lines = read_lines "./inputs/d8input.txt" in
  let mX = List.length lines - 1 in
  let mY = String.length (List.hd_exn lines) - 1 in
  let antenas_loc = find_ants lines in
  let antenas_loc_map =
    Map.of_alist_multi
      (module Char)
      (List.map antenas_loc ~f:(fun (x, y, c) -> c, (x, y)))
  in
  let antinodes =
    Map.map antenas_loc_map ~f:(fun coords -> find_antinodes [] coords [])
  in
  let res =
    Map.data antinodes
    |> List.concat
    |> List.dedup_and_sort ~compare:Stdlib.compare
    |> List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x <= mX && y <= mY)
    |> List.length
  in
  let antinodes2 =
    Map.map antenas_loc_map ~f:(fun coords -> find_pt2_antinodes coords mX mY)
  in
  let res2 =
    Map.data antinodes2
    |> List.concat
    |> List.dedup_and_sort ~compare:Stdlib.compare
    |> List.filter ~f:(fun (x, y) -> x >= 0 && y >= 0 && x <= mX && y <= mY)
    |> List.length
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
