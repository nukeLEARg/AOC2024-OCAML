open Core
open Advent

let directions = [ -1, 0; 0, 1; 1, 0; 0, -1 ]

let find_trailheads map =
  let zeroes = ref [] in
  List.iteri map ~f:(fun y row ->
    List.iteri row ~f:(fun x n -> if n = 0 then zeroes := (x, y) :: !zeroes));
  !zeroes
;;

let is_valid_move map (x, y) current_value =
  let rows = List.length map in
  let cols = List.length (List.hd_exn map) in
  x >= 0
  && y >= 0
  && x < rows
  && y < cols
  && List.nth_exn (List.nth_exn map y) x = current_value + 1
;;

let rec find_trails map (x, y) current_value found_9 =
  if current_value = 9
  then (x, y) :: found_9
  else
    List.fold directions ~init:[] ~f:(fun acc (dx, dy) ->
      let nx = x + dx in
      let ny = y + dy in
      if is_valid_move map (nx, ny) current_value
      then acc @ find_trails map (nx, ny) (current_value + 1) found_9
      else acc)
;;

let rec find_trails_2 map (x, y) current_value =
  if current_value = 9
  then 1
  else
    List.fold directions ~init:0 ~f:(fun acc (dx, dy) ->
      let nx = x + dx in
      let ny = y + dy in
      if is_valid_move map (nx, ny) current_value
      then acc + find_trails_2 map (nx, ny) (current_value + 1)
      else acc)
;;

let () =
  let lines = read_lines "./inputs/d10input.txt" in
  let map =
    List.map lines ~f:(fun row ->
      String.to_list row |> List.map ~f:(fun ch -> Char.to_int ch - Char.to_int '0'))
  in
  let ths = find_trailheads map in
  let paths = List.map ths ~f:(fun (x, y) -> find_trails map (x, y) 0 []) in
  (*answer:816*)
  let res =
    List.fold paths ~init:0 ~f:(fun acc f ->
      acc + (List.dedup_and_sort f ~compare:Stdlib.compare |> List.length))
  in
  let paths2 = List.map ths ~f:(fun (x, y) -> find_trails_2 map (x, y) 0) in
  (*answer:1960*)
  let res2 = List.fold paths2 ~init:0 ~f:(fun acc i -> acc + i) in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
