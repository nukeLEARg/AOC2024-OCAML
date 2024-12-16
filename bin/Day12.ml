open Core
open Advent

type direction =
  | Up
  | Down
  | Left
  | Right

let dirs = [ Up; Down; Left; Right ]

let move pos dir =
  let x, y = pos in
  match dir with
  | Up -> x - 1, y
  | Down -> x + 1, y
  | Left -> x, y - 1
  | Right -> x, y + 1
;;

let build_cords map =
  let max_x, max_y = Array.length map, Array.length map.(0) in
  List.cartesian_product (List.range 0 max_x) (List.range 0 max_y)
;;

let get_plot map pos =
  let x, y = pos in
  let max_x, max_y = Array.length map, Array.length map.(0) in
  if x >= 0 && x < max_x && y >= 0 && y < max_y then map.(x).(y) else '.'
;;

let set_plot map pos ch =
  let x, y = pos in
  let max_x, max_y = Array.length map, Array.length map.(0) in
  if x >= 0 && x < max_x && y >= 0 && y < max_y then map.(x).(y) <- ch
;;

let rec flood_fill map pos =
  let ch = get_plot map pos in
  if not (Char.between ~low:'A' ~high:'Z' ch)
  then []
  else (
    set_plot map pos (Char.lowercase ch);
    let w =
      List.map dirs ~f:(fun dir ->
        let npos = move pos dir in
        let nch = get_plot map npos in
        if Char.equal ch nch then flood_fill map npos else [])
    in
    pos :: List.concat w)
;;

let fence1 map area =
  let costs =
    List.map
      ~f:(fun pos ->
        let ch = get_plot map pos in
        let f =
          List.map dirs ~f:(fun dir ->
            let npos = move pos dir in
            let nch = get_plot map npos in
            if Char.equal ch nch then 1 else 0)
        in
        4 - List.fold f ~init:0 ~f:(fun acc x -> acc + x))
      area
  in
  List.length area * List.fold costs ~init:0 ~f:(fun acc x -> acc + x)
;;

let in_area plot pos =
  match List.find plot ~f:(fun p -> fst pos = fst p && snd pos = snd p) with
  | Some _ -> true
  | None -> false
;;

let fence2 map area =
  let max_x, max_y = Array.length map, Array.length map.(0) in
  let cost = ref 0 in
  List.iter (List.range 0 max_x) ~f:(fun x ->
    let count_up = ref true in
    let count_down = ref true in
    List.iter (List.range 0 max_y) ~f:(fun y ->
      let pos = x, y in
      if in_area area pos
      then (
        let ch = get_plot map pos in
        let ch_up = get_plot map (move pos Up) in
        let ch_down = get_plot map (move pos Down) in
        let ch_prev = get_plot map (move pos Left) in
        if not (equal_char ch ch_prev)
        then (
          count_up := true;
          count_down := true);
        if equal_char ch ch_up
        then count_up := true
        else if !count_up
        then (
          cost := !cost + 1;
          count_up := false);
        if equal_char ch ch_down
        then count_down := true
        else if !count_down
        then (
          cost := !cost + 1;
          count_down := false))));
  List.iter (List.range 0 max_y) ~f:(fun y ->
    let count_left = ref true in
    let count_right = ref true in
    List.iter (List.range 0 max_x) ~f:(fun x ->
      let pos = x, y in
      if in_area area pos
      then (
        let ch = get_plot map pos in
        let ch_left = get_plot map (move pos Left) in
        let ch_right = get_plot map (move pos Right) in
        let ch_prev = get_plot map (move pos Up) in
        if not (equal_char ch ch_prev)
        then (
          count_left := true;
          count_right := true);
        if equal_char ch ch_left
        then count_left := true
        else if !count_left
        then (
          cost := !cost + 1;
          count_left := false);
        if equal_char ch ch_right
        then count_right := true
        else if !count_right
        then (
          cost := !cost + 1;
          count_right := false))));
  !cost * List.length area
;;

let () =
  let garden =
    read_lines "./inputs/d12/input.txt" |> List.map ~f:String.to_array |> List.to_array
  in
  let coords = build_cords garden in
  let regions =
    List.map coords ~f:(fun y -> flood_fill garden y)
    |> List.filter ~f:(fun a -> not (List.is_empty a))
  in
  (*answer:1363484*)
  let res =
    List.map regions ~f:(fun r -> fence1 garden r)
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
  in
  (*answer:838988*)
  let res2 =
    List.map regions ~f:(fun r -> fence2 garden r)
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
