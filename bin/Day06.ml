open Core
open Advent

let directions = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
let next_direction dir = if dir >= 3 then 0 else dir + 1

module IntTripleSet = Set.Make (IntTripleComparator)

let find_guard map =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let rec search_row r =
    if r >= rows
    then 0, 0
    else (
      let rec search_col c =
        if c >= cols
        then None
        else if Char.equal map.(r).(c) '^'
        then Some (r, c)
        else search_col (c + 1)
      in
      match search_col 0 with
      | None when r + 1 < rows -> search_row (r + 1)
      | None -> 0, 0
      | Some pos -> pos)
  in
  search_row 0
;;

let rec loopsearch map (gx, gy) direction (prepos : IntTripleSet.t) =
  let nx, ny = add_pairs (gx, gy) directions.(direction) in
  let current_position = gx, gy, direction in
  if nx >= Array.length map || ny >= Array.length map.(0) || nx < 0 || ny < 0
  then 0
  else if Set.mem prepos current_position
  then 1
  else if Char.equal map.(nx).(ny) '#'
  then loopsearch map (gx, gy) (next_direction direction) prepos
  else (
    let updated_prepos = Set.add prepos current_position in
    loopsearch map (nx, ny) direction updated_prepos)
;;

let rec move_guard map (gx, gy) direction =
  let nx, ny = add_pairs (gx, gy) directions.(direction) in
  let _ = map.(gx).(gy) <- 'X' in
  if nx >= Array.length map || ny >= Array.length map.(0) || nx < 0 || ny < 0
  then map
  else if Char.equal map.(nx).(ny) '#'
  then move_guard map (gx, gy) (next_direction direction)
  else move_guard map (nx, ny) direction
;;

let gen_blocker_maps (map : char array array) =
  let newblockers = find_char_coordinates 'X' (char_grid_to_string_grid map) in
  List.map newblockers ~f:(fun (x, y) ->
    let blockedmap = Array.map ~f:(fun map -> Array.copy map) map |> Array.copy in
    let _ = blockedmap.(x).(y) <- '#' in
    blockedmap)
;;

let () =
  let map = read_lines "./inputs/mikald6.txt" |> construct_char_grid in
  let guardstart = find_guard map in
  let newmap = move_guard map guardstart 0 in
  (*answer:4559*)
  let res =
    Array.fold newmap ~init:0 ~f:(fun acc arr ->
      acc
      + Array.fold arr ~init:0 ~f:(fun acc c -> if Char.equal c 'X' then acc + 1 else acc))
  in
  let _ = newmap.(fst guardstart).(snd guardstart) <- '^' in
  let blocked_maps = gen_blocker_maps newmap in
  let test =
    List.map blocked_maps ~f:(fun map ->
      let prepos = IntTripleSet.empty in
      loopsearch map guardstart 0 prepos)
  in
  (*answer:1604*)
  let res2 = List.fold test ~init:0 ~f:(fun acc i -> acc + i) in
  printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
