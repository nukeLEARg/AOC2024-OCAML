open Core
open Advent

let directions = [| -1, 0; 0, 1; 1, 0; 0, -1 |]
let next_direction dir = if dir >= 3 then 0 else dir + 1

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

let rec loopsearch map (gx, gy) direction loopcount =
  let nx, ny = add_pairs (gx, gy) directions.(direction) in
  if nx >= Array.length map || ny >= Array.length map.(0) || nx < 0 || ny < 0
  then false
  else if
    List.exists loopcount ~f:(fun (ogx, ogy, onx, ony) ->
      if ogx = gx && ogy = gy && onx = nx && ony = ny then true else false)
  then true
  else if Char.equal map.(nx).(ny) '#' || Char.equal map.(nx).(ny) '0'
  then loopsearch map (gx, gy) (next_direction direction) loopcount
  else loopsearch map (nx, ny) direction ((gx, gy, nx, ny) :: loopcount)
;;

let rec move_guard map (gx, gy) direction loopspots =
  let nx, ny = add_pairs (gx, gy) directions.(direction) in
  let _ = map.(gx).(gy) <- 'X' in
  if nx >= Array.length map || ny >= Array.length map.(0) || nx < 0 || ny < 0
  then map, loopspots
  else if Char.equal map.(nx).(ny) '#'
  then move_guard map (gx, gy) (next_direction direction) loopspots
  else (
    let loopmap = Array.copy map in
    let _ = loopmap.(nx).(ny) <- '0' in
    if loopsearch map (gx, gy) (next_direction direction) []
    then (
      let loopspots = (nx, ny) :: loopspots in
      move_guard map (nx, ny) direction loopspots)
    else move_guard map (nx, ny) direction loopspots)
;;

let () =
  let map = read_lines "./inputs/d6input.txt" |> construct_char_grid in
  let newmap, looppoints = move_guard map (find_guard map) 0 [] in
  let res =
    Array.fold newmap ~init:0 ~f:(fun acc arr ->
      acc
      + Array.fold arr ~init:0 ~f:(fun acc c -> if Char.equal c 'X' then acc + 1 else acc))
  in
  let res2 = List.length looppoints in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
