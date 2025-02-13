open Core
open Advent

let rec check_vertical map (x, y) dir =
  match map.(x).(y) with
  | '#' -> false
  | '.' -> true
  | 'O' | '@' -> check_vertical map (x + dir, y) dir
  | '[' -> check_vertical map (x + dir, y) dir && check_vertical map (x + dir, y + 1) dir
  | ']' -> check_vertical map (x + dir, y - 1) dir && check_vertical map (x + dir, y) dir
;;

let rec check_horizontal map (x, y) dir =
  match map.(x).(y) with
  | '#' -> false
  | '.' -> true
  | 'O' | '@' | '[' | ']' -> check_horizontal map (x, y + dir) dir
;;

let rec move_horizontal map ((x, y) as pos) dir =
  match map.(x).(y) with
  | '#' | '.' -> pos
  | ('O' | '@' | '[' | ']') as ch ->
    move_horizontal map (x, y + dir) dir |> ignore;
    map.(x).(y) <- '.';
    map.(x).(y + dir) <- ch;
    x, y + dir
;;

let rec move_vertical map ((x, y) as pos) dir =
  match map.(x).(y) with
  | '#' | '.' -> pos
  | ('O' | '@') as ch ->
    move_vertical map (x + dir, y) dir |> ignore;
    map.(x).(y) <- '.';
    map.(x + dir).(y) <- ch;
    x + dir, y
  | '[' ->
    move_vertical map (x + dir, y) dir |> ignore;
    move_vertical map (x + dir, y + 1) dir |> ignore;
    map.(x).(y) <- '.';
    map.(x).(y + 1) <- '.';
    map.(x + dir).(y) <- '[';
    map.(x + dir).(y + 1) <- ']';
    x + dir, y
  | ']' ->
    move_vertical map (x + dir, y - 1) dir |> ignore;
    move_vertical map (x + dir, y) dir |> ignore;
    map.(x).(y - 1) <- '.';
    map.(x).(y) <- '.';
    map.(x + dir).(y - 1) <- '[';
    map.(x + dir).(y) <- ']';
    x + dir, y
;;

let move_up map pos =
  if check_vertical map pos (-1) then move_vertical map pos (-1) else pos
;;

let move_down map pos = if check_vertical map pos 1 then move_vertical map pos 1 else pos

let move_left map pos =
  if check_horizontal map pos (-1) then move_horizontal map pos (-1) else pos
;;

let move_right map pos =
  if check_horizontal map pos 1 then move_horizontal map pos 1 else pos
;;

let move_set ch =
  match ch with
  | '^' -> move_up
  | 'v' -> move_down
  | '<' -> move_left
  | '>' -> move_right
;;

let find_bot map =
  let x_max, y_max = grid_lim map in
  let coords = List.cartesian_product (List.range 0 y_max) (List.range 0 x_max) in
  coords
  |> List.find_exn ~f:(fun p ->
    let x, y = p in
    Char.equal map.(x).(y) '@')
;;

let run_bot moves map =
  let pos = find_bot map in
  List.fold moves ~init:pos ~f:(fun pos dir ->
    let move = move_set dir in
    move map pos)
  |> ignore;
  map
;;

let resize map =
  Array.map map ~f:(fun row ->
    Array.map row ~f:(fun ch ->
      match ch with
      | '#' -> [ '#'; '#' ]
      | '@' -> [ '@'; '.' ]
      | 'O' -> [ '['; ']' ]
      | '.' -> [ '.'; '.' ])
    |> Array.to_list
    |> List.concat
    |> List.to_array)
;;

let gps_math map =
  let x_max, y_max = grid_lim map in
  let coords = List.cartesian_product (List.range 0 y_max) (List.range 0 x_max) in
  List.fold coords ~init:0 ~f:(fun acc (x, y) ->
    acc
    +
    match map.(x).(y) with
    | 'O' | '[' -> (x * 100) + y
    | _ -> 0)
;;

let () =
  let lines =
    read_lines "./inputs/botinput.txt" |> List.group ~break:(fun _ a -> String.is_empty a)
  in
  let moves = List.nth_exn lines 1 |> List.map ~f:String.to_list |> List.concat in
  let res =
    List.hd_exn lines
    |> List.map ~f:String.to_array
    |> List.to_array
    |> run_bot moves
    |> gps_math
  in
  let res2 =
    List.hd_exn lines
    |> List.map ~f:String.to_array
    |> List.to_array
    |> resize
    |> run_bot moves
    |> gps_math
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
