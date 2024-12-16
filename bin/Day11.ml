open Core
open Advent

let blink stone count stone_freq =
  match stone with
  | 0 ->
    Hashtbl.update stone_freq 1 ~f:(function
      | None -> count
      | Some c -> c + count)
  | _ ->
    if count_digits stone mod 2 = 0
    then (
      let stone1, stone2 = split_number stone in
      Hashtbl.update stone_freq stone1 ~f:(function
        | None -> count
        | Some c -> c + count);
      Hashtbl.update stone_freq stone2 ~f:(function
        | None -> count
        | Some c -> c + count))
    else (
      let new_s = stone * 2024 in
      Hashtbl.update stone_freq new_s ~f:(function
        | None -> count
        | Some c -> c + count))
;;

let process_frequency stone_freq =
  let new_stones = Hashtbl.create (module Int) in
  Hashtbl.iteri stone_freq ~f:(fun ~key:stone ~data:count -> blink stone count new_stones);
  new_stones
;;

let rec cycle freq cycles =
  if cycles = 0 then freq else cycle (process_frequency freq) (cycles - 1)
;;

let () =
  let input =
    read_line_as_one "./inputs/d11/input.txt"
    |> String.split ~on:' '
    |> List.map ~f:int_of_string
  in
  let initial_stones = Hashtbl.create (module Int) in
  List.iter input ~f:(fun n ->
    Hashtbl.update initial_stones n ~f:(function
      | None -> 1
      | Some count -> count + 1));
  let pt1_stones = cycle initial_stones 25 in
  let res =
    Hashtbl.fold pt1_stones ~init:0 ~f:(fun ~key:_ ~data:count acc -> acc + count)
  in
  let pt2_stones = cycle pt1_stones 50 in
  let res2 =
    Hashtbl.fold pt2_stones ~init:0 ~f:(fun ~key:_ ~data:count acc -> acc + count)
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
