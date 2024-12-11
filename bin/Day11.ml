open Core
open Advent

let count_digits n =
  if n = 0 then 1 else int_of_float (Float.log10 (Float.abs (float_of_int n))) + 1
;;

let split_number n =
  let digits = int_of_float (Float.log10 (float_of_int n)) + 1 in
  let half_digits = digits / 2 in
  let divisor = int_of_float (10.0 ** float_of_int half_digits) in
  let left = n / divisor in
  let right = n mod divisor in
  right, left
;;

let rec process_stone_set (stones : int list) (acc : int list) =
  match stones with
  | [] -> acc
  | n :: rest when n = 0 -> process_stone_set rest (1 :: acc)
  | n :: rest when count_digits n mod 2 = 0 ->
    let split = split_number n in
    process_stone_set rest (fst split :: snd split :: acc)
  | n :: rest -> process_stone_set rest ((n * 2024) :: acc)
;;

let cycler stones cycles =
  let rec aux current_stones cycle =
    if cycle = 0
    then current_stones
    else aux (process_stone_set current_stones []) (cycle - 1)
  in
  aux stones cycles
;;

let () =
  let input = read_line_as_one "./inputs/d11test.txt" in
  let stones = String.split ~on:' ' input |> List.map ~f:int_of_string in
  let pt1stones = cycler stones 25 in
  let pt2stones = cycler stones 25 in
  let res = List.length pt1stones in
  let res2 = List.length pt2stones in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
