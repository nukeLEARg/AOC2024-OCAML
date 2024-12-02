open Core
open Advent

let rec sequence_check nums increasing decreasing =
  match nums with
  | [] | [ _ ] -> true
  | x :: y :: rest ->
    let diff = x - y in
    if diff = 1 || diff = 2 || diff = 3
    then if not decreasing then sequence_check (y :: rest) true decreasing else false
    else if diff = -1 || diff = -2 || diff = -3
    then if not increasing then sequence_check (y :: rest) increasing true else false
    else false
;;

let rec can_remove_one nums prev =
  match nums with
  | [] -> true
  | [ _ ] -> sequence_check prev false false
  | x :: y :: rest ->
    if sequence_check (prev @ (y :: rest)) false false
    then true
    else can_remove_one (y :: rest) (prev @ [ x ])
;;

let () =
  let lines = read_lines "./inputs/d2input.txt" in
  let nums =
    List.map lines ~f:(fun line -> String.split ~on:' ' line |> List.map ~f:int_of_string)
  in
  let res =
    List.fold nums ~init:0 ~f:(fun acc report ->
      acc + if sequence_check report false false then 1 else 0)
  in
  let res2 =
    List.fold nums ~init:0 ~f:(fun acc report ->
      acc + if sequence_check report false false || can_remove_one report [] then 1 else 0)
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
