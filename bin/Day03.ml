open Core
open Advent

let filter = Re.Pcre.re {|mul\(\d+,\d+\)|} |> Re.compile
let filter2 = Re.Pcre.re {|do\(\)|don't\(\)|mul\(\d+,\d+\)|} |> Re.compile

let num_process s =
  let x = Re.matches num_filter s |> List.map ~f:int_of_string in
  match x with
  | [] | [ _ ] -> 0
  | [ x; y ] -> x * y
  | x :: y :: _ -> x * y
;;

let rec hell_search lst dd acc =
  match lst with
  | [] -> acc
  | [ s ] -> acc + num_process s
  | s :: rest ->
    if String.equal s "do()"
    then hell_search rest true acc
    else if String.equal s "don't()"
    then hell_search rest false acc
    else if dd
    then hell_search rest true acc + num_process s
    else hell_search rest false acc
;;

let () =
  let inputline = read_lines "./inputs/d3input.txt" |> String.concat ~sep:" " in
  let matches = Re.matches filter inputline |> List.map ~f:num_process in
  let matches2 = Re.matches filter2 inputline in
  (*answer:161085926*)
  let res = List.fold matches ~init:0 ~f:(fun acc i -> acc + i) in
  (*answer:82045421*)
  let res2 = hell_search matches2 true 0 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
