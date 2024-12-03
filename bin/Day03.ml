open Core
open Advent
open Re
open Pcre

let filter = Re.Pcre.re {|mul\(\d+,\d+\)|} |> Re.compile

let print_string_list lst =
  let () = Printf.printf "\n" in
  List.iteri
    ~f:(fun i s ->
      if i > 0 then Printf.printf ", ";
      Printf.printf "%s" s)
    lst;
  Printf.printf "\n"
;;

let print_int_list lst =
  Printf.printf "\n[";
  List.iteri
    ~f:(fun i x ->
      if i > 0 then Printf.printf ", ";
      (* Add a comma and space between elements *)
      Printf.printf "%d" x)
    lst;
  Printf.printf "]\n"
;;

let () =
  let inputline = read_lines "./inputs/d3test.txt" |> String.concat ~sep:" " in
  let matches =
    Re.matches filter inputline
    |> List.map ~f:(fun s ->
      let m = Re.matches num_filter s |> List.map ~f:int_of_string in
      match m with
      | [] | [ _ ] -> 0
      | [ x; y ] -> x * y
      | x :: y :: _ -> x * y)
  in
  let res = List.fold matches ~init:0 ~f:(fun acc i -> acc + i) in
  let res2 = 1 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
