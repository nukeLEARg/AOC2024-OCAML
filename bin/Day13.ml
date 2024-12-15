open Core
open Advent

type claw_machine =
  { ax : int
  ; ay : int
  ; bx : int
  ; by : int
  ; rx : int
  ; ry : int
  }

let parse_input input : claw_machine list =
  let regex =
    Re.Pcre.re
      "Button A: X\\+(\\d+), Y\\+(\\d+)\\s+Button B: X\\+(\\d+), Y\\+(\\d+)\\s+Prize: \
       X=(\\d+), Y=(\\d+)"
    |> Re.compile
  in
  List.fold_right input ~init:[] ~f:(fun line acc ->
    match acc with
    | [] when String.is_empty line -> []
    | [] -> [ [ line ] ]
    | group :: rest when String.is_empty line -> [] :: group :: rest
    | group :: rest -> (line :: group) :: rest)
  |> List.map ~f:(fun block ->
    let block_str = String.concat ~sep:" " block |> String.strip in
    let groups = Re.Pcre.exec ~rex:regex block_str in
    { ax = int_of_string (Re.Pcre.get_substring groups 1)
    ; ay = int_of_string (Re.Pcre.get_substring groups 2)
    ; bx = int_of_string (Re.Pcre.get_substring groups 3)
    ; by = int_of_string (Re.Pcre.get_substring groups 4)
    ; rx = int_of_string (Re.Pcre.get_substring groups 5)
    ; ry = int_of_string (Re.Pcre.get_substring groups 6)
    })
;;

let solve_machinept1 (machine : claw_machine) : (int * int) list =
  let rec aux (machine : claw_machine) (acc : (int * int) list) (a : int)
    : (int * int) list
    =
    let b = (machine.rx - (machine.ax * a)) / machine.bx in
    if a > 100
    then acc
    else if
      b <= 100
      && b >= 0
      && (machine.rx - (machine.ax * a)) mod machine.bx = 0
      && (machine.ry - (machine.ay * a)) mod machine.by = 0
      && b = (machine.ry - (machine.ay * a)) / machine.by
    then aux machine ((a, b) :: acc) (a + 1)
    else aux machine acc (a + 1)
  in
  aux machine [] 0
;;

let pricemoves (moves : (int * int) list list) : int list =
  List.map moves ~f:(fun ms ->
    List.fold ms ~init:0 ~f:(fun acc m ->
      let a, b = m in
      let tokens = (3 * a) + b in
      if acc = 0 || tokens < acc then tokens else acc))
;;

let () =
  let lines = read_lines "./inputs/d13input.txt" in
  let machines = parse_input lines in
  let pt1moves = List.map machines ~f:(fun m -> solve_machinept1 m) in
  let pt1prices = pricemoves pt1moves in
  let res = List.fold pt1prices ~init:0 ~f:(fun acc price -> acc + price) in
  let res2 = 1 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
