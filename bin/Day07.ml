open Core
open Advent

let rec generate_permutations nums =
  match nums with
  | [] -> []
  | [ n ] -> [ [ `Num n ] ]
  | n :: rest ->
    let ex = generate_permutations rest in
    List.concat_map ex ~f:(fun ex -> [ `Num n :: `Add :: ex; `Num n :: `Mul :: ex ])
;;

let rec generate_permutations_wconcat nums =
  match nums with
  | [] -> []
  | [ n ] -> [ [ `Num n ] ]
  | n :: rest ->
    let ex = generate_permutations_wconcat rest in
    List.concat_map ex ~f:(fun ex ->
      [ `Num n :: `Add :: ex; `Num n :: `Mul :: ex; `Num n :: `Con :: ex ])
;;

let rec compute acc ex =
  match ex with
  | [] -> acc
  | `Add :: `Num n :: rest -> compute (acc + n) rest
  | `Mul :: `Num n :: rest -> compute (acc * n) rest
  | `Con :: `Num n :: rest -> compute (concat_integers acc n) rest
  | `Num n :: rest -> compute n rest
  | _ -> acc
;;

let () =
  let lines =
    read_lines "./inputs/d7input.txt"
    |> List.map ~f:(fun line ->
      match String.split line ~on:':' with
      | [ ans; nums ] ->
        let ans = Int.of_string ans in
        let nums =
          String.split nums ~on:' '
          |> List.filter ~f:(fun s -> not (String.is_empty s))
          |> List.map ~f:Int.of_string
        in
        ans, nums
      | _ -> 0, [])
  in
  (*answer:4122618559853*)
  let res =
    List.fold lines ~init:0 ~f:(fun acc (ans, eq) ->
      let expres = generate_permutations eq in
      if List.exists expres ~f:(fun expr -> compute 0 expr = ans) then acc + ans else acc)
  in
  (*answer:227615740238334*)
  let res2 =
    List.fold lines ~init:0 ~f:(fun acc (ans, eq) ->
      let expres = generate_permutations_wconcat eq in
      if List.exists expres ~f:(fun expr -> compute 0 expr = ans) then acc + ans else acc)
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
