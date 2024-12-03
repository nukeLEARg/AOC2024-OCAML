open Core
open Advent

let () =
  let lines = read_lines "./inputs/d1input.txt" in
  let nums =
    List.map lines ~f:(fun line ->
      let n =
        String.split ~on:' ' line
        |> List.filter ~f:(fun s -> not (String.equal s ""))
        |> List.map ~f:int_of_string
      in
      List.hd_exn n, List.rev n |> List.hd_exn)
    |> Stdlib.List.split
  in
  let snums = List.sort (fst nums) ~compare, List.sort (snd nums) ~compare in
  (*answer:765748*)
  let res =
    List.fold2_exn (fst snums) (snd snums) ~init:0 ~f:(fun acc x y ->
      let diff = abs (x - y) in
      acc + diff)
  in
  (*answer:27732508*)
  let res2 =
    List.fold (fst snums) ~init:0 ~f:(fun acc x ->
      let c =
        List.fold (snd snums) ~init:0 ~f:(fun acc2 y -> if x = y then acc2 + 1 else acc2)
      in
      acc + (x * c))
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
