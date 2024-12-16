open Core
open Advent

type bot =
  { p : int * int
  ; v : int * int
  }

type quadrants =
  { tl : int
  ; tr : int
  ; bl : int
  ; br : int
  }
(*
   (*Test grid size*)
let xmax = 11
let ymax = 7*)

(*real grid size*)

let xmax = 101
let ymax = 103
let xcenter = xmax / 2
let ycenter = ymax / 2
let num_filter_neg = Re.Pcre.regexp {|-?\d+|}

let printbot (b : bot) =
  let { p = px, py; v = vx, vy } = b in
  printf "\nBot at pos:%d,%d with velocity:%d,%d%!" px py vx vy
;;

let vizbots (bots : bot list) : string =
  let map = Array.make_matrix ~dimy:xmax ~dimx:ymax '.' in
  List.iter bots ~f:(fun b ->
    let x, y = b.p in
    map.(y).(x) <- '@');
  Array.fold map ~init:[] ~f:(fun acc row -> String.of_array row :: acc)
  |> List.rev
  |> String.concat_lines
;;

let createbot (s : string) : bot =
  let x = Re.matches num_filter_neg s |> List.map ~f:int_of_string in
  match x with
  | [ px; py; vx; vy ] -> { p = px, py; v = vx, vy }
  | _ -> { p = 0, 0; v = 0, 0 }
;;

let advancebot bot =
  let { p = x, y; v = vx, vy } = bot in
  let nx = (x + vx) % xmax in
  let ny = (y + vy) % ymax in
  { p = nx, ny; v = vx, vy }
;;

let cyclehandler (cycles : int) (bot : bot) : bot =
  let rec aux bot cycles =
    if cycles = 0 then bot else aux (advancebot bot) (cycles - 1)
  in
  aux bot cycles
;;

let safetyfactor ({ tl; tr; bl; br } : quadrants) : int = tl * tr * bl * br

let rec search_tree cycles bots =
  let newbots = List.map bots ~f:advancebot in
  let viz = vizbots newbots in
  if String.is_substring ~substring:"@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" viz
  then viz, cycles + 1
  else search_tree (cycles + 1) newbots
;;

let () =
  let lines = read_lines "./inputs/d14/input.txt" in
  let bots = List.map lines ~f:createbot in
  let pt1bots = List.map bots ~f:(cyclehandler 100) in
  let quads =
    List.fold
      pt1bots
      ~init:{ tl = 0; tr = 0; bl = 0; br = 0 }
      ~f:(fun acc { p = x, y; _ } ->
        if x < xcenter && y < ycenter
        then { acc with tl = acc.tl + 1 }
        else if x > xcenter && y < ycenter
        then { acc with tr = acc.tr + 1 }
        else if x < xcenter && y > ycenter
        then { acc with bl = acc.bl + 1 }
        else if x > xcenter && y > ycenter
        then { acc with br = acc.br + 1 }
        else acc)
  in
  let res = safetyfactor quads in
  let viz, res2 = search_tree 0 bots in
  printf "%s" viz;
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
