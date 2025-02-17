open Core
open Core.Poly
open Advent
open Graph
open Nukegraph

let graph = CordDirMap.create ()

module DijkstraAlgo = Path.Dijkstra (CordDirMap) (W)

let () =
  let map = read_lines "./inputs/d16/input.txt" in
  let ymax = List.length map in
  let xmax = String.length (List.hd_exn map) in
  let start_x, start_y, end_x, end_y =
    List.foldi map ~init:(0, 0, 0, 0) ~f:(fun y acc str ->
      String.foldi str ~init:acc ~f:(fun x acc c ->
        let sx, sy, ex, ey = acc in
        match c with
        | 'S' -> x, y, ex, ey
        | 'E' -> sx, sy, x, y
        | _ -> acc))
  in
  let start = start_x, start_y, East in
  let ends = List.map all_directions ~f:(fun d -> end_x, end_y, d) in
  List.iteri map ~f:(fun y row ->
    String.iteri row ~f:(fun x c ->
      if c <> '#'
      then
        List.iter all_directions ~f:(fun d ->
          let vertex = x, y, d in
          CordDirMap.add_vertex graph vertex)));
  List.iteri map ~f:(fun y row ->
    String.iteri row ~f:(fun x c ->
      if c <> '#'
      then
        List.iter all_directions ~f:(fun dir ->
          let current_vertex = x, y, dir in
          let left_vertex = x, y, left_rot dir in
          CordDirMap.add_edge_e graph (current_vertex, 1000, left_vertex);
          let right_vertex = x, y, right_rot dir in
          CordDirMap.add_edge_e graph (current_vertex, 1000, right_vertex);
          let nx, ny = next_pos (x, y) dir in
          if nx >= 0 && nx < xmax && ny >= 0 && ny < ymax
          then (
            let cell = (List.nth_exn map ny).[nx] in
            if cell <> '#'
            then CordDirMap.add_edge_e graph (current_vertex, 1, (nx, ny, dir))))));
  let res =
    List.fold ends ~init:0 ~f:(fun acc endv ->
      let _, dist = DijkstraAlgo.shortest_path graph start endv in
      if acc = 0 || dist < acc then dist else acc)
  in
  let res2 = 1 in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
