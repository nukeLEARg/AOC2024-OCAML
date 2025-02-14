open Core
open Advent
open Graph
open Nukegraph

let graph = CordDirMap.create ()

let vertices =
  [ 1, 0, East; 2, 0, East; 3, 0, East; 4, 0, East; 5, 0, East; 6, 0, East; 7, 0, East ]
;;

let () = List.iter ~f:(CordDirMap.add_vertex graph) vertices

let edges =
  [ (1, 0, East), (3, 0, East), 3
  ; (3, 0, East), (4, 0, East), 4
  ; (4, 0, East), (2, 0, East), 1
  ; (1, 0, East), (6, 0, East), 2
  ; (3, 0, East), (6, 0, East), 2
  ; (6, 0, East), (5, 0, East), 3
  ; (3, 0, East), (5, 0, East), 2
  ; (5, 0, East), (2, 0, East), 2
  ; (6, 0, East), (7, 0, East), 5
  ; (7, 0, East), (2, 0, East), 2
  ]
;;

let () =
  List.iter edges ~f:(fun (v1, v2, w) ->
    CordDirMap.add_edge_e graph (v1, w, v2);
    CordDirMap.add_edge_e graph (v2, w, v1))
;;

module DijkstraAlgo = Path.Dijkstra (CordDirMap) (W)

let start_vertex = 1, 0, East
let end_vertex = 2, 0, East

let () =
  try
    let edges, total_weight = DijkstraAlgo.shortest_path graph start_vertex end_vertex in
    Printf.printf
      "Shortest path from %s to %s has weight %d:\n"
      (Display.vertex_name start_vertex)
      (Display.vertex_name end_vertex)
      total_weight;
    List.iter edges ~f:(fun (v1, w, v2) ->
      Printf.printf
        "%s --[%d]--> %s\n"
        (Display.vertex_name v1)
        w
        (Display.vertex_name v2);
      highlighted_vertices := v1 :: !highlighted_vertices;
      highlighted_vertices := v2 :: !highlighted_vertices;
      highlighted_edges := (v1, w, v2) :: !highlighted_edges)
  with
  | Not_found ->
    Printf.printf
      "No path found from %s to %s\n"
      (Display.vertex_name start_vertex)
      (Display.vertex_name end_vertex)
;;

let () =
  let dot_file = Out_channel.create "graph.dot" in
  Dot.output_graph dot_file graph;
  Out_channel.close dot_file;
  Printf.printf "File written to graph.dot\n"
;;
