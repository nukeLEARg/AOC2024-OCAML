open Core
open Core.Poly
open Graph
open Advent

type direction =
  | East
  | South
  | West
  | North

module CordDirMap = struct
  module Vertex = struct
    type t = int * int * direction

    let compare (x1, y1, d1) (x2, y2, d2) =
      match Stdlib.compare x1 x2 with
      | 0 ->
        (match Stdlib.compare y1 y2 with
         | 0 -> Stdlib.compare d1 d2
         | c -> c)
      | c -> c
    ;;

    let hash (r, c, d) = Hashtbl.hash (r, c, d)
    let equal ((r1, c1, d1) : t) ((r2, c2, d2) : t) : bool = r1 = r2 && c1 = c2 && d1 = d2
  end

  module Edge = struct
    type t = int

    let compare = Stdlib.compare
    let default = 0
  end

  include Imperative.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
end

module Display = struct
  include CordDirMap

  let vertex_name ((x, y, d) : Vertex.t) : string =
    let dir_str =
      match d with
      | East -> "E"
      | South -> "S"
      | West -> "W"
      | North -> "N"
    in
    Printf.sprintf "\"%d,%d:%s\"" x y dir_str
  ;;

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes (_, w, _) = [ `Label (string_of_int w) ]
  let get_subgraph _ = None
end

module Dot = Graphviz.Dot (Display)

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

let () =
  Dot.output_graph stdout graph;
  let dot_file = Out_channel.create "graph.dot" in
  Dot.output_graph dot_file graph;
  Out_channel.close dot_file;
  Printf.printf "File written to graph.dot\n"
;;

module W = struct
  type edge = CordDirMap.E.t
  type t = int

  let weight x = CordDirMap.E.label x
  let zero = 0
  let add = ( + )
  let sub = ( - )
  let compare = compare
end

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
        (Display.vertex_name v2))
  with
  | Not_found ->
    Printf.printf
      "No path found from %s to %s\n"
      (Display.vertex_name start_vertex)
      (Display.vertex_name end_vertex)
;;
