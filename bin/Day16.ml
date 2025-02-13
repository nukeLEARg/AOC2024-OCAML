open Core
open Core.Poly
open Graph

type direction =
  | East
  | South
  | West
  | North

module IntGraph = struct
  module Vertex = struct
    type t = int * int * direction

    let compare (r1, c1, d1) (r2, c2, d2) =
      match Stdlib.compare r1 r2 with
      | 0 ->
        (match Stdlib.compare c1 c2 with
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
    let hash = Hashtbl.hash
    let equal = ( = )
    let default = 0
  end

  include Imperative.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
end

module Display = struct
  include IntGraph

  let vertex_name ((r, c, d) : Vertex.t) : string =
    let dir_str =
      match d with
      | East -> "East"
      | South -> "South"
      | West -> "West"
      | North -> "North"
    in
    Printf.sprintf "v_%d_%d_%s" r c dir_str
  ;;

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes (_, w, _) = [ `Label (string_of_int w) ]
  let get_subgraph _ = None
end

module Dot = Graphviz.Dot (Display)

let graph = IntGraph.create ()

let vertices =
  [ 1, 0, East; 2, 0, East; 3, 0, East; 4, 0, East; 5, 0, East; 6, 0, East; 7, 0, East ]
;;

let () = List.iter ~f:(IntGraph.add_vertex graph) vertices

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

let () = List.iter ~f:(fun (v1, v2, w) -> IntGraph.add_edge_e graph (v1, w, v2)) edges

let () =
  Dot.output_graph stdout graph;
  let dot_file = open_out "graph.dot" in
  Dot.output_graph dot_file graph;
  close_out dot_file;
  Printf.printf "file written to graph.dot\n"
;;
