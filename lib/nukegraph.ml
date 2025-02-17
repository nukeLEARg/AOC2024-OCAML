open Core
open Core.Poly
open Graph

type direction =
  | East
  | South
  | West
  | North

let highlighted_vertices = ref []
let highlighted_edges = ref []

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

  let vertex_attributes v =
    if List.mem !highlighted_vertices v ~equal:(fun a b -> CordDirMap.Vertex.equal a b)
    then [ `Fillcolor 16711680; `Style `Filled ]
    else []
  ;;

  let default_edge_attributes _ = []

  let edge_attributes (v1, w, v2) =
    if
      List.mem !highlighted_edges (v1, w, v2) ~equal:(fun (a1, w1, a2) (b1, w2, b2) ->
        CordDirMap.Vertex.equal a1 b1 && CordDirMap.Vertex.equal a2 b2 && w1 = w2)
    then [ `Color 16711680; `Label (string_of_int w) ]
    else [ `Label (string_of_int w) ]
  ;;

  let get_subgraph _ = None
end

module Dot = Graphviz.Dot (Display)

module W = struct
  type edge = CordDirMap.E.t
  type t = int

  let weight x = CordDirMap.E.label x
  let zero = 0
  let add = ( + )
  let sub = ( - )
  let compare = compare
end

let all_directions = [ North; East; South; West ]

let left_rot = function
  | North -> West
  | West -> South
  | South -> East
  | East -> North
;;

let right_rot = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North
;;

let next_pos (x, y) = function
  | North -> x, y - 1
  | East -> x + 1, y
  | South -> x, y + 1
  | West -> x - 1, y
;;
