open Domain.Lib
open Sl.Ast
open Util.Source

(* Ticker for node identifier tracking *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := id + 1;
  id

(* Value dependency graph

   Nodes is a mutable map from a value id to a node and its taint.
   Edges is a mutable map from a node to the set of nodes it depends on. *)

module G = Hashtbl.Make (struct
  type t = vid

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t = { nodes : Node.t G.t; edges : Edges.t G.t }

(* Constructor *)

let empty () : t = { nodes = G.create 0; edges = G.create 0 }
let init () : t = { nodes = G.create 100000; edges = G.create 100000 }

(* Size *)

let size (graph : t) : int = graph.nodes |> G.length

(* Adders *)

let add_edge (graph : t) (value_from : value) (value_to : value)
    (label : Edges.label) : unit =
  let vid_from = value_from.note.vid in
  let vid_to = value_to.note.vid in
  (* Add an edge from the from node to the to node *)
  let edge = (label, vid_to) in
  let edges = G.find graph.edges vid_from in
  Edges.E.add edges edge ()

let add_node ~(taint : bool) (graph : t) (value : value) : unit =
  let vid = value.note.vid in
  let typ = value.note.typ in
  (* Create a mirror for the value *)
  let node, values_from =
    match value.it with
    | BoolV b -> (Node.BoolN b, [])
    | NumV n -> (Node.NumN n, [])
    | TextV s -> (Node.TextN s, [])
    | StructV valuefields ->
        let atoms, values = List.split valuefields in
        let vids_from = List.map (fun value -> value.note.vid) values in
        let vidfields = List.combine atoms vids_from in
        (Node.StructN vidfields, values)
    | CaseV (mixop, values) ->
        let vids_from = List.map (fun value -> value.note.vid) values in
        (Node.CaseN (mixop, vids_from), values)
    | TupleV values ->
        let vids_from = List.map (fun value -> value.note.vid) values in
        (Node.TupleN vids_from, values)
    | OptV None -> (Node.OptN None, [])
    | OptV (Some value) ->
        let vid_from = value.note.vid in
        (Node.OptN (Some vid_from), [ value ])
    | ListV values ->
        let vids_from = List.map (fun value -> value.note.vid) values in
        (Node.ListN vids_from, values)
    | FuncV id -> (Node.FuncN id, [])
  in
  (* Create a node for the value *)
  let node =
    let node = node $$$ typ in
    let taint = if taint then Node.Red else Node.White in
    (node, taint)
  in
  G.add graph.nodes vid node;
  (* Add edges, if tainted, expansion edges, otherwise narrow edges *)
  if taint then (
    G.add graph.edges vid (Edges.E.create 0);
    List.iter
      (fun value_from -> add_edge graph value_from value Edges.Expand)
      values_from)
  else (
    G.add graph.edges vid (Edges.E.create (List.length values_from));
    List.iter
      (fun value_from -> add_edge graph value value_from Edges.Narrow)
      values_from)

(* Finders *)

let find_node (graph : t) (vid : vid) : Node.t option =
  G.find_opt graph.nodes vid

(* Reassemblers *)

let rec reassemble_node (graph : t) (renamer : value VIdMap.t) (vid : vid) :
    value =
  match VIdMap.find_opt vid renamer with
  | Some value -> value
  | _ -> reassemble_node' graph renamer vid

and reassemble_node' (graph : t) (renamer : value VIdMap.t) (vid : vid) : value
    =
  let mirror = G.find graph.nodes vid |> fst in
  let typ = mirror.note in
  let value =
    match mirror.it with
    | BoolN b -> BoolV b
    | NumN n -> NumV n
    | TextN s -> TextV s
    | StructN valuefields ->
        let atoms, vids = List.split valuefields in
        let values = List.map (reassemble_node graph renamer) vids in
        let valuefields = List.combine atoms values in
        StructV valuefields
    | CaseN (mixop, vids) ->
        let values = List.map (reassemble_node graph renamer) vids in
        CaseV (mixop, values)
    | TupleN vids ->
        let values = List.map (reassemble_node graph renamer) vids in
        TupleV values
    | OptN vid_opt ->
        let value_opt = Option.map (reassemble_node graph renamer) vid_opt in
        OptV value_opt
    | ListN vids ->
        let values = List.map (reassemble_node graph renamer) vids in
        ListV values
    | FuncN id -> FuncV id
  in
  value $$$ { vid; typ }

(* Dot output *)

let dot_of_nodes (nodes : Node.t G.t) : string =
  G.fold
    (fun vid node dot ->
      Format.asprintf "%s\n%s" dot (Node.dot_of_node vid node))
    nodes ""

let dot_of_edges (edges : Edges.t G.t) : string =
  G.fold
    (fun vid_from edges dot ->
      Edges.E.fold
        (fun (label, vid_to) () dot ->
          Format.asprintf "%s\n  %s" dot
            (Edges.dot_of_edge vid_from label vid_to))
        edges dot)
    edges ""

let dot_of_graph (graph : t) : string =
  "digraph dependencies {\n" ^ dot_of_nodes graph.nodes
  ^ dot_of_edges graph.edges ^ "}"
