open Sl.Ast
open Runtime_testgen

(* Modules for dependency graph derivation *)

module S = Set.Make (Int)
module D = Map.Make (Int)

(* Derivation from the dependency graph *)

let derive_vid (graph : Dep.Graph.t) (vid : vid) : S.t * int D.t =
  let vids_visited = ref (S.singleton vid) in
  let depths_visited = ref (D.singleton vid 0) in
  let vids_queue = Queue.create () in
  Queue.add (vid, 0) vids_queue;
  while not (Queue.is_empty vids_queue) do
    let vid_current, depth_current = Queue.take vids_queue in
    match Dep.Graph.G.find_opt graph.edges vid_current with
    | Some edges ->
        Dep.Edges.E.iter
          (fun (_, vid_from) () ->
            if not (S.mem vid_from !vids_visited) then (
              vids_visited := S.add vid_from !vids_visited;
              depths_visited :=
                D.add vid_from (depth_current + 1) !depths_visited;
              Queue.add (vid_from, depth_current + 1) vids_queue))
          edges
    | None -> ()
  done;
  (!vids_visited, !depths_visited)

let derive_miss'' (filename_derive : string) (graph : Dep.Graph.t) (vid : vid) :
    unit =
  let vids_visited, depths_visited = derive_vid graph vid in
  (* Pick the source nodes *)
  let vids_source =
    S.filter
      (fun vid ->
        vid
        |> Dep.Graph.G.find graph.nodes
        |> Dep.Node.taint |> Dep.Node.is_source)
      vids_visited
  in
  (* Sort the source nodes by depth, i.e., closest to the miss derivation *)
  let vids_source =
    vids_source |> S.elements
    |> List.map (fun vid ->
           let depth = D.find vid depths_visited in
           (vid, depth))
    |> List.sort (fun (_, depth_a) (_, depth_b) -> Int.compare depth_a depth_b)
  in
  (* Reassemble the values from the source node *)
  let values =
    List.fold_left
      (fun values (vid, depth) ->
        let value_assembled = Dep.Graph.reassemble_node graph vid in
        if
          List.exists
            (fun (value, _) ->
              Runtime_dynamic_sl.Value.eq value value_assembled)
            values
        then values
        else (value_assembled, depth) :: values)
      [] vids_source
    |> List.rev
  in
  if List.length values = 0 then ()
  else
    let oc = open_out filename_derive in
    List.iter
      (fun (value, depth) ->
        Format.asprintf "// Rank %d\n%s\n" depth
          (Sl.Print.string_of_value value)
        |> output_string oc)
      values;
    close_out oc

let derive_miss' (filename_derive : string) (graph : Dep.Graph.t) (vid : vid) :
    unit =
  let vids_visited, depths_visited = derive_vid graph vid in
  let oc = open_out filename_derive in
  if S.is_empty vids_visited then ()
  else
    "// ranks where smaller the depth, more relevant it is\n"
    |> output_string oc;
  D.iter
    (fun vid depth ->
      let taint = Dep.Graph.G.find graph.nodes vid |> Dep.Node.taint in
      if taint = Dep.Node.Red then
        Format.asprintf "// [#%d]: %d\n" vid depth |> output_string oc)
    depths_visited;
  "digraph dependencies {\n" |> output_string oc;
  S.iter
    (fun vid ->
      let node = Dep.Graph.G.find graph.nodes vid in
      let dot = Dep.Node.dot_of_node vid node in
      dot ^ "\n" |> output_string oc)
    vids_visited;
  S.iter
    (fun vid ->
      let edges = Dep.Graph.G.find graph.edges vid in
      Dep.Edges.E.iter
        (fun (label, vid_to) () ->
          let dot = Dep.Edges.dot_of_edge vid label vid_to in
          dot ^ "\n" |> output_string oc)
        edges)
    vids_visited;
  "}" |> output_string oc;
  close_out oc

let derive_miss (dirname_derive : string) (graph : Dep.Graph.t)
    (miss : pid * vid list) : unit =
  let pid, vids = miss in
  List.iter
    (fun vid ->
      let filename_derive =
        dirname_derive ^ "/phantom" ^ string_of_int pid ^ "-value"
        ^ string_of_int vid ^ ".value"
      in
      derive_miss'' filename_derive graph vid)
    vids

let derive_misses (dirname_derive : string) (filename_p4 : string)
    (graph : Dep.Graph.t) (misses : (pid * vid list) list) : unit =
  let filename_p4 =
    String.split_on_char '/' filename_p4 |> List.rev |> List.hd
  in
  let dirname_derive =
    dirname_derive ^ "/derive-" ^ filename_p4 ^ "-"
    ^ (Unix.gettimeofday () |> string_of_float)
  in
  Unix.mkdir dirname_derive 0o755;
  List.iter (derive_miss dirname_derive graph) misses

(* Generate derivations from closest-misses *)

let gen_typing (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string) : unit =
  let ctx, _ =
    Interp_sl.Interp.run_typing ~derive:true spec includes_p4 filename_p4
  in
  let cover = !(ctx.cover) in
  let graph = Option.get ctx.graph in
  let misses = Cov.collect_miss cover in
  derive_misses dirname_derive filename_p4 graph misses
