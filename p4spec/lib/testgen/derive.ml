open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module F = Format

(* Derivation of the close-AST from the dependency graph *)

let derive_vid (graph : Dep.Graph.t) (vid : vid) : VIdSet.t * int VIdMap.t =
  let vids_visited = ref (VIdSet.singleton vid) in
  let depths_visited = ref (VIdMap.singleton vid 0) in
  let vids_queue = Queue.create () in
  Queue.add (vid, 0) vids_queue;
  while not (Queue.is_empty vids_queue) do
    let vid_current, depth_current = Queue.take vids_queue in
    match Dep.Graph.G.find_opt graph.edges vid_current with
    | Some edges ->
        Dep.Edges.E.iter
          (fun (_, vid_from) () ->
            if not (VIdSet.mem vid_from !vids_visited) then (
              vids_visited := VIdSet.add vid_from !vids_visited;
              depths_visited :=
                VIdMap.add vid_from (depth_current + 1) !depths_visited;
              Queue.add (vid_from, depth_current + 1) vids_queue))
          edges
    | None -> ()
  done;
  (!vids_visited, !depths_visited)

(* Entry point for deriving close-ASTs *)

let derive_phantom (pid : pid) (graph : Dep.Graph.t) (cover : SCov.Cover.t) :
    (vid * int) list =
  (* Find related values that contributed to the close-miss *)
  let vids_related =
    let branch = SCov.Cover.find pid cover in
    match branch.status with Hit -> [] | Miss vids_related -> vids_related
  in
  (* Randomly sample related vids *)
  let vids_related =
    Rand.random_sample Config.related_vid_samples vids_related
  in
  (* Find close-ASTs for each related values *)
  vids_related
  |> List.concat_map (fun vid_related ->
         let vids_visited, depths_visited = derive_vid graph vid_related in
         vids_visited
         |> VIdSet.filter (fun vid ->
                vid
                |> Dep.Graph.G.find graph.nodes
                |> Dep.Node.taint |> Dep.Node.is_source)
         |> VIdSet.elements
         |> List.map (fun vid ->
                let depth = VIdMap.find vid depths_visited in
                (vid, depth)))
  |> List.sort (fun (_, depth_a) (_, depth_b) -> Int.compare depth_a depth_b)

(* Entry point for debugging close-ASTs *)

let debug_phantom (spec : spec) (includes_p4 : string list)
    (filename_p4 : string) (dirname_debug : string) (pid : pid) : unit =
  match
    Interp_sl.Interp.run_typing ~derive:true spec includes_p4 filename_p4
  with
  | Well (graph, _, cover) ->
      let graph = Option.get graph in
      (* Find related values that contributed to the close-miss *)
      let vids_related =
        let branch = SCov.Cover.find pid cover in
        match branch.status with Hit -> [] | Miss vids_related -> vids_related
      in
      (* Randomly sample related vids *)
      let vids_related =
        Rand.random_sample Config.related_vid_samples vids_related
      in
      (* Log if fail to derive a close-AST *)
      List.iter
        (fun vid_related ->
          let vids_visited, _depths_visited = derive_vid graph vid_related in
          let vids_source =
            vids_visited
            |> VIdSet.filter (fun vid ->
                   vid
                   |> Dep.Graph.G.find graph.nodes
                   |> Dep.Node.taint |> Dep.Node.is_source)
          in
          if VIdSet.is_empty vids_source then (
            F.asprintf "Failed to derive close-AST for pid %d" pid
            |> print_endline;
            let filename_dot =
              F.asprintf "%s/debug_p%d_v%d.dot" dirname_debug pid vid_related
            in
            let oc_dot = open_out filename_dot in
            Dep.Graph.dot_of_graph graph |> output_string oc_dot;
            close_out oc_dot;
            let filename_dot_sub =
              F.asprintf "%s/debug_p%d_v%d_sub.dot" dirname_debug pid vid_related
            in
            let oc_dot_sub = open_out filename_dot_sub in
            "digraph dependencies {\n" |> output_string oc_dot_sub;
            VIdSet.iter
              (fun vid ->
                let node = Dep.Graph.G.find graph.nodes vid in
                let dot = Dep.Node.dot_of_node vid node in
                dot ^ "\n" |> output_string oc_dot_sub)
              vids_visited;
            VIdSet.iter
              (fun vid ->
                let edges = Dep.Graph.G.find graph.edges vid in
                Dep.Edges.E.iter
                  (fun (label, vid_to) () ->
                    let dot = Dep.Edges.dot_of_edge vid label vid_to in
                    dot ^ "\n" |> output_string oc_dot_sub)
                  edges)
              vids_visited;
            "}" |> output_string oc_dot_sub;
            close_out oc_dot_sub)
          else (
            F.asprintf "Found close-AST for pid %d" pid |> print_endline;
            let filename_value =
              F.asprintf "%s/debug_p%d_v%d.value" dirname_debug pid vid_related
            in
            let oc_value = open_out filename_value in
            let values_source =
              vids_source |> VIdSet.elements
              |> List.map (Dep.Graph.reassemble_node graph VIdMap.empty)
            in
            List.iter
              (fun value_source ->
                Sl.Print.string_of_value value_source ^ "\n"
                |> output_string oc_value)
              values_source))
        vids_related
  | Ill _ -> print_endline "ill-typed"
