open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple

(* Helper for random sampling *)

let random_sample (size : int) (vids : vid list) =
  let vids_arr = Array.of_list vids in
  let len = Array.length vids_arr in
  if len <= size then vids
  else
    let idxs = Hashtbl.create size in
    let rec random_sample' vids_sample =
      if List.length vids_sample = size then vids_sample
      else
        let idx = Random.int len in
        if Hashtbl.mem idxs idx then random_sample' vids_sample
        else (
          Hashtbl.add idxs idx ();
          random_sample' (vids_arr.(idx) :: vids_sample))
    in
    random_sample' []

(* Derivation of the closest-AST from the dependency graph *)

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

let rec derive_miss (dirname_derive : string) (graph : Dep.Graph.t)
    (pids_boot : PIdSet.t) (miss : pid * vid list) : unit =
  let pid, vids = miss in
  (* Filter phantoms that were already covered *)
  if not (PIdSet.mem pid pids_boot) then ()
  else
    let vids_sample = random_sample 3 vids in
    List.iter
      (fun vid ->
        let filename_derive =
          dirname_derive ^ "/phantom" ^ string_of_int pid ^ "-value"
          ^ string_of_int vid ^ ".value"
        in
        derive_miss' filename_derive graph vid)
      vids_sample

and derive_miss' (filename_derive : string) (graph : Dep.Graph.t) (vid : vid) :
    unit =
  let vids_visited, depths_visited = derive_vid graph vid in
  (* Pick the source nodes *)
  let vids_source =
    VIdSet.filter
      (fun vid ->
        vid
        |> Dep.Graph.G.find graph.nodes
        |> Dep.Node.taint |> Dep.Node.is_source)
      vids_visited
  in
  (* Sort the source nodes by depth, i.e., closest to the miss derivation *)
  let vids_source =
    vids_source |> VIdSet.elements
    |> List.map (fun vid ->
           let depth = VIdMap.find vid depths_visited in
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
  (* Dump the values to a file *)
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

let derive_misses (dirname_derive : string) (filename_p4 : string)
    (graph : Dep.Graph.t) (pids_boot : PIdSet.t)
    (misses : (pid * vid list) list) : unit =
  (* Create a directory for the derived values *)
  let filename_p4 =
    String.split_on_char '/' filename_p4 |> List.rev |> List.hd
  in
  let dirname_derive =
    let timestamp =
      let tm = Unix.gettimeofday () |> Unix.localtime in
      Format.asprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.Unix.tm_year + 1900)
        (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
        tm.Unix.tm_sec
    in
    dirname_derive ^ "/derive-" ^ filename_p4 ^ "-" ^ timestamp
  in
  Unix.mkdir dirname_derive 0o755;
  (* Derive the misses *)
  List.iter (derive_miss dirname_derive graph pids_boot) misses
