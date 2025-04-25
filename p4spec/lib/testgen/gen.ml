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

(* Derivation from the dependency graph *)

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

let derive_miss' (filename_derive : string) (graph : Dep.Graph.t) (vid : vid) :
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

let derive_miss (dirname_derive : string) (graph : Dep.Graph.t)
    (pids_boot : PIdSet.t) (miss : pid * vid list) : unit =
  let pid, vids = miss in
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

let derive_misses (dirname_derive : string) (filename_p4 : string)
    (graph : Dep.Graph.t) (pids_boot : PIdSet.t) (misses : (pid * vid list) list) : unit =
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
  List.iter (derive_miss dirname_derive graph pids_boot) misses

(* Measure initial coverage of phantoms *)

let boot_cold (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) : PIdSet.t =
  print_endline ">>> Booting fuzzing campaign ... measuring seed coverage";
  let cover_multi =
    Interp_sl.Interp.cover_typing spec includes_p4 filenames_p4
  in
  MCov.log ~short:true cover_multi;
  let misses = MCov.collect_miss cover_multi in
  misses |> List.map fst |> PIdSet.of_list

let boot_warm (filename_cov : string) : PIdSet.t =
  print_endline ">>> Booting fuzzing campaign ... loading seed coverage";
  let oc = open_in filename_cov in
  let rec read_lines pids =
    try
      let line = input_line oc in
      let line =
        if String.starts_with ~prefix:"Phantom#" line then
          String.sub line 8 (String.length line - 8)
        else line
      in
      match int_of_string_opt line with
      | Some pid -> read_lines (pid :: pids)
      | None -> read_lines pids
    with End_of_file -> List.rev pids
  in
  let pids = read_lines [] in
  close_in oc;
  pids |> PIdSet.of_list

(* Generate derivations from closest-misses *)

let gen_typing (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string) (pids_boot : PIdSet.t) : unit =
  let ctx, _ =
    Interp_sl.Interp.run_typing ~derive:true spec includes_p4 filename_p4
  in
  let cover = !(ctx.cover) in
  let graph = Option.get ctx.graph in
  let misses = SCov.collect_miss cover in
  derive_misses dirname_derive filename_p4 graph pids_boot misses

let gen_typing_cold (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string)
    (filenames_p4 : string list) : unit =
  let pids_boot = boot_cold spec includes_p4 filenames_p4 in
  gen_typing spec dirname_derive includes_p4 filename_p4 pids_boot

let gen_typing_warm (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string)
    (filename_cov : string) : unit =
  let pids_boot = boot_warm filename_cov in
  gen_typing spec dirname_derive includes_p4 filename_p4 pids_boot

let cover_typing (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) : unit =
  let cover_multi =
    Interp_sl.Interp.cover_typing spec includes_p4 filenames_p4
  in
  MCov.log cover_multi
