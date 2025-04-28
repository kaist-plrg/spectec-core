open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
open Util.Source

(* Filesystem *)

let rec collect_files ~(suffix : string) dir =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files ~suffix filename
      else if String.ends_with ~suffix filename then files @ [ filename ]
      else files)
    [] files

let mv (filename_src : string) (dirname_dst : string) : unit =
  let filename_base =
    String.split_on_char '/' filename_src |> List.rev |> List.hd
  in
  let filename_dst = dirname_dst ^ "/" ^ filename_base in
  Sys_unix.rename filename_src filename_dst

let rmdir (dirname : string) : unit =
  let files = collect_files ~suffix:".p4" dirname in
  List.iter Sys_unix.remove files;
  Unix.rmdir dirname

let mkdir (dirname : string) : unit = Unix.mkdir dirname 0o755

(* Collect the phantoms from the coverage *)

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

let rec derive_miss (graph : Dep.Graph.t) (pids_covered : PIdSet.t)
    (miss : pid * vid list) : (pid * (vid * int) list) list =
  let pid, vids = miss in
  (* Filter phantoms that were already covered *)
  if not (PIdSet.mem pid pids_covered) then []
  else
    let vids_sample = Rand.random_sample 3 vids in
    List.map
      (fun vid_sample ->
        let vids_source = derive_miss' graph vid_sample in
        (pid, vids_source))
      vids_sample

and derive_miss' (graph : Dep.Graph.t) (vid : vid) : (vid * int) list =
  let vids_visited, depths_visited = derive_vid graph vid in
  (* Sort the source nodes by depth, i.e., closest to the miss derivation *)
  vids_visited
  |> VIdSet.filter (fun vid ->
         vid
         |> Dep.Graph.G.find graph.nodes
         |> Dep.Node.taint |> Dep.Node.is_source)
  |> VIdSet.elements
  |> List.map (fun vid ->
         let depth = VIdMap.find vid depths_visited in
         (vid, depth))
  |> List.sort (fun (_, depth_a) (_, depth_b) -> Int.compare depth_a depth_b)

let derive_misses (graph : Dep.Graph.t) (pids_covered : PIdSet.t)
    (misses : (pid * vid list) list) : (pid * (vid * int) list) list =
  List.concat_map (derive_miss graph pids_covered) misses

(* Mutation of the closest-ASTs *)

let mutate_miss (filename_gen : string) (graph : Dep.Graph.t)
    (vid_program : vid) (vid_source : vid) : unit =
  (* Retrieve the closest source AST *)
  let value_source = Dep.Graph.reassemble_node graph VIdMap.empty vid_source in
  (* Mutate the closest source AST *)
  let value_mutated = Mutate.mutate value_source in
  (* Reassemble the program with the mutated AST *)
  let renamer = VIdMap.singleton value_source.note.vid value_mutated in
  let value_program = Dep.Graph.reassemble_node graph renamer vid_program in
  (* Write the mutated program to a file *)
  let filename_gen = filename_gen ^ ".p4" in
  let oc = open_out filename_gen in
  let program = Interp_sl.Out.out_program value_program in
  Format.asprintf "%a\n" P4el.Pp.pp_program program |> output_string oc;
  close_out oc

let mutate_misses (fuel : int) (dirname_gen : string) (graph : Dep.Graph.t)
    (vid_program : vid) (derivations_source : (pid * (vid * int) list) list) :
    unit =
  (* Randomly sample the closest ASTs *)
  let derivations_source =
    List.map
      (fun (pid, vids_source) ->
        let depths = List.map snd vids_source in
        let depth_max = List.fold_left Int.max 0 depths in
        let probs =
          List.map
            (fun depth ->
              float_of_int (depth_max - depth + 1) /. float_of_int depth_max)
            depths
        in
        let vids_source = Rand.random_sample_weighted 3 probs vids_source in
        (pid, vids_source))
      derivations_source
  in
  let count =
    List.fold_left
      (fun count (_, vids_source) ->
        List.fold_left (fun count (_, _) -> count + 1) count vids_source)
      0 derivations_source
  in
  ">>> Generating " ^ string_of_int count ^ " test programs" |> print_endline;
  List.iter
    (fun (pid, vids_source) ->
      List.iter
        (fun (vid_source, depth) ->
          let filename_gen =
            Format.asprintf "%s/fuel%d-phantom%d-value%d-rank%d" dirname_gen
              fuel pid vid_source depth
          in
          mutate_miss filename_gen graph vid_program vid_source)
        vids_source)
    derivations_source

(* Sort out interesting test programs:

   (i) If the new test program covers a new phantom and is ill-typed
   (ii) If the new test program covers a new phantom and is well-typed
   (iii) If the new test program does not cover a new phantom *)

let rec filter_interesting (config : Config.t) (filenames_gen_p4 : string list)
    : Config.t =
  let total = List.length filenames_gen_p4 in
  List.fold_left
    (fun (count, config) filename_gen_p4 ->
      if count mod (total / 10) = 0 then
        Format.asprintf ">>> %d%% done" (count * 100 / total) |> print_endline;
      let config = filter_interesting' config filename_gen_p4 in
      (count + 1, config))
    (0, config) filenames_gen_p4
  |> snd

and filter_interesting' (config : Config.t) (filename_gen_p4 : string) :
    Config.t =
  let welltyped, cover =
    Interp_sl.Interp.cover_typing config.spec [] filename_gen_p4
  in
  let pids_hit = cover |> SCov.collect_hit |> PIdSet.of_list in
  let pids_new = PIdSet.diff pids_hit config.pids_covered in
  if PIdSet.is_empty pids_new then config
  else (
    print_endline ">>> Found an interesting test program";
    (* Copy the interesting test program to the output directory *)
    if welltyped then mv filename_gen_p4 config.dirname_well_p4
    else mv filename_gen_p4 config.dirname_ill_p4;
    (* Update the set of covered phantoms *)
    let pids_covered = PIdSet.union config.pids_covered pids_new in
    { config with pids_covered })

(* Fuzz loop *)

let fuzz_single (fuel : int) (config : Config.t) (filename_p4 : string) : string
    =
  (* Create a directory for the generated programs *)
  let dirname_gen_single =
    let filename_p4 =
      String.split_on_char '/' filename_p4 |> List.rev |> List.hd
    in
    config.dirname_gen ^ "/" ^ filename_p4
  in
  mkdir dirname_gen_single;
  (* Run the typing rules on the seed program *)
  let ctx, _ =
    Interp_sl.Interp.run_typing ~derive:true config.spec config.includes_p4
      filename_p4
  in
  let cover = !(ctx.cover) in
  let graph = Option.get ctx.graph in
  let vid_program = Option.get ctx.vid_program in
  let misses = SCov.collect_miss cover in
  (* Derive closest ASTs from the closest-miss phantoms *)
  let derivations_source = derive_misses graph config.pids_covered misses in
  (* Mutate the closest ASTs and dump to file *)
  mutate_misses fuel dirname_gen_single graph vid_program derivations_source;
  dirname_gen_single

let fuzz_multiple (fuel : int) (config : Config.t) : Config.t =
  (* Generate test programs from the seed programs *)
  let dirnames_gen =
    List.map (fuzz_single fuel config) config.filenames_seed_p4
  in
  (* Sort out interesting test programs *)
  let filenames_gen_p4 =
    List.concat_map (collect_files ~suffix:".p4") dirnames_gen
  in
  let config = filter_interesting config filenames_gen_p4 in
  (* Remove the generated programs *)
  List.iter rmdir dirnames_gen;
  config

let rec fuzz_loop (fuel : int) (config : Config.t) : Config.t =
  if fuel = 0 then config
  else (
    ">>> Fuzzing loop " ^ string_of_int fuel |> print_endline;
    let config = fuzz_multiple fuel config in
    fuzz_loop (fuel - 1) config)

(* Entry point:

    (i) Generate mutated test programs from a seed program
    (ii) Measure the coverage of seed programs, to be reused in warm boot *)

let fuzz_typing (spec : spec) (includes_p4 : string list)
    (filenames_seed_p4 : string list) (dirname_gen : string)
    (pids_boot : PIdSet.t) : unit =
  (* Create a directory for the generated programs *)
  let dirname_gen =
    let timestamp =
      let tm = Unix.gettimeofday () |> Unix.localtime in
      Format.asprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.Unix.tm_year + 1900)
        (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
        tm.Unix.tm_sec
    in
    dirname_gen ^ "/fuzz-" ^ timestamp
  in
  mkdir dirname_gen;
  (* Create a directory for interesting well-typed programs *)
  let dirname_well_p4 = dirname_gen ^ "/well" in
  mkdir dirname_well_p4;
  (* Create a directory for interesting ill-typed programs *)
  let dirname_ill_p4 = dirname_gen ^ "/ill" in
  mkdir dirname_ill_p4;
  (* Configure the fuzzing loop *)
  let config =
    Config.init spec includes_p4 dirname_gen dirname_well_p4 dirname_ill_p4
      pids_boot filenames_seed_p4
  in
  (* Run the fuzzing loop *)
  fuzz_loop 3 config |> ignore;
  ()

(* Entry point for the fuzzer *)

let fuzz_typing_cold (spec : spec) (includes_p4 : string list)
    (filenames_seed_p4 : string list) (dirname_gen : string)
    (filenames_boot_p4 : string list) : unit =
  let pids_boot = Boot.boot_cold spec includes_p4 filenames_boot_p4 in
  fuzz_typing spec includes_p4 filenames_seed_p4 dirname_gen pids_boot

let fuzz_typing_warm (spec : spec) (includes_p4 : string list)
    (filenames_seed_p4 : string list) (dirname_gen : string)
    (filename_boot : string) : unit =
  let pids_boot = Boot.boot_warm filename_boot in
  fuzz_typing spec includes_p4 filenames_seed_p4 dirname_gen pids_boot
