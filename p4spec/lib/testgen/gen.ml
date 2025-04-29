open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module F = Format
open Util.Source

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

let rec derive_miss (graph : Dep.Graph.t) (pids_uncovered : PIdSet.t)
    (miss : pid * vid list) : (pid * (vid * int) list) list =
  let pid, vids = miss in
  (* Filter phantoms that were already covered *)
  if not (PIdSet.mem pid pids_uncovered) then []
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

let derive_misses (graph : Dep.Graph.t) (pids_uncovered : PIdSet.t)
    (misses : (pid * vid list) list) : (pid * (vid * int) list) list =
  List.concat_map (derive_miss graph pids_uncovered) misses

(* Mutation of the closest-ASTs *)

let rec mutate_miss (config : Config.t) (filename_gen : string)
    (comment_gen : string) (graph : Dep.Graph.t) (vid_program : vid)
    (vid_source : vid) : unit =
  (* Retrieve the closest source AST *)
  let value_source = Dep.Graph.reassemble_node graph VIdMap.empty vid_source in
  (* Mutate the closest source AST *)
  let value_mutated_opt = Mutate.mutate config.specenv.tdenv value_source in
  Option.iter
    (mutate_miss' filename_gen comment_gen graph vid_program value_source)
    value_mutated_opt

and mutate_miss' (filename_gen : string) (comment_gen : string)
    (graph : Dep.Graph.t) (vid_program : vid) (value_source : value)
    (value_mutated : value) : unit =
  (* Reassemble the program with the mutated AST *)
  let renamer = VIdMap.singleton value_source.note.vid value_mutated in
  let value_program = Dep.Graph.reassemble_node graph renamer vid_program in
  let program = Interp_sl.Out.out_program value_program in
  (* Write the mutated program to a file *)
  let oc = open_out filename_gen in
  F.asprintf "%s\n/*\nFrom %s\nTo %s\n*/\n\n%a\n" comment_gen
    (Sl.Print.string_of_value value_source)
    (Sl.Print.string_of_value value_mutated)
    P4el.Pp.pp_program program
  |> output_string oc;
  close_out oc

let mutate_misses (fuel : int) (config : Config.t) (filename_p4 : string)
    (dirname_gen : string) (graph : Dep.Graph.t) (vid_program : vid)
    (derivations_source : (pid * (vid * int) list) list) : unit =
  (* Randomly sample the close-ASTs, yet always take the first one *)
  let derivations_source =
    List.map
      (fun (pid, vids_source) ->
        let vids_source =
          match vids_source with
          | [] -> vids_source
          | vid_source :: vids_source ->
              let depths = List.map snd vids_source in
              let depth_max = List.fold_left Int.max 0 depths in
              let probs =
                List.map
                  (fun depth ->
                    float_of_int (depth_max - depth + 1)
                    /. float_of_int depth_max)
                  depths
              in
              let vids_source =
                Rand.random_sample_weighted 2 probs vids_source
              in
              vid_source :: vids_source
        in
        (pid, vids_source))
      derivations_source
  in
  let derivations_count =
    List.fold_left
      (fun count (_, vids_source) ->
        List.fold_left (fun count (_, _) -> count + 1) count vids_source)
      0 derivations_source
  in
  F.asprintf "Start generating %d test programs" derivations_count
  |> Config.log config;
  List.iteri
    (fun idx_a (pid, vids_source) ->
      List.iteri
        (fun idx_b (vid_source, depth) ->
          let filename_base = Filesys.base ~suffix:".p4" filename_p4 in
          let filename_gen =
            F.asprintf "%s/%s_F%d_A%dB%d.p4" dirname_gen filename_base fuel
              idx_a idx_b
          in
          let comment_gen =
            F.asprintf "// Intended pid %d\n// Source vid %d\n// Depth %d\n" pid
              vid_source depth
          in
          mutate_miss config filename_gen comment_gen graph vid_program
            vid_source)
        vids_source)
    derivations_source;
  F.asprintf "Generated %d test programs" derivations_count |> Config.log config

(* Sort out interesting test programs:

   (i) If the new test program covers a new phantom and is ill-typed
   (ii) If the new test program covers a new phantom and is well-typed
   (iii) If the new test program does not cover a new phantom *)

let rec filter_interesting (config : Config.t) (filenames_gen_p4 : string list)
    : Config.t =
  let filenames_gen_count = List.length filenames_gen_p4 in
  F.asprintf "Evaluating %d test programs" filenames_gen_count
  |> Config.log config;
  List.fold_left
    (fun (idx, config) filename_gen_p4 ->
      if idx mod (filenames_gen_count / 10) = 0 then
        F.asprintf "%.2f%% done"
          (float_of_int idx
          /. float_of_int filenames_gen_count
          *. float_of_int 100)
        |> Config.log config;
      let config = filter_interesting' config filename_gen_p4 in
      (idx + 1, config))
    (0, config) filenames_gen_p4
  |> snd

and filter_interesting' (config : Config.t) (filename_gen_p4 : string) :
    Config.t =
  let welltyped, cover =
    match
      Interp_sl.Interp.run_typing config.specenv.spec config.specenv.includes_p4
        filename_gen_p4
    with
    | Well (_, _, cover) -> (true, cover)
    | Ill cover -> (false, cover)
  in
  let pids_hit = cover |> SCov.collect_hit |> PIdSet.of_list in
  let pids_new = PIdSet.inter pids_hit config.seed.pids_uncovered in
  if PIdSet.is_empty pids_new then config
  else
    (* Copy the interesting test program to the output directory *)
    let filename_covered =
      if welltyped then
        Filesys.mv filename_gen_p4 config.outdirs.dirname_well_p4
      else Filesys.mv filename_gen_p4 config.outdirs.dirname_ill_p4
    in
    F.asprintf "%s covers %s" filename_covered
      (pids_new |> PIdSet.elements |> List.map string_of_int
     |> String.concat ", ")
    |> Config.log config;
    let oc = open_out_gen [ Open_append; Open_text ] 0o666 filename_covered in
    F.asprintf "\n// Covered pids %s\n"
      (pids_new |> PIdSet.elements |> List.map string_of_int
     |> String.concat ", ")
    |> output_string oc;
    close_out oc;
    (* Update the set of covered phantoms *)
    let pids_uncovered = PIdSet.diff config.seed.pids_uncovered pids_new in
    { config with seed = { config.seed with pids_uncovered } }

(* Fuzz loop *)

let fuzz_single (fuel : int) (config : Config.t) (filename_p4 : string) : string
    =
  (* Create a directory for the generated programs *)
  let dirname_gen_single =
    config.outdirs.dirname_gen ^ "/" ^ Filesys.base ~suffix:".p4" filename_p4
  in
  Filesys.mkdir dirname_gen_single;
  (* Run the typing rules on the seed program : assuming it is well-typed *)
  (match
     Interp_sl.Interp.run_typing ~derive:true config.specenv.spec
       config.specenv.includes_p4 filename_p4
   with
  | Well (graph, vid_program, cover) ->
      let graph = Option.get graph in
      let vid_program = Option.get vid_program in
      let misses = SCov.collect_miss cover in
      (* Derive closest ASTs from the closest-miss phantoms *)
      let derivations_source =
        derive_misses graph config.seed.pids_uncovered misses
      in
      (* Mutate the closest ASTs and dump to file *)
      mutate_misses fuel config filename_p4 dirname_gen_single graph vid_program
        derivations_source
  | Ill _ -> ());
  dirname_gen_single

let fuzz_multiple (fuel : int) (config : Config.t) : Config.t =
  (* Generate test programs from the seed programs *)
  let dirnames_gen =
    List.map (fuzz_single fuel config) config.seed.filenames_seed_p4
  in
  (* Sort out interesting test programs *)
  let filenames_gen_p4 =
    List.concat_map (Filesys.collect_files ~suffix:".p4") dirnames_gen
  in
  let config = filter_interesting config filenames_gen_p4 in
  (* Remove the generated programs *)
  List.iter Filesys.rmdir dirnames_gen;
  config

let rec fuzz_loop (fuel : int) (config : Config.t) : Config.t =
  if fuel = 0 then config
  else (
    F.asprintf "Start fuzzing loop %d with %d nodes uncovered" fuel
      (PIdSet.cardinal config.seed.pids_uncovered)
    |> Config.log config;
    let config = fuzz_multiple fuel config in
    F.asprintf "End fuzzing loop %d with %d nodes uncovered" fuel
      (PIdSet.cardinal config.seed.pids_uncovered)
    |> Config.log config;
    fuzz_loop (fuel - 1) config)

(* Entry point to main fuzzing loop *)

type bootmode = Cold of string list | Warm of string

let fuzz_typing_init (spec : spec) (includes_p4 : string list)
    (filenames_seed_p4 : string list) (dirname_gen : string)
    (bootmode : bootmode) : Config.t =
  (* Create a timestamp *)
  let timestamp =
    let tm = Unix.gettimeofday () |> Unix.localtime in
    F.asprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
  in
  (* Create a logger *)
  let logname = dirname_gen ^ "/fuzz-" ^ timestamp ^ ".log" in
  let logger = Logger.init logname in
  (* Create a spec environment *)
  Logger.log logger "Loading type definitions from the spec file";
  let specenv = Config.init_specenv spec includes_p4 in
  (* Create directories for generated programs *)
  let dirname_gen = dirname_gen ^ "/fuzz-" ^ timestamp in
  let outdirs = Config.init_outdirs dirname_gen in
  (* Create a seed *)
  Logger.log logger "Booting initial coverage";
  let pids_uncovered =
    match bootmode with
    | Cold filenames_boot_p4 ->
        Boot.boot_cold spec includes_p4 filenames_boot_p4
    | Warm filename_boot -> Boot.boot_warm filename_boot
  in
  Logger.log logger
    (F.asprintf "Found %d uncovered phantoms" (PIdSet.cardinal pids_uncovered));
  let seed = Config.init_seed filenames_seed_p4 pids_uncovered in
  (* Create a configuration *)
  let config = Config.init logger specenv outdirs seed in
  (* Init random seed *)
  Random.init 2025;
  config

let fuzz_typing_cold (spec : spec) (includes_p4 : string list)
    (filenames_seed_p4 : string list) (dirname_gen : string)
    (filenames_boot_p4 : string list) : unit =
  let bootmode = Cold filenames_boot_p4 in
  let config =
    fuzz_typing_init spec includes_p4 filenames_seed_p4 dirname_gen bootmode
  in
  let config = fuzz_loop 2 config in
  Config.close config

let fuzz_typing_warm (spec : spec) (includes_p4 : string list)
    (filenames_seed_p4 : string list) (dirname_gen : string)
    (filename_boot : string) : unit =
  let bootmode = Warm filename_boot in
  let config =
    fuzz_typing_init spec includes_p4 filenames_seed_p4 dirname_gen bootmode
  in
  let config = fuzz_loop 2 config in
  Config.close config
