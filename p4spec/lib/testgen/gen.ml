open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module F = Format
open Util.Error
open Util.Source

(* Overview of the fuzzing loop

   (#) Pre-loop: Measure the initial coverage of the phantom nodes

   (#) Loop
      1. For each phantoms that were missed:
          A. Identify close-miss filenames
          B. Randomly sample N close-miss filenames
          C. For each close-miss filename:
              i. Run SL interpreter on the program
              ii. Fetch derivations, i.e., a set of close-ASTs for the phantom
              iii. For each close-AST:
                    (1) Mutate the close-AST
                    (2) Reassemble the program with the mutated AST
                    (3) Run the SL interpreter on the mutated program
                    (4) See if it has covered the phantom
      2. Repeat the loop until the fuel is exhausted *)

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

(* Check if the mutated file is interesting,
   and if so, copy it to the output directory *)

let update_hit_new (fuel : int) (pid : pid) (idx_derivation : int)
    (idx_mutation : int) (config : Config.t) (filename_gen_p4 : string)
    (welltyped : bool) (pids_hit_new : PIdSet.t) : unit =
  (* Copy the interesting test program to the output directory *)
  let filename_hit_p4 =
    if welltyped then Filesys.cp filename_gen_p4 config.outdirs.dirname_well_p4
    else Filesys.cp filename_gen_p4 config.outdirs.dirname_ill_p4
  in
  F.asprintf
    "[Fuel %d] [Phantom %d] [Derivation %d] [Mutation %d] %s covers %s (%s %d)"
    fuel pid idx_derivation idx_mutation filename_hit_p4
    (PIdSet.to_string pids_hit_new)
    (if PIdSet.mem pid pids_hit_new then "good" else "bad")
    pid
  |> Config.log config;
  let oc = open_out_gen [ Open_append; Open_text ] 0o666 filename_hit_p4 in
  F.asprintf "\n// Covered pids %s\n" (PIdSet.to_string pids_hit_new)
  |> output_string oc;
  close_out oc;
  (* Update the set of covered phantoms *)
  Config.update_hit_cover_seed config pids_hit_new

let update_close_miss_new (fuel : int) (pid : pid) (idx_derivation : int)
    (idx_mutation : int) (config : Config.t) (filename_gen_p4 : string)
    (pids_close_miss_new : PIdSet.t) : unit =
  (* Copy the interesting test program to the output directory *)
  let filename_close_miss_p4 =
    Filesys.cp filename_gen_p4 config.outdirs.dirname_close_miss_p4
  in
  F.asprintf
    "[Fuel %d] [Phantom %d] [Derivation %d] [Mutation %d] %s close-misses %s"
    fuel pid idx_derivation idx_mutation filename_close_miss_p4
    (PIdSet.to_string pids_close_miss_new)
  |> Config.log config;
  let oc =
    open_out_gen [ Open_append; Open_text ] 0o666 filename_close_miss_p4
  in
  F.asprintf "\n// Close-missed pids %s\n"
    (PIdSet.to_string pids_close_miss_new)
  |> output_string oc;
  close_out oc;
  (* Update the set of covered phantoms *)
  Config.update_close_miss_cover_seed config filename_close_miss_p4
    pids_close_miss_new

let update_interesting (fuel : int) (pid : pid) (idx_derivation : int)
    (idx_mutation : int) (config : Config.t) (filename_gen_p4 : string)
    (value_program : value) : unit =
  (* Evaluate the generated program to see if it is interesting *)
  let time_start = Unix.gettimeofday () in
  F.asprintf
    "[Fuel %d] [Phantom %d] [Derivation %d] [Mutation %d] Evaluating %s" fuel
    pid idx_derivation idx_mutation filename_gen_p4
  |> Config.log config;
  let welltyped, cover =
    match
      Interp_sl.Interp.run_typing_internal config.specenv.spec filename_gen_p4
        value_program
    with
    | Well (_, _, cover) -> (true, cover)
    | Ill (_, _, cover) -> (false, cover)
  in
  let time_end = Unix.gettimeofday () in
  F.asprintf
    "[Fuel %d] [Phantom %d] [Derivation %d] [Mutation %d] Evaluated %s (took \
     %.2f)"
    fuel pid idx_derivation idx_mutation filename_gen_p4 (time_end -. time_start)
  |> Config.log config;
  (* Find newly hit or newly close-missing nodes *)
  let pids_hit_new, pids_close_miss_new =
    Config.find_interesting_cover_seed config cover
  in
  (* Collect the file if it covers a new phantom, and update the running coverage *)
  if not (PIdSet.is_empty pids_hit_new) then
    update_hit_new fuel pid idx_derivation idx_mutation config filename_gen_p4
      welltyped pids_hit_new;
  (* Collect the file if it is well-typed and covers a new close-miss phantom,
     then update the running coverage *)
  if welltyped && not (PIdSet.is_empty pids_close_miss_new) then
    update_close_miss_new fuel pid idx_derivation idx_mutation config
      filename_gen_p4 pids_close_miss_new

(* Fuzzing from derivations *)

let fuzz_derivation_mutated (fuel : int) (pid : pid) (idx_derivation : int)
    (idx_mutation : int) (config : Config.t) (dirname_gen_tmp : string)
    (filename_p4 : string) (comment_gen_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (value_source : value) (value_mutated : value) : unit =
  (* Reassemble the program with the mutated AST *)
  let renamer = VIdMap.singleton value_source.note.vid value_mutated in
  let value_program = Dep.Graph.reassemble_node graph renamer vid_program in
  (* Mutation may yield a syntactically ill-formed AST, so have a try block *)
  try
    let program = Interp_sl.Out.out_program value_program in
    let filename_gen_p4 =
      F.asprintf "%s/%s_F%d_P%d_D%d_M%d.p4" dirname_gen_tmp
        (Filesys.base ~suffix:".p4" filename_p4)
        fuel pid idx_derivation idx_mutation
    in
    let comment_gen_p4 =
      F.asprintf "%s\n/*\nFrom %s\nTo %s\n*/\n" comment_gen_p4
        (Sl.Print.string_of_value value_source)
        (Sl.Print.string_of_value value_mutated)
    in
    (* Write the mutated program to a file *)
    let oc = open_out filename_gen_p4 in
    F.asprintf "%s\n%a\n" comment_gen_p4 P4el.Pp.pp_program program
    |> output_string oc;
    close_out oc;
    (* Check if the mutated program is interesting, and if so, update *)
    update_interesting fuel pid idx_derivation idx_mutation config
      filename_gen_p4 value_program
  with Error (_, msg) -> Config.warn config msg

let fuzz_derivation (fuel : int) (pid : pid) (idx_derivation : int)
    (trials_derivation : int ref) (config : Config.t) (dirname_gen_tmp : string)
    (filename_p4 : string) (comment_gen_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (vid_source : vid) : unit =
  (* Reassemble the close-AST *)
  let value_source = Dep.Graph.reassemble_node graph VIdMap.empty vid_source in
  F.asprintf "[Fuel %d] [Phantom %d] [Derivation %d]\n[File] %s\n[Source] %s\n"
    fuel pid idx_derivation filename_p4
    (Sl.Print.string_of_value value_source)
  |> Config.query config;
  (* Mutate the close-AST *)
  let mutations =
    Mutate.mutates Config.mutation_trials config.specenv.tdenv value_source
  in
  (* Generate the mutated program *)
  List.iteri
    (fun idx_mutation (kind, value_mutated) ->
      if
        !trials_derivation < Config.trials_derivation
        && MCov.is_miss config.seed.cover_seed pid
      then (
        trials_derivation := !trials_derivation + 1;
        F.asprintf "[Mutated] %s\n" (Sl.Print.string_of_value value_mutated)
        |> Config.answer config;
        let comment_gen_p4 =
          F.asprintf "%s\n// Mutation %s\n" comment_gen_p4
            (Mutate.string_of_kind kind)
        in
        fuzz_derivation_mutated fuel pid idx_derivation idx_mutation config
          dirname_gen_tmp filename_p4 comment_gen_p4 graph vid_program
          value_source value_mutated))
    mutations

let fuzz_derivations (fuel : int) (pid : pid) (trials_derivation : int ref)
    (config : Config.t) (dirname_gen_tmp : string) (filename_p4 : string)
    (graph : Dep.Graph.t) (vid_program : vid)
    (derivations_source : (vid * int) list) : unit =
  List.iteri
    (fun idx_derivation (vid_source, depth) ->
      if
        !trials_derivation < Config.trials_derivation
        && MCov.is_miss config.seed.cover_seed pid
      then
        let comment_gen_p4 =
          F.asprintf "// Intended pid %d\n// Source vid %d\n// Depth %d\n" pid
            vid_source depth
        in
        fuzz_derivation fuel pid idx_derivation trials_derivation config
          dirname_gen_tmp filename_p4 comment_gen_p4 graph vid_program
          vid_source)
    derivations_source

let fuzz_derivations_bounded (fuel : int) (pid : pid) (config : Config.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (derivations_source : (vid * int) list) : unit =
  if derivations_source = [] then
    F.asprintf "[Fuel %d] [Phantom %d] Skipping, no derivation found" fuel pid
    |> Config.log config
  else
    let derivations_total = List.length derivations_source in
    F.asprintf
      "[Fuel %d] [Phantom %d] Fuzzing from %d derivations, until %d trials" fuel
      pid derivations_total Config.trials_derivation
    |> Config.log config;
    let trials_derivation = ref 0 in
    while
      !trials_derivation < Config.trials_derivation
      && MCov.is_miss config.seed.cover_seed pid
    do
      fuzz_derivations fuel pid trials_derivation config dirname_gen_tmp
        filename_p4 graph vid_program derivations_source;
      F.asprintf "[Fuel %d] [Phantom %d] Fuzzed %d trials" fuel pid
        !trials_derivation
      |> Config.log config
    done

(* Fuzzing from a seed program *)

let rec fuzz_seed (fuel : int) (pid : pid) (config : Config.t)
    (dirname_gen_tmp : string) (filename_p4 : string) : unit =
  (* Run SL interpreter on the program,
     and if it is well-typed, start generating tests from it *)
  let time_start = Unix.gettimeofday () in
  F.asprintf "[Fuel %d] [Phantom %d] Running SL interpreter on %s" fuel pid
    filename_p4
  |> Config.log config;
  match
    Interp_sl.Interp.run_typing ~derive:true config.specenv.spec
      config.specenv.includes_p4 filename_p4
  with
  | Well (graph, vid_program, cover) ->
      let time_end = Unix.gettimeofday () in
      F.asprintf
        "[Fuel %d] [Phantom %d] SL interpreter succeeded on %s (took %.2f)" fuel
        pid filename_p4 (time_end -. time_start)
      |> Config.log config;
      let graph = Option.get graph in
      let vid_program = Option.get vid_program in
      fuzz_seed' fuel pid config dirname_gen_tmp filename_p4 graph vid_program
        cover
  | Ill _ ->
      F.asprintf "[Fuel %d] [Phantom %d] SL interpreter failed on %s" fuel pid
        filename_p4
      |> Config.log config

and fuzz_seed' (fuel : int) (pid : pid) (config : Config.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (cover : SCov.Cover.t) : unit =
  (* Derive closes-ASTs from the phantom *)
  F.asprintf "[Fuel %d] [Phantom %d] Finding derivations from %s" fuel pid
    filename_p4
  |> Config.log config;
  let time_start = Unix.gettimeofday () in
  let derivations_source = derive_phantom pid graph cover in
  let time_end = Unix.gettimeofday () in
  (* Take top ranked derivations, i.e., the ones with the smallest depth *)
  F.asprintf
    "[Fuel %d] [Phantom %d] Found total %d derivations, sampling top %d (took \
     %.2f)"
    fuel pid
    (List.length derivations_source)
    Config.derivation_source_samples (time_end -. time_start)
  |> Config.log config;
  let derivations_source =
    if List.length derivations_source < Config.derivation_source_samples then
      derivations_source
    else
      List.init Config.derivation_source_samples (List.nth derivations_source)
  in
  (* Mutate the close-ASTs and dump to file *)
  fuzz_derivations_bounded fuel pid config dirname_gen_tmp filename_p4 graph
    vid_program derivations_source

let fuzz_seeds (fuel : int) (pid : pid) (config : Config.t)
    (dirname_gen_tmp : string) (filenames_p4 : string list) : unit =
  (* Fuzz from seed programs until the target phantom node is covered,
     for overall trials, have a hard timeout *)
  List.iter
    (fun filename_p4 ->
      if MCov.is_miss config.seed.cover_seed pid then
        fuzz_seed fuel pid config dirname_gen_tmp filename_p4)
    filenames_p4

(* Fuzzing from a target phantom node *)

let fuzz_phantom (fuel : int) (pid : pid) (config : Config.t)
    (filenames_p4 : string list) : unit =
  F.asprintf "[Fuel %d] [Phantom %d] Targeting phantom %d" fuel pid pid
  |> Config.log config;
  (* Create a directory for the generated programs *)
  let dirname_gen_tmp =
    config.outdirs.dirname_gen ^ "/fuel" ^ string_of_int fuel ^ "phantom"
    ^ string_of_int pid
  in
  Filesys.mkdir dirname_gen_tmp;
  (* Randomly sample N close-miss filenames *)
  let filenames_p4 =
    Rand.random_sample Config.close_miss_samples filenames_p4
  in
  (* Generate tests from the files *)
  fuzz_seeds fuel pid config dirname_gen_tmp filenames_p4;
  (* Remove the directory for the generated programs *)
  Filesys.rmdir dirname_gen_tmp

let fuzz_phantoms (fuel : int) (config : Config.t) : unit =
  let pids = MCov.Cover.dom config.seed.cover_seed in
  PIdSet.iter
    (fun pid ->
      let branch = MCov.Cover.find pid config.seed.cover_seed in
      match branch.status with
      | Hit -> ()
      | Miss filenames_p4 -> fuzz_phantom fuel pid config filenames_p4)
    pids

(* Fuzzing in a loop with fuel *)

let rec fuzz_loop (fuel : int) (config : Config.t) : Config.t =
  if fuel = 0 then config
  else (
    F.asprintf "[Fuel %d] Start fuzzing loop" fuel |> Config.log config;
    fuzz_phantoms fuel config;
    let total, hits, coverage = MCov.coverage config.seed.cover_seed in
    F.asprintf "[Fuel %d] End fuzzing loop with coverage %d/%d (%.2f%%)" fuel
      hits total coverage
    |> Config.log config;
    Config.set_rand config;
    fuzz_loop (fuel - 1) config)

(* Entry point to main fuzzing loop *)

type bootmode = Cold | Warm of string
type targetmode = Roundrobin | Target of string

let fuzz_typing_init ?(silent : bool = false) (spec : spec)
    (includes_p4 : string list) (filenames_seed_p4 : string list)
    (dirname_gen : string) (bootmode : bootmode) (targetmode : targetmode) :
    Config.t =
  (* Create a timestamp *)
  let timestamp =
    let tm = Unix.gettimeofday () |> Unix.localtime in
    F.asprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
  in
  (* Create directories for generated programs *)
  let dirname_gen = dirname_gen ^ "/fuzz-" ^ timestamp in
  let outdirs = Config.init_outdirs dirname_gen in
  (* Create a logger *)
  let logname = dirname_gen ^ "/fuzz.log" in
  let logger = Logger.init ~silent logname in
  (* Create a query *)
  let queryname = dirname_gen ^ "/fuzz.query" in
  let queries = Query.init queryname in
  (* Create a spec environment *)
  Logger.log logger "Loading type definitions from the spec file";
  let specenv = Config.init_specenv spec includes_p4 in
  (* Create a seed *)
  Logger.log logger "Booting initial coverage";
  let cover_seed =
    match bootmode with
    | Cold ->
        let cover_seed = Boot.boot_cold spec includes_p4 filenames_seed_p4 in
        (* Log the initial coverage for later use in warm boot *)
        let filename_cov = dirname_gen ^ "/boot.coverage" in
        MCov.log ~filename_cov_opt:(Some filename_cov) cover_seed;
        cover_seed
    | Warm filename_boot -> Boot.boot_warm filename_boot
  in
  let cover_seed =
    match targetmode with
    | Roundrobin -> cover_seed
    | Target filename_target ->
        let targets = Target.target_phantom filename_target in
        let cover_seed = MCov.target cover_seed targets in
        (* Log the targeted coverage for later use in warm boot *)
        let filename_cov_target = dirname_gen ^ "/target.coverage" in
        MCov.log ~filename_cov_opt:(Some filename_cov_target) cover_seed;
        cover_seed
  in
  let total, hits, coverage = MCov.coverage cover_seed in
  F.asprintf "Finished booting with initial coverage %d/%d (%.2f%%)" hits total
    coverage
  |> Logger.log logger;
  let seed = Config.init_seed filenames_seed_p4 cover_seed in
  (* Create a configuration *)
  let config = Config.init logger queries specenv outdirs seed in
  config

let fuzz_typing ?(silent : bool = false) (fuel : int) (spec : spec)
    (includes_p4 : string list) (filenames_seed_p4 : string list)
    (dirname_gen : string) (bootmode : bootmode) (targetmode : targetmode) :
    unit =
  let config =
    fuzz_typing_init ~silent spec includes_p4 filenames_seed_p4 dirname_gen
      bootmode targetmode
  in
  let config = fuzz_loop fuel config in
  Config.close config
