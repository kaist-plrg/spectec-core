open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module F = Format
open Util.Source

(* Timeout exception for the fuzzing loop *)

exception Timeout

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

(* Check if the mutated file is interesting,
   and if so, copy it to the output directory *)

let find_interesting (config : Config.t) (cover : SCov.Cover.t) :
    PIdSet.t * PIdSet.t =
  MCov.Cover.fold
    (fun pid (branch_fuzz : MCov.Branch.t) (pids_hit_new, pids_close_miss_new) ->
      let branch_single = SCov.Cover.find pid cover in
      match (branch_single.status, branch_fuzz.status) with
      (* Hits a new phantom *)
      | Hit, Miss _ ->
          let pids_hit_new = PIdSet.add pid pids_hit_new in
          (pids_hit_new, pids_close_miss_new)
      (* Adds a new close-miss *)
      | Miss (_ :: _), Miss [] ->
          let pids_close_miss_new = PIdSet.add pid pids_close_miss_new in
          (pids_hit_new, pids_close_miss_new)
      | _ -> (pids_hit_new, pids_close_miss_new))
    config.seed.cover
    (PIdSet.empty, PIdSet.empty)

let update_hit_new' (fuel : int) (pid : pid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (config : Config.t) (log : Logger.t) (filename_hit_p4 : string)
    (kind : Mutate.kind) (welltyped : bool) (pids_hit_new : PIdSet.t) : unit =
  F.asprintf
    "[F %d] [P %d] [S %d] [%s %d] [M %d] %s hits %s (COUNT %d) (%s) (%s)" fuel
    pid idx_seed strategy idx_method idx_mutation filename_hit_p4
    (PIdSet.to_string pids_hit_new)
    (PIdSet.cardinal pids_hit_new)
    (Mutate.string_of_kind kind)
    (if PIdSet.mem pid pids_hit_new then "GOODHIT" else "BADHIT")
  |> Logger.mark config.modes.logmode log;
  let oc = open_out_gen [ Open_append; Open_text ] 0o666 filename_hit_p4 in
  F.asprintf "\n// Covered pids %s\n" (PIdSet.to_string pids_hit_new)
  |> output_string oc;
  close_out oc;
  (* Update the set of covered phantoms *)
  Config.update_hit_seed config filename_hit_p4 welltyped pids_hit_new

let update_hit_new (fuel : int) (pid : pid) (idx_seed : int) (strategy : string)
    (idx_method : int) (idx_mutation : int) (config : Config.t) (log : Logger.t)
    (filename_gen_p4 : string) (kind : Mutate.kind) (pids_hit_new : PIdSet.t) :
    unit =
  (* Re-run the SL interpreter to make sure of the new hits *)
  (* Then copy the interesting test program to the output directory
     and update the running coverage *)
  match
    Interp_sl.Typing.run_typing' ~mini:config.mini ~derive:false
      config.specenv.spec config.specenv.includes_p4 filename_gen_p4
      config.specenv.ignores
  with
  | WellTyped (_, _, cover) when PIdSet.for_all (SCov.is_hit cover) pids_hit_new
    ->
      let filename_hit_p4 =
        Filesys.cp filename_gen_p4 config.storage.dirname_welltyped_p4
      in
      update_hit_new' fuel pid idx_seed strategy idx_method idx_mutation config
        log filename_hit_p4 kind true pids_hit_new
  | IllTyped (_, _, cover) when PIdSet.for_all (SCov.is_hit cover) pids_hit_new
    ->
      let filename_hit_p4 =
        Filesys.cp filename_gen_p4 config.storage.dirname_illtyped_p4
      in
      update_hit_new' fuel pid idx_seed strategy idx_method idx_mutation config
        log filename_hit_p4 kind false pids_hit_new
  | _ -> ()

let update_close_miss_new' (fuel : int) (pid : pid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (config : Config.t) (log : Logger.t) (filename_close_miss_p4 : string)
    (pids_close_miss_new : PIdSet.t) : unit =
  F.asprintf "[F %d] [P %d] [S %d] [%s %d] [M %d] %s close-misses %s" fuel pid
    idx_seed strategy idx_method idx_mutation filename_close_miss_p4
    (PIdSet.to_string pids_close_miss_new)
  |> Logger.log config.modes.logmode log;
  let oc =
    open_out_gen [ Open_append; Open_text ] 0o666 filename_close_miss_p4
  in
  F.asprintf "\n// Close-missed pids %s\n"
    (PIdSet.to_string pids_close_miss_new)
  |> output_string oc;
  close_out oc;
  (* Update the set of covered phantoms *)
  Config.update_close_miss_seed config filename_close_miss_p4
    pids_close_miss_new

let update_close_miss_new (fuel : int) (pid : pid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (config : Config.t) (log : Logger.t) (filename_gen_p4 : string)
    (pids_close_miss_new : PIdSet.t) : unit =
  (* Re-run the SL interpreter to make sure of the new close-misses *)
  (* Then copy the interesting test program to the output directory,
     and update the running coverage *)
  (* Then copy the interesting test program to the output directory
     and update the running coverage *)
  match
    Interp_sl.Typing.run_typing' ~mini:config.mini ~derive:false
      config.specenv.spec config.specenv.includes_p4 filename_gen_p4
      config.specenv.ignores
  with
  | WellTyped (_, _, cover)
    when PIdSet.for_all (SCov.is_close_miss cover) pids_close_miss_new ->
      let filename_close_miss_p4 =
        Filesys.cp filename_gen_p4 config.storage.dirname_close_miss_p4
      in
      update_close_miss_new' fuel pid idx_seed strategy idx_method idx_mutation
        config log filename_close_miss_p4 pids_close_miss_new
  | _ -> ()

let update_interesting (fuel : int) (pid : pid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (trials : int ref) (config : Config.t) (log : Logger.t)
    (filename_gen_p4 : string) (kind : Mutate.kind) (value_program : value) :
    unit =
  (* Evaluate the generated program to see if it is interesting *)
  let time_start = Unix.gettimeofday () in
  F.asprintf "[F %d] [P %d] [S %d] [%s %d] [M %d] [%d/%d] Evaluating %s" fuel
    pid idx_seed strategy idx_method idx_mutation !trials Config.trials_seed
    filename_gen_p4
  |> Logger.log config.modes.logmode log;
  let welltyped, cover =
    match
      Interp_sl.Typing.run_typing_internal config.specenv.spec filename_gen_p4
        value_program config.specenv.ignores
    with
    | WellTyped (_, _, cover) -> (true, cover)
    | IllTyped (_, _, cover) -> (false, cover)
    | IllFormed (_, cover) -> (false, cover)
  in
  let time_end = Unix.gettimeofday () in
  F.asprintf
    "[F %d] [P %d] [S %d] [%s %d] [M %d] [%d/%d] Evaluated %s (took %.2f)" fuel
    pid idx_seed strategy idx_method idx_mutation !trials Config.trials_seed
    filename_gen_p4 (time_end -. time_start)
  |> Logger.log config.modes.logmode log;
  (* Find newly hit or newly close-missing nodes *)
  let pids_hit_new, pids_close_miss_new = find_interesting config cover in
  (* Collect the file if it covers a new phantom, and update the running coverage
     If in strict mode, we only collect the file if it covers the intended phantom *)
  (match config.modes.covermode with
  | Relaxed ->
      if not (PIdSet.is_empty pids_hit_new) then
        update_hit_new fuel pid idx_seed strategy idx_method idx_mutation config
          log filename_gen_p4 kind pids_hit_new
  | Strict ->
      if PIdSet.mem pid pids_hit_new then
        update_hit_new fuel pid idx_seed strategy idx_method idx_mutation config
          log filename_gen_p4 kind (PIdSet.singleton pid));
  (* Collect the file if it is well-typed and covers a new close-miss phantom,
     then update the running coverage *)
  if welltyped && not (PIdSet.is_empty pids_close_miss_new) then
    update_close_miss_new fuel pid idx_seed strategy idx_method idx_mutation
      config log filename_gen_p4 pids_close_miss_new

(* Mutate an AST and generate a new program *)

let classify_mutation' (fuel : int) (pid : pid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (trials : int ref) (config : Config.t) (log : Logger.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (comment_gen_p4 : string)
    (kind : Mutate.kind) (value_source : value) (value_mutated : value)
    (value_program : value) (program : P4el.Ast.program) : unit =
  let filename_gen_p4 =
    F.asprintf "%s/%s_F%dP%dS%d%s%dM%dT%d.p4" dirname_gen_tmp
      (Filesys.base ~suffix:".p4" filename_p4)
      fuel pid idx_seed
      (if strategy = "Derive" then "D"
       else if strategy = "Random" then "R"
       else "")
      idx_method idx_mutation !trials
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
  update_interesting fuel pid idx_seed strategy idx_method idx_mutation trials
    config log filename_gen_p4 kind value_program

let classify_mutation (fuel : int) (pid : pid) (idx_seed : int)
    (strategy : string) (idx_method : int) (idx_mutation : int)
    (trials : int ref) (config : Config.t) (log : Logger.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (comment_gen_p4 : string)
    (graph : Dep.Graph.t) (vid_program : vid) (kind : Mutate.kind)
    (value_source : value) (value_mutated : value) : unit =
  (* Reassemble the program with the mutated AST *)
  let renamer = VIdMap.singleton value_source.note.vid value_mutated in
  let value_program = Dep.Graph.reassemble_node graph renamer vid_program in
  (* Mutation may yield a syntactically ill-formed AST, so have a try block *)
  try
    let program =
      if config.mini then Interp_sl.Convert.Out_mini.out_program value_program
      else Interp_sl.Convert.Out.out_program value_program
    in
    classify_mutation' fuel pid idx_seed strategy idx_method idx_mutation trials
      config log dirname_gen_tmp filename_p4 comment_gen_p4 kind value_source
      value_mutated value_program program
  with Util.Error.ConvertOutError msg ->
    Logger.warn config.modes.logmode log msg

let fuzz_mutation (fuel : int) (pid : pid) (idx_seed : int) (strategy : string)
    (idx_method : int) (trials : int ref) (config : Config.t) (log : Logger.t)
    (query : Query.t) (dirname_gen_tmp : string) (filename_p4 : string)
    (comment_gen_p4 : string) (graph : Dep.Graph.t) (vid_program : vid)
    (vid_source : vid) : unit =
  (* Reassemble the AST *)
  let value_source = Dep.Graph.reassemble_node graph VIdMap.empty vid_source in
  F.asprintf "[F %d] [P %d] [S %d] [%s %d]\n[File] %s\n[Source] %s\n" fuel pid
    idx_seed strategy idx_method filename_p4
    (Sl.Print.string_of_value value_source)
  |> Query.query query;
  (* Mutate the AST *)
  let mutations =
    Mutate.mutates Config.trials_mutation config.specenv.tdenv
      config.specenv.mixopenv graph vid_program vid_source
  in
  (* Generate the mutated program *)
  List.iteri
    (fun idx_mutation (kind, value_source, value_mutated) ->
      if !trials < Config.trials_seed && MCov.is_miss config.seed.cover pid then (
        trials := !trials + 1;
        F.asprintf "[Mutated] %s\n" (Sl.Print.string_of_value value_mutated)
        |> Query.answer query;
        let comment_gen_p4 =
          F.asprintf "%s\n// Mutation %s\n" comment_gen_p4
            (Mutate.string_of_kind kind)
        in
        classify_mutation fuel pid idx_seed strategy idx_method idx_mutation
          trials config log dirname_gen_tmp filename_p4 comment_gen_p4 graph
          vid_program kind value_source value_mutated))
    mutations

(* Fuzzing from derivations *)

let fuzz_derivations (fuel : int) (pid : pid) (idx_seed : int)
    (trials : int ref) (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (derivations_source : (vid * int) list) : unit =
  List.iteri
    (fun idx_derivation (vid_source, depth) ->
      if !trials < Config.trials_seed && MCov.is_miss config.seed.cover pid then
        let comment_gen_p4 =
          F.asprintf "// Intended pid %d\n// Source vid %d\n// Depth %d\n" pid
            vid_source depth
        in
        let strategy = "Derive" in
        fuzz_mutation fuel pid idx_seed strategy idx_derivation trials config
          log query dirname_gen_tmp filename_p4 comment_gen_p4 graph vid_program
          vid_source)
    derivations_source

let fuzz_derivations_bounded (fuel : int) (pid : pid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (derivations_source : (vid * int) list) : unit =
  if derivations_source = [] then
    F.asprintf "[F %d] [P %d] [S %d] Skipping, no derivation found" fuel pid
      idx_seed
    |> Logger.log config.modes.logmode log
  else
    let derivations_total = List.length derivations_source in
    F.asprintf
      "[F %d] [P %d] [S %d] Fuzzing from %d derivations, until %d trials" fuel
      pid idx_seed derivations_total Config.trials_seed
    |> Logger.log config.modes.logmode log;
    let trials = ref 0 in
    while !trials < Config.trials_seed && MCov.is_miss config.seed.cover pid do
      fuzz_derivations fuel pid idx_seed trials config log query dirname_gen_tmp
        filename_p4 graph vid_program derivations_source
    done

(* Fuzzing from a random value id *)

let fuzz_randoms (fuel : int) (pid : pid) (idx_seed : int) (trials : int ref)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (vids_source : vid list) : unit =
  List.iteri
    (fun idx_random vid_source ->
      if !trials < Config.trials_seed && MCov.is_miss config.seed.cover pid then
        let comment_gen_p4 =
          F.asprintf "// Intended pid %d\n// Source vid %d\n" pid vid_source
        in
        let strategy = "Random" in
        fuzz_mutation fuel pid idx_seed strategy idx_random trials config log
          query dirname_gen_tmp filename_p4 comment_gen_p4 graph vid_program
          vid_source)
    vids_source

let fuzz_randoms_bounded (fuel : int) (pid : pid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (vids_source : vid list) : unit =
  F.asprintf
    "[F %d] [P %d] [S %d] Fuzzing from %d random values, until %d trials" fuel
    pid idx_seed (List.length vids_source) Config.trials_seed
  |> Logger.log config.modes.logmode log;
  let trials = ref 0 in
  while !trials < Config.trials_seed && MCov.is_miss config.seed.cover pid do
    fuzz_randoms fuel pid idx_seed trials config log query dirname_gen_tmp
      filename_p4 graph vid_program vids_source
  done

(* Fuzzing from a seed program *)

let fuzz_seed_random (fuel : int) (pid : pid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) : unit =
  (* Randomly sample N vids from the program *)
  let vids_source =
    List.init vid_program Fun.id
    |> Rand.random_sample Config.samples_related_vid
  in
  (* Mutate the ASTs and dump to file *)
  fuzz_randoms_bounded fuel pid idx_seed config log query dirname_gen_tmp
    filename_p4 graph vid_program vids_source

let fuzz_seed_deriving (fuel : int) (pid : pid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (cover : SCov.Cover.t) : unit =
  (* Derive closes-ASTs from the phantom *)
  F.asprintf "[F %d] [P %d] [S %d] Finding derivations from %s" fuel pid
    idx_seed filename_p4
  |> Logger.log config.modes.logmode log;
  let time_start = Unix.gettimeofday () in
  let derivations_source = Derive.derive_phantom pid graph cover in
  let time_end = Unix.gettimeofday () in
  (* Take top ranked derivations, i.e., the ones with the smallest depth *)
  F.asprintf
    "[F %d] [P %d] [S %d] Found total %d derivations, sampling top %d (took \
     %.2f)"
    fuel pid idx_seed
    (List.length derivations_source)
    Config.samples_derivation_source (time_end -. time_start)
  |> Logger.log config.modes.logmode log;
  let derivations_source =
    if List.length derivations_source < Config.samples_derivation_source then
      derivations_source
    else
      List.init Config.samples_derivation_source (List.nth derivations_source)
  in
  (* Mutate the close-ASTs and dump to file *)
  fuzz_derivations_bounded fuel pid idx_seed config log query dirname_gen_tmp
    filename_p4 graph vid_program derivations_source

let fuzz_seed_hybrid (fuel : int) (pid : pid) (idx_seed : int)
    (config : Config.t) (log : Logger.t) (query : Query.t)
    (dirname_gen_tmp : string) (filename_p4 : string) (graph : Dep.Graph.t)
    (vid_program : vid) (cover : SCov.Cover.t) : unit =
  (* Derive closes-ASTs from the phantom *)
  F.asprintf "[F %d] [P %d] [S %d] Finding derivations from %s" fuel pid
    idx_seed filename_p4
  |> Logger.log config.modes.logmode log;
  let time_start = Unix.gettimeofday () in
  let derivations_source = Derive.derive_phantom pid graph cover in
  let time_end = Unix.gettimeofday () in
  (* Take top ranked derivations, i.e., the ones with the smallest depth *)
  F.asprintf
    "[F %d] [P %d] [S %d] Found total %d derivations, sampling top %d (took \
     %.2f)"
    fuel pid idx_seed
    (List.length derivations_source)
    Config.samples_derivation_source (time_end -. time_start)
  |> Logger.log config.modes.logmode log;
  let derivations_source =
    if List.length derivations_source < Config.samples_derivation_source then
      derivations_source
    else
      List.init Config.samples_derivation_source (List.nth derivations_source)
  in
  (* If there are no derivations, fallback to random *)
  match derivations_source with
  | [] ->
      fuzz_seed_random fuel pid idx_seed config log query dirname_gen_tmp
        filename_p4 graph vid_program
  | _ ->
      fuzz_derivations_bounded fuel pid idx_seed config log query
        dirname_gen_tmp filename_p4 graph vid_program derivations_source

let fuzz_seed (fuel : int) (pid : pid) (idx_seed : int) (config : Config.t)
    (log : Logger.t) (query : Query.t) (dirname_gen_tmp : string)
    (filename_p4 : string) : unit =
  let time_start = Unix.gettimeofday () in
  F.asprintf "[F %d] [P %d] [S %d] Running SL interpreter on %s" fuel pid
    idx_seed filename_p4
  |> Logger.log config.modes.logmode log;
  (* Construct the value dependency graph for deriving and hybrid modes *)
  let derive =
    match config.modes.mutationmode with
    | Random -> false
    | Derive -> true
    | Hybrid -> true
  in
  (* Run SL interpreter on the program,
     and if it is well-typed, start generating tests from it *)
  (match
     Interp_sl.Typing.run_typing' ~mini:config.mini ~derive config.specenv.spec
       config.specenv.includes_p4 filename_p4 config.specenv.ignores
   with
  | WellTyped (graph, vid_program, cover) ->
      let time_end = Unix.gettimeofday () in
      F.asprintf
        "[F %d] [P %d] [S %d] SL interpreter succeeded on %s (took %.2f)" fuel
        pid idx_seed filename_p4 (time_end -. time_start)
      |> Logger.log config.modes.logmode log;
      (match config.modes.mutationmode with
      | Random ->
          fuzz_seed_random fuel pid idx_seed config log query dirname_gen_tmp
            filename_p4 graph vid_program
      | Derive ->
          fuzz_seed_deriving fuel pid idx_seed config log query dirname_gen_tmp
            filename_p4 graph vid_program cover
      | Hybrid ->
          fuzz_seed_hybrid fuel pid idx_seed config log query dirname_gen_tmp
            filename_p4 graph vid_program cover);
      Dep.Graph.G.reset graph.nodes;
      Dep.Graph.G.reset graph.edges
  | IllTyped _ | IllFormed _ ->
      F.asprintf "[F %d] [P %d] [S %d] SL interpreter failed on %s" fuel pid
        idx_seed filename_p4
      |> Logger.log config.modes.logmode log);
  let total, hits, coverage = MCov.measure_coverage config.seed.cover in
  F.asprintf "[F %d] [P %d] [S %d] Coverage %d/%d (%.2f%%)" fuel pid idx_seed
    hits total coverage
  |> Logger.log config.modes.logmode log

let fuzz_seeds (fuel : int) (pid : pid) (config : Config.t) (log : Logger.t)
    (query : Query.t) (dirname_gen_tmp : string) (filenames_p4 : string list) :
    unit =
  (* Fuzz from seed programs until the target phantom node is covered *)
  List.iteri
    (fun idx_seed filename_p4 ->
      if MCov.is_miss config.seed.cover pid then (
        let _ =
          Sys.set_signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> raise Timeout))
        in
        Unix.alarm Config.timeout_seed |> ignore;
        (try
           fuzz_seed fuel pid idx_seed config log query dirname_gen_tmp
             filename_p4
         with Timeout ->
           F.asprintf "[F %d] [S %d] [P %d] Timeout on %s" fuel pid idx_seed
             filename_p4
           |> Logger.warn config.modes.logmode log);
        Unix.alarm 0 |> ignore))
    filenames_p4

(* Fuzzing from a target phantom node *)

let fuzz_phantom (fuel : int) (pid : pid) (config : Config.t) (log : Logger.t)
    (query : Query.t) (filenames_p4 : string list) : unit =
  F.asprintf "[F %d] [P %d] Targeting phantom %d" fuel pid pid
  |> Logger.log config.modes.logmode log;
  (* Create a directory for the generated programs *)
  let dirname_gen_tmp =
    config.storage.dirname_gen ^ "/fuel" ^ string_of_int fuel ^ "phantom"
    ^ string_of_int pid
  in
  Filesys.mkdir dirname_gen_tmp;
  (* Randomly sample N close-miss filenames *)
  let filenames_p4 =
    Rand.random_sample Config.samples_close_miss filenames_p4
  in
  (* Generate tests from the files *)
  (try fuzz_seeds fuel pid config log query dirname_gen_tmp filenames_p4
   with _ ->
     F.asprintf "[F %d] [P %d] Unexpected error occurred" fuel pid
     |> Logger.warn config.modes.logmode log);
  (* Remove the directory for the generated programs *)
  Filesys.rmdir dirname_gen_tmp

let fuzz_phantoms (fuel : int) (config : Config.t) (log : Logger.t)
    (query : Query.t) : unit =
  let pids = MCov.Cover.dom config.seed.cover in
  PIdSet.iter
    (fun pid ->
      let branch = MCov.Cover.find pid config.seed.cover in
      match branch.status with
      | Hit _ -> ()
      | Miss [] -> ()
      | Miss filenames_p4 -> fuzz_phantom fuel pid config log query filenames_p4)
    pids

(* Fuzzing in a loop with fuel *)

let rec fuzz_loop (fuel : int) (config : Config.t) : Config.t =
  if fuel = 0 then config
  else
    (* Create a log for the current fuel *)
    let logname = F.asprintf "%s/fuel%d.log" config.storage.dirname_log fuel in
    let log = Logger.init logname in
    (* Create q query for the current fuel *)
    let queryname =
      F.asprintf "%s/fuel%d.query" config.storage.dirname_query fuel
    in
    let query = Query.init queryname in
    (* Fuzz single iteration *)
    F.asprintf "[F %d] Start fuzzing loop" fuel
    |> Logger.log config.modes.logmode log;
    fuzz_phantoms fuel config log query;
    let total, hits, coverage = MCov.measure_coverage config.seed.cover in
    F.asprintf "[F %d] End fuzzing loop with coverage %d/%d (%.2f%%)" fuel hits
      total coverage
    |> Logger.log config.modes.logmode log;
    (* Close the logger *)
    Logger.close log;
    (* Close the query *)
    Query.close query;
    (* Proceed to the next fuel level *)
    fuzz_loop (fuel - 1) config

(* Entry point to main fuzzing loop *)

let fuzz_typing_init ?(mini : bool = false) (spec : spec)
    (includes_p4 : string list) (filenames_ignore : string list)
    (dirname_gen : string) (name_campaign : string option)
    (randseed : int option) (logmode : Modes.logmode)
    (bootmode : Modes.bootmode) (mutationmode : Modes.mutationmode)
    (covermode : Modes.covermode) : Config.t =
  (* Name the campaign *)
  let name_campaign =
    match name_campaign with
    | Some name_campaign -> name_campaign
    | None ->
        let timestamp =
          let tm = Unix.gettimeofday () |> Unix.localtime in
          F.asprintf "%04d-%02d-%02d-%02d-%02d-%02d" (tm.Unix.tm_year + 1900)
            (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
            tm.Unix.tm_sec
        in
        "fuzz-" ^ timestamp
  in
  (* Create directories for storage *)
  let dirname_gen = dirname_gen ^ "/" ^ name_campaign in
  let storage = Config.init_storage dirname_gen in
  (* Create a mode *)
  let modes = Modes.{ bootmode; logmode; mutationmode; covermode } in
  (* Create a initializer log *)
  let logname_init = storage.dirname_log ^ "/init.log" in
  let log_init = Logger.init logname_init in
  (* Log the command line arguments *)
  F.asprintf "[COMMAND] testgen -gen %s%s%s%s" dirname_gen
    (match modes.bootmode with
    | Cold (excludes_p4, dirname_seed_p4) ->
        "-e" ^ String.concat " " excludes_p4 ^ "-cold " ^ dirname_seed_p4
    | Warm filename_boot -> " -warm " ^ filename_boot)
    (match modes.mutationmode with
    | Random -> " -random"
    | Derive -> ""
    | Hybrid -> " -hybrid")
    (match modes.covermode with Strict -> " -strict" | Relaxed -> "")
  |> Logger.log modes.logmode log_init;
  (* Create a spec environment *)
  "Loading type definitions from the spec file"
  |> Logger.log modes.logmode log_init;
  let specenv = Config.init_specenv spec includes_p4 filenames_ignore in
  (* Create a seed *)
  "Booting initial coverage" |> Logger.log modes.logmode log_init;
  let cover_seed =
    match modes.bootmode with
    | Cold (excludes_p4, dirname_seed_p4) ->
        let cover_seed =
          Boot.boot_cold ~mini spec includes_p4 excludes_p4 dirname_seed_p4
            filenames_ignore
        in
        (* Log the initial coverage for later use in warm boot *)
        let filename_cov = dirname_gen ^ "/boot.coverage" in
        MCov.log ~filename_cov_opt:(Some filename_cov) cover_seed;
        cover_seed
    | Warm filename_boot -> Boot.boot_warm filename_boot
  in
  let seed = Config.init_seed cover_seed in
  (* Close the initial log *)
  let total, hits, coverage = MCov.measure_coverage cover_seed in
  F.asprintf "Finished booting with initial coverage %d/%d (%.2f%%)" hits total
    coverage
  |> Logger.log modes.logmode log_init;
  F.asprintf
    "[SAMPLES_CLOSE_MISS] %d [SAMPLES_RELATED_VID] %d \
     [SAMPLES_DERIVATION_SOURCE] %d [TRIALS_MUTATION] %d [TRIALS_SEED] %d \
     [TIMEOUT_SEED] %d"
    Config.samples_close_miss Config.samples_related_vid
    Config.samples_derivation_source Config.trials_mutation Config.trials_seed
    Config.timeout_seed
  |> Logger.log modes.logmode log_init;
  Logger.close log_init;
  (* Create a configuration *)
  let config = Config.init ~mini randseed modes specenv storage seed in
  config

let fuzz_typing ?(mini : bool = false) (fuel : int) (spec : spec)
    (includes_p4 : string list) (filenames_ignore : string list)
    (dirname_gen : string) (name_campaign : string option)
    (randseed : int option) (logmode : Modes.logmode)
    (bootmode : Modes.bootmode) (mutationmode : Modes.mutationmode)
    (covermode : Modes.covermode) : unit =
  (* Initialize the fuzzing configuration *)
  let config =
    fuzz_typing_init ~mini spec includes_p4 filenames_ignore dirname_gen
      name_campaign randseed logmode bootmode mutationmode covermode
  in
  (* Call the main fuzzing loop *)
  let config = fuzz_loop fuel config in
  (* Log the final coverage *)
  let filename_cov = config.storage.dirname_gen ^ "/final.coverage" in
  MCov.log ~filename_cov_opt:(Some filename_cov) config.seed.cover
