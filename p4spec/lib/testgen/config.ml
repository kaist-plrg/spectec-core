open Domain.Lib
open Sl.Ast
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple

(* Hyperparameters for the fuzzing loop *)

(* Max number of seeds per phantom *)
let samples_close_miss = 3

(* Max number of related vids to derive from per seed *)
let samples_related_vid = 3

(* Max number of close-ASTs per seed *)
let samples_derivation_source = 5

(* Max number of mutation trials per close-AST *)
let trials_mutation = 5

(* Trials per seed *)
let trials_seed = 25

(* Timeout per seed *)
let timeout_seed = 30

(* Environment for the spec *)

type specenv = { spec : spec; tdenv : TDEnv.t; includes_p4 : string list }

(* Storage for generated files *)

type storage = {
  dirname_gen : string;
  dirname_log : string;
  dirname_query : string;
  dirname_close_miss_p4 : string;
  dirname_welltyped_p4 : string;
  dirname_illtyped_p4 : string;
  dirname_illformed_p4 : string;
}

(* Seed programs for mutation *)

type seed = {
  mutable filenames_seed_p4 : string list;
  mutable cover_seed : MCov.Cover.t;
}

(* Configuration for the fuzz campaign *)

type t = {
  mutable rand : int;
  modes : Modes.t;
  specenv : specenv;
  storage : storage;
  seed : seed;
}

(* Load type definitions into environment *)

let load_def (tdenv : TDEnv.t) (def : def) : TDEnv.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      TDEnv.add id typdef tdenv
  | _ -> tdenv

let load_spec (tdenv : TDEnv.t) (spec : spec) : TDEnv.t =
  List.fold_left load_def tdenv spec

(* Changing random seed *)

let set_rand (config : t) : unit =
  config.rand <- config.rand + 1;
  Random.init config.rand

(* Constructor *)

let init_specenv (spec : spec) (includes_p4 : string list) : specenv =
  let tdenv = load_spec TDEnv.empty spec in
  { spec; tdenv; includes_p4 }

let init_storage (dirname_gen : string) : storage =
  Filesys.mkdir dirname_gen;
  let dirname_log = dirname_gen ^ "/log" in
  Filesys.mkdir dirname_log;
  let dirname_query = dirname_gen ^ "/query" in
  Filesys.mkdir dirname_query;
  let dirname_close_miss_p4 = dirname_gen ^ "/closemiss" in
  Filesys.mkdir dirname_close_miss_p4;
  let dirname_welltyped_p4 = dirname_gen ^ "/welltyped" in
  Filesys.mkdir dirname_welltyped_p4;
  let dirname_illtyped_p4 = dirname_gen ^ "/illtyped" in
  Filesys.mkdir dirname_illtyped_p4;
  let dirname_illformed_p4 = dirname_gen ^ "/illformed" in
  Filesys.mkdir dirname_illformed_p4;
  {
    dirname_gen;
    dirname_log;
    dirname_query;
    dirname_close_miss_p4;
    dirname_welltyped_p4;
    dirname_illtyped_p4;
    dirname_illformed_p4;
  }

let init_seed (filenames_seed_p4 : string list) (cover_seed : MCov.Cover.t) :
    seed =
  { filenames_seed_p4; cover_seed }

let init (modes : Modes.t) (specenv : specenv) (storage : storage) (seed : seed)
    =
  let rand = 2025 in
  Random.init rand;
  { rand; modes; specenv; storage; seed }

(* Seed updater *)

let find_interesting_cover_seed (config : t) (cover : SCov.Cover.t) :
    PIdSet.t * PIdSet.t =
  MCov.Cover.fold
    (fun pid (branch_multi : MCov.Branch.t) (pids_hit_new, pids_close_miss_new) ->
      let branch_single = SCov.Cover.find pid cover in
      match (branch_single.status, branch_multi.status) with
      | _, Hit -> (pids_hit_new, pids_close_miss_new)
      | Hit, Miss _ ->
          let pids_hit_new = PIdSet.add pid pids_hit_new in
          (pids_hit_new, pids_close_miss_new)
      | Miss vids, Miss filenames_p4 -> (
          match (vids, filenames_p4) with
          | _ :: _, [] ->
              let pids_close_miss_new = PIdSet.add pid pids_close_miss_new in
              (pids_hit_new, pids_close_miss_new)
          | _ -> (pids_hit_new, pids_close_miss_new)))
    config.seed.cover_seed
    (PIdSet.empty, PIdSet.empty)

let update_hit_cover_seed (config : t) (pids_hit : PIdSet.t) : unit =
  let cover_seed = config.seed.cover_seed in
  let cover_seed =
    PIdSet.fold
      (fun pid_hit cover_seed ->
        let branch = MCov.Cover.find pid_hit cover_seed in
        let branch = MCov.Branch.{ branch with status = Hit } in
        MCov.Cover.add pid_hit branch cover_seed)
      pids_hit cover_seed
  in
  config.seed.cover_seed <- cover_seed

let update_close_miss_cover_seed (config : t) (filename_p4 : string)
    (pids_close_miss : PIdSet.t) : unit =
  let cover_seed = config.seed.cover_seed in
  let cover_seed =
    PIdSet.fold
      (fun pid_close_miss cover_seed ->
        let branch = MCov.Cover.find pid_close_miss cover_seed in
        let branch =
          MCov.Branch.{ branch with status = Miss [ filename_p4 ] }
        in
        MCov.Cover.add pid_close_miss branch cover_seed)
      pids_close_miss cover_seed
  in
  config.seed.cover_seed <- cover_seed
