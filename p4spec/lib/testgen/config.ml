open Domain.Lib
open Xl
open Sl.Ast
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
module Ignore = Runtime_testgen.Cov.Ignore
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple

(* Hyperparameters for the fuzzing loop *)

(* Max number of seeds per phantom *)
let samples_close_miss = 3

(* Max number of related vids to derive from per seed *)
let samples_related_vid = 3

(* Max number of close-ASTs per seed *)
let samples_derivation_source = 10

(* Max number of mutation trials per close-AST *)
let trials_mutation = 3

(* Trials per seed *)
let trials_seed = 30

(* Timeout per seed *)
let timeout_seed = 30

module MixopSet = Set.Make (struct
  type t = Mixop.t

  let compare = compare
end)

module MixopSetSet = Set.Make (struct
  type t = MixopSet.t

  let compare = compare
end)

module MixopEnv = Map.Make (struct
  type t = string

  let compare = compare
end)

type mixopenv = MixopSetSet.t MixopEnv.t

(* Environment for the spec *)
type specenv = {
  spec : spec;
  tdenv : TDEnv.t;
  mixopenv : mixopenv;
  includes_p4 : string list;
  ignores : IdSet.t;
}

(* Storage for generated files *)

type storage = {
  dirname_gen : string;
  dirname_log : string;
  dirname_query : string;
  dirname_close_miss_p4 : string;
  dirname_welltyped_p4 : string;
  dirname_illtyped_p4 : string;
}

(* Seed for the fuzz campaign *)

type seed = { mutable cover : MCov.Cover.t }

(* Configuration for the fuzz campaign *)

type t = {
  mini : bool;
  rand : int;
  modes : Modes.t;
  specenv : specenv;
  storage : storage;
  seed : seed;
}

(* Load mixop groups into the environment *)

let load_groups (mixopenv : mixopenv) (def : def) : mixopenv =
  match def.it with
  | TypD (id, _, deftyp) -> (
      match deftyp.it with
      | VariantT nottyps ->
          let insert_into_groups (nottyp : nottyp)
              (typed_groups : (typ list * MixopSet.t) list) :
              (typ list * MixopSet.t) list =
            let mixop, typs = nottyp.it in
            let rec aux typed_group = function
              | [] -> (typs, MixopSet.singleton mixop) :: typed_group
              | (typs_found, group) :: rest ->
                  if List.equal Sl.Eq.eq_typ typs typs_found then
                    (typs, MixopSet.add mixop group)
                    :: (List.rev typed_group @ rest)
                  else aux ((typs, group) :: typed_group) rest
            in
            aux [] typed_groups
          in
          let new_typed_groups =
            List.fold_left
              (fun acc nottyp -> insert_into_groups nottyp acc)
              [] nottyps
          in
          let new_typed_groups =
            new_typed_groups
            |> List.filter (fun (_, mixop_set) ->
                   MixopSet.cardinal mixop_set > 1)
          in
          if List.length new_typed_groups = 0 then mixopenv
          else
            let orig_groups =
              try MixopEnv.find id.it mixopenv
              with Not_found -> MixopSetSet.empty
            in
            let groups =
              List.fold_left
                (fun acc (_, mixop_set) -> MixopSetSet.add mixop_set acc)
                orig_groups new_typed_groups
            in
            let mixopenv = mixopenv |> MixopEnv.add id.it groups in
            mixopenv
      | PlainT { it = VarT (id', _); _ } ->
          let mixop_set_set =
            try MixopEnv.find id'.it mixopenv
            with Not_found -> MixopSetSet.empty
          in
          let mixopenv = mixopenv |> MixopEnv.add id.it mixop_set_set in
          mixopenv
      | _ -> mixopenv)
  | _ -> mixopenv

(* Load type definitions into the environment *)

let load_def (tdenv : TDEnv.t) (def : def) : TDEnv.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      TDEnv.add id typdef tdenv
  | _ -> tdenv

let load_spec (tdenv : TDEnv.t) (mixopenv : mixopenv) (spec : spec) :
    TDEnv.t * mixopenv =
  let tdenv = List.fold_left load_def tdenv spec in
  let mixopenv = List.fold_left load_groups mixopenv spec in
  (tdenv, mixopenv)

(* Constructor *)

let init_specenv (spec : spec) (includes_p4 : string list)
    (filenames_ignore : string list) : specenv =
  let tdenv, mixopenv = load_spec TDEnv.empty MixopEnv.empty spec in
  let ignores = Ignore.init filenames_ignore in
  { spec; tdenv; mixopenv; includes_p4; ignores }

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
  {
    dirname_gen;
    dirname_log;
    dirname_query;
    dirname_close_miss_p4;
    dirname_welltyped_p4;
    dirname_illtyped_p4;
  }

let init_seed (cover : MCov.Cover.t) : seed = { cover }

let init ?(mini : bool = false) (randseed : int option) (modes : Modes.t)
    (specenv : specenv) (storage : storage) (seed : seed) =
  let rand = Option.value ~default:2025 randseed in
  Random.init rand;
  { mini; rand; modes; specenv; storage; seed }

(* Seed updater *)

let update_hit_seed (config : t) (filename_p4 : string) (welltyped : bool)
    (pids_hit : PIdSet.t) : unit =
  let cover_seed = config.seed.cover in
  let cover_seed =
    PIdSet.fold
      (fun pid_hit cover_seed ->
        let branch : MCov.Branch.t = MCov.Cover.find pid_hit cover_seed in
        let branch =
          match branch.status with
          | Hit (likely, filenames_p4) ->
              let likely = likely && not welltyped in
              let filenames_p4 = filename_p4 :: filenames_p4 in
              MCov.Branch.{ branch with status = Hit (likely, filenames_p4) }
          | _ ->
              let likely = not welltyped in
              let filenames_p4 = [ filename_p4 ] in
              MCov.Branch.{ branch with status = Hit (likely, filenames_p4) }
        in
        MCov.Cover.add pid_hit branch cover_seed)
      pids_hit cover_seed
  in
  config.seed.cover <- cover_seed

let update_close_miss_seed (config : t) (filename_p4 : string)
    (pids_close_miss : PIdSet.t) : unit =
  let cover_seed = config.seed.cover in
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
  config.seed.cover <- cover_seed
