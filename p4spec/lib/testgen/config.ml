open Domain.Lib
open Sl.Ast
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv

(* Environment for the spec *)

type specenv = { spec : spec; tdenv : TDEnv.t; includes_p4 : string list }

(* Output directories for generated P4 programs *)

type outdirs = {
  dirname_gen : string;
  dirname_well_p4 : string;
  dirname_ill_p4 : string;
}

(* Seed programs for mutation *)

type seed = { filenames_seed_p4 : string list; pids_uncovered : PIdSet.t }

(* Configuration for the fuzz campaign *)

type t = {
  logger : Logger.t;
  specenv : specenv;
  outdirs : outdirs;
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

(* Logging *)

let log (config : t) (msg : string) : unit = Logger.log config.logger msg

(* Constructor *)

let init_specenv (spec : spec) (includes_p4 : string list) : specenv =
  let tdenv = load_spec TDEnv.empty spec in
  { spec; tdenv; includes_p4 }

let init_outdirs (dirname_gen : string) : outdirs =
  Filesys.mkdir dirname_gen;
  let dirname_well_p4 = dirname_gen ^ "/welltyped" in
  Filesys.mkdir dirname_well_p4;
  let dirname_ill_p4 = dirname_gen ^ "/illtyped" in
  Filesys.mkdir dirname_ill_p4;
  { dirname_gen; dirname_well_p4; dirname_ill_p4 }

let init_seed (filenames_seed_p4 : string list) (pids_uncovered : PIdSet.t) :
    seed =
  { filenames_seed_p4; pids_uncovered }

let init (logger : Logger.t) (specenv : specenv) (outdirs : outdirs)
    (seed : seed) =
  { logger; specenv; outdirs; seed }

(* Destructor *)

let close (config : t) = Logger.close config.logger
