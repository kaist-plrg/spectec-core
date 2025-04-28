open Domain.Lib
open Sl.Ast
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv

type t = {
  (* Specification *)
  spec : spec;
  (* Typedef environment for type-driven mutation *)
  tdenv : TDEnv.t;
  (* Includes for P4 program *)
  includes_p4 : string list;
  (* Output directories *)
  dirname_gen : string;
  dirname_well_p4 : string;
  dirname_ill_p4 : string;
  (* Seed programs *)
  pids_covered : PIdSet.t;
  filenames_seed_p4 : string list;
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

(* Constructor *)

let init (spec : spec) (includes_p4 : string list) (dirname_gen : string)
    (dirname_well_p4 : string) (dirname_ill_p4 : string)
    (pids_covered : PIdSet.t) (filenames_seed_p4 : string list) =
  let tdenv = load_spec TDEnv.empty spec in
  {
    spec;
    tdenv;
    includes_p4;
    dirname_gen;
    dirname_well_p4;
    dirname_ill_p4;
    pids_covered;
    filenames_seed_p4;
  }
