open Domain.Lib
open Sl.Ast

type t = {
  (* Specification *)
  spec : spec;
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

(* Constructor *)

let init (spec : spec) (includes_p4 : string list) (dirname_gen : string)
    (dirname_well_p4 : string) (dirname_ill_p4 : string)
    (pids_covered : PIdSet.t) (filenames_seed_p4 : string list) =
  {
    spec;
    includes_p4;
    dirname_gen;
    dirname_well_p4;
    dirname_ill_p4;
    pids_covered;
    filenames_seed_p4;
  }
