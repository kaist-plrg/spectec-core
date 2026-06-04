open Common.Source
module Il = Lang.Il
module Sl = Lang.Sl

type t =
  | Test_start of { test_case_id : string }
  | Test_end of { test_case_id : string }
  | Rel_enter of { id : string; at : region; inputs : Il.Value.t list }
  | Rel_exit of {
      id : string;
      at : region;
      conclusion : (Il.Value.t, Il.Value.t) Il.Mode.t option;
    }
  | Rule_enter of { id : string; rule_id : string; at : region }
  | Rule_exit of { id : string; rule_id : string; at : region; success : bool }
  | Func_enter of { id : string; at : region; inputs : Il.Value.t list }
  | Func_exit of { id : string; at : region; output : Il.Value.t option }
  | Clause_enter of { id : string; clause_idx : int; at : region }
  | Clause_exit of {
      id : string;
      clause_idx : int;
      at : region;
      success : bool;
    }
  | Iter_prem_enter of { prem : Il.prem; at : region }
  | Iter_prem_exit of { at : region }
  | Prem_enter of { prem : Il.prem; at : region }
  | Prem_exit of {
      prem : Il.prem;
      at : region;
      success : bool;
      bindings : (Il.id * Il.Value.t) list;
    }
  | Instr of { instr : Sl.instr; at : region }
