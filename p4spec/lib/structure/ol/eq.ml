open Ast

(* Cases *)

let rec eq_case (case_a : case) (case_b : case) : bool =
  let guard_a, instrs_a = case_a in
  let guard_b, instrs_b = case_b in
  eq_guard guard_a guard_b && eq_instrs instrs_a instrs_b

and eq_cases (cases_a : case list) (cases_b : case list) : bool =
  List.length cases_a = List.length cases_b
  && List.for_all2 eq_case cases_a cases_b

and eq_guard (guard_a : guard) (guard_b : guard) : bool =
  match (guard_a, guard_b) with
  | BoolG b_a, BoolG b_b -> b_a = b_b
  | CmpG (cmpop_a, optyp_a, exp_a), CmpG (cmpop_b, optyp_b, exp_b) ->
      cmpop_a = cmpop_b && optyp_a = optyp_b && Sl.Eq.eq_exp exp_a exp_b
  | SubG typ_a, SubG typ_b -> Sl.Eq.eq_typ typ_a typ_b
  | MatchG pattern_a, MatchG pattern_b -> Sl.Eq.eq_pattern pattern_a pattern_b
  | MemG exp_a, MemG exp_b -> Sl.Eq.eq_exp exp_a exp_b
  | _ -> false

(* Instructions *)

and eq_instr (instr_a : instr) (instr_b : instr) : bool =
  match (instr_a.it, instr_b.it) with
  | ( IfI (exp_cond_a, iterexps_a, instrs_then_a),
      IfI (exp_cond_b, iterexps_b, instrs_then_b) ) ->
      Sl.Eq.eq_exp exp_cond_a exp_cond_b
      && Sl.Eq.eq_iterexps iterexps_a iterexps_b
      && eq_instrs instrs_then_a instrs_then_b
  | CaseI (exp_a, cases_a, total_a), CaseI (exp_b, cases_b, total_b) ->
      Sl.Eq.eq_exp exp_a exp_b && eq_cases cases_a cases_b && total_a = total_b
  | OtherwiseI instr_a, OtherwiseI instr_b -> eq_instr instr_a instr_b
  | LetI (exp_l_a, exp_r_a, iterexps_a), LetI (exp_l_b, exp_r_b, iterexps_b) ->
      Sl.Eq.eq_exp exp_l_a exp_l_b
      && Sl.Eq.eq_exp exp_r_a exp_r_b
      && Sl.Eq.eq_iterexps iterexps_a iterexps_b
  | ( RuleI (id_a, (mixop_a, exps_a), iterexps_a),
      RuleI (id_b, (mixop_b, exps_b), iterexps_b) ) ->
      Sl.Eq.eq_id id_a id_b
      && Sl.Eq.eq_mixop mixop_a mixop_b
      && Sl.Eq.eq_exps exps_a exps_b
      && Sl.Eq.eq_iterexps iterexps_a iterexps_b
  | ResultI exps_a, ResultI exps_b -> Sl.Eq.eq_exps exps_a exps_b
  | ReturnI exp_a, ReturnI exp_b -> Sl.Eq.eq_exp exp_a exp_b
  | _ -> false

and eq_instrs (instrs_a : instr list) (instrs_b : instr list) : bool =
  List.length instrs_a = List.length instrs_b
  && List.for_all2 eq_instr instrs_a instrs_b
