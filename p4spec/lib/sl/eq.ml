open Ast
open Util.Source

(* Identifiers *)

let eq_id (id_a : id) (id_b : id) : bool = Il.Eq.eq_id id_a id_b

(* Atoms *)

let eq_atom (atom_a : atom) (atom_b : atom) : bool = Il.Eq.eq_atom atom_a atom_b

let eq_atoms (atoms_a : atom list) (atoms_b : atom list) : bool =
  Il.Eq.eq_atoms atoms_a atoms_b

(* Mixfix operators *)

let eq_mixop (mixop_a : mixop) (mixop_b : mixop) : bool =
  Il.Eq.eq_mixop mixop_a mixop_b

(* Iterators *)

let eq_iter (iter_a : iter) (iter_b : iter) : bool = Il.Eq.eq_iter iter_a iter_b

let eq_iters (iters_a : iter list) (iters_b : iter list) : bool =
  Il.Eq.eq_iters iters_a iters_b

(* Variables *)

let eq_var (var_a : var) (var_b : var) : bool = Il.Eq.eq_var var_a var_b

let eq_vars (vars_a : var list) (vars_b : var list) : bool =
  Il.Eq.eq_vars vars_a vars_b

(* Types *)

let eq_typ (typ_a : typ) (typ_b : typ) : bool = Il.Eq.eq_typ typ_a typ_b

let eq_typs (typs_a : typ list) (typs_b : typ list) : bool =
  Il.Eq.eq_typs typs_a typs_b

(* Expressions *)

let eq_exp (exp_a : exp) (exp_b : exp) : bool = Il.Eq.eq_exp exp_a exp_b

let eq_exps (exps_a : exp list) (exps_b : exp list) : bool =
  Il.Eq.eq_exps exps_a exps_b

let eq_iterexp (iterexp_a : iterexp) (iterexp_b : iterexp) : bool =
  Il.Eq.eq_iterexp iterexp_a iterexp_b

let eq_iterexps (iterexps_a : iterexp list) (iterexps_b : iterexp list) : bool =
  Il.Eq.eq_iterexps iterexps_a iterexps_b

(* Patterns *)

let eq_pattern (pattern_a : pattern) (pattern_b : pattern) : bool =
  Il.Eq.eq_pattern pattern_a pattern_b

(* Paths *)

let eq_path (path_a : path) (path_b : path) : bool = Il.Eq.eq_path path_a path_b

(* Arguments *)

let eq_arg (arg_a : arg) (arg_b : arg) : bool = Il.Eq.eq_arg arg_a arg_b

let eq_args (args_a : arg list) (args_b : arg list) : bool =
  Il.Eq.eq_args args_a args_b

(* Type arguments *)

let eq_targ (targ_a : targ) (targ_b : targ) : bool = Il.Eq.eq_targ targ_a targ_b

let eq_targs (targs_a : targ list) (targs_b : targ list) : bool =
  Il.Eq.eq_targs targs_a targs_b

(* Path conditions *)

let rec eq_phantom (phantom_a : phantom) (phantom_b : phantom) : bool =
  let pid_a, pathconds_a = phantom_a in
  let pid_b, pathconds_b = phantom_b in
  pid_a = pid_b && eq_pathconds pathconds_a pathconds_b

and eq_phantom_opt (phantom_opt_a : phantom option)
    (phantom_opt_b : phantom option) : bool =
  match (phantom_opt_a, phantom_opt_b) with
  | Some phantom_a, Some phantom_b -> eq_phantom phantom_a phantom_b
  | None, None -> true
  | _ -> false

and eq_pathcond (pathcond_a : pathcond) (pathcond_b : pathcond) : bool =
  match (pathcond_a, pathcond_b) with
  | ForallC (exp_a, iterexps_a), ForallC (exp_b, iterexps_b) ->
      eq_exp exp_a exp_b && eq_iterexps iterexps_a iterexps_b
  | ExistsC (exp_a, iterexps_a), ExistsC (exp_b, iterexps_b) ->
      eq_exp exp_a exp_b && eq_iterexps iterexps_a iterexps_b
  | PlainC exp_a, PlainC exp_b -> eq_exp exp_a exp_b
  | _ -> false

and eq_pathconds (pathconds_a : pathcond list) (pathconds_b : pathcond list) :
    bool =
  List.length pathconds_a = List.length pathconds_b
  && List.for_all2 eq_pathcond pathconds_a pathconds_b

(* Case analysis *)

and eq_case (case_a : case) (case_b : case) : bool =
  let guard_a, instrs_a = case_a in
  let guard_b, instrs_b = case_b in
  eq_guard guard_a guard_b && eq_instrs instrs_a instrs_b

and eq_cases (case_a : case list) (case_b : case list) : bool =
  List.length case_a = List.length case_b && List.for_all2 eq_case case_a case_b

and eq_guard (guard_a : guard) (guard_b : guard) : bool =
  match (guard_a, guard_b) with
  | BoolG b_a, BoolG b_b -> b_a = b_b
  | CmpG (cmpop_a, optyp_a, exp_a), CmpG (cmpop_b, optyp_b, exp_b) ->
      cmpop_a = cmpop_b && optyp_a = optyp_b && eq_exp exp_a exp_b
  | SubG typ_a, SubG typ_b -> eq_typ typ_a typ_b
  | MatchG pattern_a, MatchG pattern_b -> eq_pattern pattern_a pattern_b
  | MemG exp_a, MemG exp_b -> eq_exp exp_a exp_b
  | _ -> false

(* Instructions *)

and eq_instr (instr_a : instr) (instr_b : instr) : bool =
  match (instr_a.it, instr_b.it) with
  | ( IfI (exp_cond_a, iterexps_a, instrs_then_a, phantom_opt_a),
      IfI (exp_cond_b, iterexps_b, instrs_then_b, phantom_opt_b) ) ->
      eq_exp exp_cond_a exp_cond_b
      && eq_iterexps iterexps_a iterexps_b
      && eq_instrs instrs_then_a instrs_then_b
      && eq_phantom_opt phantom_opt_a phantom_opt_b
  | CaseI (exp_a, cases_a, phantom_opt_a), CaseI (exp_b, cases_b, phantom_opt_b)
    ->
      eq_exp exp_a exp_b && eq_cases cases_a cases_b
      && eq_phantom_opt phantom_opt_a phantom_opt_b
  | OtherwiseI instr_a, OtherwiseI instr_b -> eq_instr instr_a instr_b
  | LetI (exp_l_a, exp_r_a, iterexps_a), LetI (exp_l_b, exp_r_b, iterexps_b) ->
      eq_exp exp_l_a exp_l_b && eq_exp exp_r_a exp_r_b
      && eq_iterexps iterexps_a iterexps_b
  | ( RuleI (id_a, (mixop_a, exps_a), iterexps_a),
      RuleI (id_b, (mixop_b, exps_b), iterexps_b) ) ->
      eq_id id_a id_b && eq_mixop mixop_a mixop_b && eq_exps exps_a exps_b
      && eq_iterexps iterexps_a iterexps_b
  | ResultI exps_a, ResultI exps_b -> eq_exps exps_a exps_b
  | ReturnI exp_a, ReturnI exp_b -> eq_exp exp_a exp_b
  | _ -> false

and eq_instrs (instrs_a : instr list) (instrs_b : instr list) : bool =
  List.length instrs_a = List.length instrs_b
  && List.for_all2 eq_instr instrs_a instrs_b
