open Il.Ast
module HEnv = Runtime_static.Envs.HEnv
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
open Util.Source

(* Structuring premises *)

let rec internalize_iter ?(iterexps : iterexp list = []) (prem : prem) :
    prem * iterexp list =
  match prem.it with
  | IterPr (prem, iterexp) ->
      internalize_iter ~iterexps:(iterexps @ [ iterexp ]) prem
  | _ -> (prem, iterexps)

let rec struct_prems (prems : prem list) (instr_ret : Ol.Ast.instr) :
    Ol.Ast.instr list =
  let prems_internalized = List.map internalize_iter prems in
  struct_prems' prems_internalized instr_ret

and struct_prems' (prems_internalized : (prem * iterexp list) list)
    (instr_ret : Ol.Ast.instr) : Ol.Ast.instr list =
  match prems_internalized with
  | [] -> [ instr_ret ]
  | [ ({ it = ElsePr; at; _ }, []) ] ->
      let instr = Ol.Ast.OtherwiseI instr_ret $ at in
      [ instr ]
  | (prem_h, iterexps_h) :: prems_internalized_t -> (
      let at = prem_h.at in
      match prem_h.it with
      | RulePr (id, notexp) ->
          let instr_h = Ol.Ast.RuleI (id, notexp, iterexps_h) $ at in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          instr_h :: instrs_t
      | IfPr exp ->
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h = Ol.Ast.IfI (exp, iterexps_h, instrs_t) $ at in
          [ instr_h ]
      | LetPr (exp_l, exp_r) ->
          let instr_h = Ol.Ast.LetI (exp_l, exp_r, iterexps_h) $ at in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          instr_h :: instrs_t
      | _ -> assert false)

(* Structuring rules *)

let struct_rule_path ((prems, exps_output) : prem list * exp list) :
    Ol.Ast.instr list =
  let at = exps_output |> List.map at |> over_region in
  let instr_ret = Ol.Ast.ResultI exps_output $ at in
  struct_prems prems instr_ret

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  struct_prems prems instr_ret

(* Structuring definitions *)

let rec struct_def (henv : HEnv.t) (tdenv : TDEnv.t) (def : def) : Sl.Ast.def =
  let at = def.at in
  match def.it with
  | TypD (id, tparams, deftyp) -> Sl.Ast.TypD (id, tparams, deftyp) $ at
  | RelD (id, nottyp, inputs, rules) ->
      struct_rel_def henv tdenv at id nottyp inputs rules
  | DecD (id, tparams, _params, _typ, clauses) ->
      struct_dec_def henv tdenv at id tparams clauses

(* Structuring relation definitions *)

and struct_rel_def (henv : HEnv.t) (tdenv : TDEnv.t) (at : region) (id_rel : id)
    (nottyp : nottyp) (inputs : int list) (rules : rule list) : Sl.Ast.def =
  let mixop, _ = nottyp.it in
  let exps_input, paths = Antiunify.antiunify_rules inputs rules in
  let instrs = List.concat_map struct_rule_path paths in
  let instrs = Optimize.optimize henv tdenv instrs in
  let instrs = Instrument.instrument tdenv instrs in
  Sl.Ast.RelD (id_rel, (mixop, inputs), exps_input, instrs) $ at

(* Structuring declaration definitions *)

and struct_dec_def (henv : HEnv.t) (tdenv : TDEnv.t) (at : region) (id_dec : id)
    (tparams : tparam list) (clauses : clause list) : Sl.Ast.def =
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let instrs = List.concat_map struct_clause_path paths in
  let instrs = Optimize.optimize henv tdenv instrs in
  let instrs = Instrument.instrument tdenv instrs in
  Sl.Ast.DecD (id_dec, tparams, args_input, instrs) $ at

(* Load type definitions *)

let load_def (henv : HEnv.t) (tdenv : TDEnv.t) (def : def) : HEnv.t * TDEnv.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      let tdenv = TDEnv.add id typdef tdenv in
      (henv, tdenv)
  | RelD (id, _, inputs, _) ->
      let henv = HEnv.add id inputs henv in
      (henv, tdenv)
  | _ -> (henv, tdenv)

let load_spec (henv : HEnv.t) (tdenv : TDEnv.t) (spec : spec) : HEnv.t * TDEnv.t
    =
  List.fold_left
    (fun (henv, tdenv) def -> load_def henv tdenv def)
    (henv, tdenv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.Ast.spec =
  let henv, tdenv = load_spec HEnv.empty TDEnv.empty spec in
  List.map (struct_def henv tdenv) spec
