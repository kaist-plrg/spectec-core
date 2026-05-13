open Common.Source
open Common.Domain
open Lang
open Lang.Il
module RTEnv = Envs.RTEnv
module TDEnv = Envs.Il.TDEnv

(* Structuring premises *)

let rec internalize_iter ?(iterexps : iterexp list = []) (prem : prem) :
    prem * iterexp list =
  match prem.it with
  | IterPr (prem, iterexp) ->
      internalize_iter ~iterexps:(iterexp :: iterexps) prem
  | _ -> (prem, iterexps)

let rec struct_prems (prems : prem list) (instr_ret : Ol.Ast.instr) :
    Ol.Ast.instr =
  let prems_internalized = List.map internalize_iter prems in
  struct_prems' prems_internalized instr_ret

and struct_prems' (prems_internalized : (prem * iterexp list) list)
    (instr_ret : Ol.Ast.instr) : Ol.Ast.instr =
  match prems_internalized with
  | [] -> instr_ret
  | [ ({ it = ElsePr; at; _ }, []) ] -> Ol.Ast.OtherwiseI instr_ret $ at
  | (prem_h, iterexps_h) :: prems_internalized_t -> (
      let at = prem_h.at in
      match prem_h.it with
      | RelPr { relid = id; notexp } ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.RuleI (id, notexp, iterexps_h, [ instr_t ]) $ at
      | IfPr { cond; _ } ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.IfI (cond, iterexps_h, [ instr_t ]) $ at
      | RelAssertPr { call = { relid = id; notexp }; expect } ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h' =
            if expect then Ol.Ast.IfHoldI (id, notexp, iterexps_h, [ instr_t ])
            else Ol.Ast.IfNotHoldI (id, notexp, iterexps_h, [ instr_t ])
          in
          instr_h' $ at
      | LetPr (exp_l, exp_r) ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.LetI (exp_l, exp_r, iterexps_h, [ instr_t ]) $ at
      | DebugPr exp ->
          let instr_t = struct_prems' prems_internalized_t instr_ret in
          Ol.Ast.DebugI (exp, instr_t) $ at
      | _ -> assert false)

let split_else_path ((prems, payload) : prem list * 'a) :
    (prem list * 'a) option * (prem list * 'a) =
  match List.rev prems with
  | { it = ElsePr; _ } :: prems_rev ->
      (Some (List.rev prems_rev, payload), (prems, payload))
  | _ -> (None, (prems, payload))

let partition_else_paths (paths : (prem list * 'a) list) :
    (prem list * 'a) list * (prem list * 'a) option =
  let normal_paths, else_paths =
    List.fold_right
      (fun path (normal_paths, else_paths) ->
        match split_else_path path with
        | Some else_path, _ ->
            if else_paths <> None then
              failwith "multiple otherwise paths are not supported"
            else (normal_paths, Some else_path)
        | None, normal_path -> (normal_path :: normal_paths, else_paths))
      paths ([], None)
  in
  (normal_paths, else_paths)

(* Structuring rules *)

let struct_rule_path ((prems, exps_output) : prem list * exp list) :
    Ol.Ast.instr list =
  let at =
    (* if exps_output is empty, use last prem's region *)
    if exps_output = [] then
      match List.rev prems with
      | prem_last :: _ -> prem_last.at
      | [] -> no_region
    else exps_output |> List.map at |> over_region
  in
  let instr_ret = Ol.Ast.ResultI exps_output $ at in
  let prems =
    match List.rev prems with
    | { it = ElsePr; _ } :: prems_rev -> List.rev prems_rev
    | _ -> prems
  in
  [ struct_prems prems instr_ret ]

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  let prems =
    match List.rev prems with
    | { it = ElsePr; _ } :: prems_rev -> List.rev prems_rev
    | _ -> prems
  in
  [ struct_prems prems instr_ret ]

(* Structuring definitions *)

let rec struct_def (rtenv : RTEnv.t) (tdenv : TDEnv.t) (def : def) : Sl.def =
  let at = def.at in
  match def.it with
  | TypD { synid = id; tparams; deftyp } -> Sl.TypD (id, tparams, deftyp) $ at
  | RelD { relid = id; reltyp; rules } ->
      struct_rel_def rtenv tdenv at id reltyp rules
  | BuiltinDecD { defid = id; tparams; params; _ } ->
      struct_builtin_dec_def at id tparams params
  | DecD { defid = id; tparams; clauses; _ } ->
      struct_dec_def rtenv tdenv at id tparams clauses

(* Structuring relation definitions *)

and struct_rel_def (rtenv : RTEnv.t) (tdenv : TDEnv.t) (at : region)
    (id_rel : id) (reltyp : reltyp) (rules : rule list) : Sl.def =
  let exps_input, paths = Antiunify.antiunify_rules reltyp.it rules in
  let block_paths, else_path_opt = partition_else_paths paths in
  let block =
    List.concat_map struct_rule_path block_paths
    |> Optimize.optimize rtenv tdenv
  in
  let elseblock_opt =
    Option.map
      (fun path -> struct_rule_path path |> Optimize.optimize rtenv tdenv)
      else_path_opt
  in
  let block, elseblock_opt = Instrument.instrument tdenv block elseblock_opt in
  let in_typs = Il.Mode.inputs reltyp.it in
  let exps_input =
    match rules with
    | [] ->
        (* The relation is never invoked, but the SL shape still needs one
           placeholder per input slot. *)
        let _, exps =
          List.fold_left
            (fun (frees, exps) typ ->
              let exp, frees = Elaborate.Fresh.fresh_exp_from_typ frees typ in
              (frees, exps @ [ exp ]))
            (IdSet.empty, []) in_typs
        in
        exps
    | _ ->
        assert (List.length exps_input = List.length in_typs);
        exps_input
  in
  let sl_mode = Il.Mode.with_inputs reltyp.it exps_input in
  Sl.RelD (id_rel, sl_mode, block, elseblock_opt) $ at

(* Structuring builtin declaration definitions *)

and struct_builtin_dec_def (at : region) (id_dec : id) (tparams : tparam list)
    (params : param list) : Sl.def =
  let args_input, _ =
    List.fold_left
      (fun (args_input, frees) param ->
        let arg_input, frees =
          match param.it with
          | ExpP typ ->
              let exp_input, frees =
                Elaborate.Fresh.fresh_exp_from_typ frees typ
              in
              let arg_input = ExpA exp_input $ param.at in
              (arg_input, frees)
          | DefP { defid = id_def; _ } ->
              let arg_input = DefA id_def $ param.at in
              (arg_input, frees)
        in
        (args_input @ [ arg_input ], frees))
      ([], IdSet.empty) params
  in
  Sl.BuiltinDecD (id_dec, tparams, args_input) $ at

(* Structuring declaration definitions *)

and struct_dec_def (rtenv : RTEnv.t) (tdenv : TDEnv.t) (at : region)
    (id_dec : id) (tparams : tparam list) (clauses : clause list) : Sl.def =
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let block_paths, else_path_opt = partition_else_paths paths in
  let block =
    List.concat_map struct_clause_path block_paths
    |> Optimize.optimize rtenv tdenv
  in
  let elseblock_opt =
    Option.map
      (fun path -> struct_clause_path path |> Optimize.optimize rtenv tdenv)
      else_path_opt
  in
  let block, elseblock_opt = Instrument.instrument tdenv block elseblock_opt in
  Sl.DecD (id_dec, tparams, args_input, block, elseblock_opt) $ at

(* Load type definitions *)

let load_def (rtenv : RTEnv.t) (tdenv : TDEnv.t) (def : def) : RTEnv.t * TDEnv.t
    =
  match def.it with
  | TypD { synid = id; tparams; deftyp } ->
      let typdef = (tparams, deftyp) in
      let tdenv = TDEnv.add id typdef tdenv in
      (rtenv, tdenv)
  | RelD { relid = id; reltyp; _ } ->
      let rtenv = RTEnv.add id reltyp rtenv in
      (rtenv, tdenv)
  | _ -> (rtenv, tdenv)

let load_spec (rtenv : RTEnv.t) (tdenv : TDEnv.t) (spec : spec) :
    RTEnv.t * TDEnv.t =
  List.fold_left
    (fun (rtenv, tdenv) def -> load_def rtenv tdenv def)
    (rtenv, tdenv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.spec =
  let rtenv, tdenv = load_spec RTEnv.empty TDEnv.empty spec in
  List.map (struct_def rtenv tdenv) spec
