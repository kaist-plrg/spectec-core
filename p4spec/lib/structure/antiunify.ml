open Domain.Lib
open Il.Ast
open Util.Source

(* Populating expression templates *)

let rec populate_exp_template (unifiers : IdSet.t) (exp_template : exp)
    (exp : exp) : prem list =
  let populate_exp_template_unequal () =
    match (exp_template.it, exp.it) with
    | VarE id_template, _ when IdSet.mem id_template unifiers ->
        let prem = LetPr (exp, exp_template) $ exp.at in
        [ prem ]
    | TupleE exps_template, TupleE exps ->
        populate_exps_templates unifiers exps_template exps
    | CaseE (mixop_template, exps_template), CaseE (mixop, exps)
      when Il.Eq.eq_mixop mixop_template mixop ->
        populate_exps_templates unifiers exps_template exps
    | IterE (exp_template, iterexp_template), IterE (exp, iterexp)
      when Il.Eq.eq_iterexp iterexp_template iterexp ->
        let prem = LetPr (exp, exp_template) $ exp.at in
        let prem = IterPr (prem, iterexp_template) $ exp.at in
        [ prem ]
    | VarE _, _ ->
        Format.asprintf
          "cannot populate iterated anti-unified expressions %s and %s"
          (Il.Print.string_of_exp exp_template)
          (Il.Print.string_of_exp exp)
        |> failwith
    | _ ->
        Format.asprintf "cannot populate anti-unified expressions %s and %s"
          (Il.Print.string_of_exp exp_template)
          (Il.Print.string_of_exp exp)
        |> failwith
  in
  if Il.Eq.eq_exp exp_template exp then [] else populate_exp_template_unequal ()

and populate_exps_templates (unifiers : IdSet.t) (exps_template : exp list)
    (exps : exp list) : prem list =
  List.fold_left2
    (fun prems exp_template exp ->
      prems @ populate_exp_template unifiers exp_template exp)
    [] exps_template exps

(* Anti-unification of expressions *)

let rec antiunify_exp (frees : IdSet.t) (unifiers : IdSet.t)
    (exp_template : exp) (exp : exp) : IdSet.t * IdSet.t * exp =
  let antiunify_exp_unequal () =
    let at, note = (exp_template.at, exp_template.note) in
    match (exp_template.it, exp.it) with
    | VarE id_template, _ when IdSet.mem id_template unifiers ->
        (frees, unifiers, exp_template)
    | VarE id_template, _ ->
        let id_fresh = Elaborate.Fresh.fresh_id frees id_template in
        let frees = IdSet.add id_fresh frees in
        let unifiers = IdSet.add id_fresh unifiers in
        let exp_template = VarE id_fresh $$ (at, note) in
        (frees, unifiers, exp_template)
    | _, VarE id ->
        let id_fresh = Elaborate.Fresh.fresh_id frees id in
        let frees = IdSet.add id_fresh frees in
        let unifiers = IdSet.add id_fresh unifiers in
        let exp_template = VarE id_fresh $$ (at, note) in
        (frees, unifiers, exp_template)
    | TupleE exps_template, TupleE exps ->
        let frees, unifiers, exps_template =
          antiunify_exps frees unifiers exps_template exps
        in
        let exp_template = TupleE exps_template $$ (at, note) in
        (frees, unifiers, exp_template)
    | CaseE (mixop_template, exps_template), CaseE (mixop, exps)
      when Il.Eq.eq_mixop mixop_template mixop ->
        let frees, unifiers, exps_template =
          antiunify_exps frees unifiers exps_template exps
        in
        let exp_template =
          CaseE (mixop_template, exps_template) $$ (at, note)
        in
        (frees, unifiers, exp_template)
    | IterE (exp_template, iterexp_template), IterE (exp, iterexp)
      when Il.Eq.eq_iterexp iterexp_template iterexp ->
        let frees, unifiers, exp_template =
          antiunify_exp frees unifiers exp_template exp
        in
        (* CHECK iterexp *)
        let exp_template =
          IterE (exp_template, iterexp_template) $$ (at, note)
        in
        (frees, unifiers, exp_template)
    | _ ->
        Format.asprintf "cannot anti-unify expressions %s and %s"
          (Il.Print.string_of_exp exp_template)
          (Il.Print.string_of_exp exp)
        |> failwith
  in
  if Il.Eq.eq_exp exp_template exp then (frees, unifiers, exp_template)
  else antiunify_exp_unequal ()

and antiunify_exps (frees : IdSet.t) (unifiers : IdSet.t)
    (exps_template : exp list) (exps : exp list) : IdSet.t * IdSet.t * exp list
    =
  List.fold_left2
    (fun (frees, unifiers, exps_template) exp_template exp ->
      let frees, unifiers, exp_template =
        antiunify_exp frees unifiers exp_template exp
      in
      (frees, unifiers, exps_template @ [ exp_template ]))
    (frees, unifiers, []) exps_template exps

let antiunify_exp_group (frees : IdSet.t) (exps : exp list) :
    IdSet.t * IdSet.t * exp =
  let exp_template, exps = (List.hd exps, List.tl exps) in
  List.fold_left
    (fun (frees, unifiers, exp_template) exp ->
      antiunify_exp frees unifiers exp_template exp)
    (frees, IdSet.empty, exp_template)
    exps

let antiunify_exps_group (frees : IdSet.t) (exps_group : exp list list) :
    IdSet.t * exp list =
  match exps_group with
  | [] -> (IdSet.empty, [])
  | _ ->
      let exps_batch =
        let width = exps_group |> List.hd |> List.length in
        let height = List.length exps_group in
        List.init width (fun j ->
            List.init height (fun i -> List.nth (List.nth exps_group i) j))
      in
      let _, unifiers_acc, exps_template =
        List.fold_left
          (fun (frees, unifiers_acc, exps_template) exp_batch ->
            let frees, unifiers, exp_template =
              antiunify_exp_group frees exp_batch
            in
            let unifiers_acc = IdSet.union unifiers_acc unifiers in
            (frees, unifiers_acc, exps_template @ [ exp_template ]))
          (frees, IdSet.empty, []) exps_batch
      in
      (unifiers_acc, exps_template)

(* Populating argument templates *)

let rec populate_arg_template (unifiers : IdSet.t) (arg_template : arg)
    (arg : arg) : prem list =
  match (arg_template.it, arg.it) with
  | ExpA exp_template, ExpA exp ->
      populate_exp_template unifiers exp_template exp
  | DefA id_template, DefA id when Il.Eq.eq_id id_template id -> []
  | _ ->
      Format.asprintf "cannot populate anti-unified arguments %s and %s"
        (Il.Print.string_of_arg arg_template)
        (Il.Print.string_of_arg arg)
      |> failwith

and populate_args_templates (unifiers : IdSet.t) (args_template : arg list)
    (args : arg list) : prem list =
  List.fold_left2
    (fun prems arg_template arg ->
      prems @ populate_arg_template unifiers arg_template arg)
    [] args_template args

(* Anti-unification of arguments *)

let antiunify_arg (frees : IdSet.t) (unifiers : IdSet.t) (arg_template : arg)
    (arg : arg) : IdSet.t * IdSet.t * arg =
  match (arg_template.it, arg.it) with
  | ExpA exp_template, ExpA exp ->
      let frees, unifiers, exp_template =
        antiunify_exp frees unifiers exp_template exp
      in
      let arg_template = ExpA exp_template $ arg_template.at in
      (frees, unifiers, arg_template)
  | DefA id_template, DefA id when Il.Eq.eq_id id_template id ->
      (frees, unifiers, arg_template)
  | _ -> assert false

let antiunify_arg_group (frees : IdSet.t) (args : arg list) :
    IdSet.t * IdSet.t * arg =
  let arg_template, args = (List.hd args, List.tl args) in
  List.fold_left
    (fun (frees, unifiers, arg_template) arg ->
      antiunify_arg frees unifiers arg_template arg)
    (frees, IdSet.empty, arg_template)
    args

let antiunify_args_group (frees : IdSet.t) (args_group : arg list list) :
    IdSet.t * arg list =
  match args_group with
  | [] -> (IdSet.empty, [])
  | _ ->
      let args_batch =
        let width = args_group |> List.hd |> List.length in
        let height = List.length args_group in
        List.init width (fun j ->
            List.init height (fun i -> List.nth (List.nth args_group i) j))
      in
      let _, unifiers_acc, args_template =
        List.fold_left
          (fun (frees, unifiers_acc, args_template) arg_batch ->
            let frees, unifiers, arg_template =
              antiunify_arg_group frees arg_batch
            in
            let unifiers_acc = IdSet.union unifiers_acc unifiers in
            (frees, unifiers_acc, args_template @ [ arg_template ]))
          (frees, IdSet.empty, []) args_batch
      in
      (unifiers_acc, args_template)

(* Anti-unification of rules *)

let antiunify_rules (inputs : int list) (rules : rule list) :
    exp list * (prem list * exp list) list =
  let exps_input_group, exps_output_group, prems_group, frees =
    List.fold_left
      (fun (exps_input_group, exps_output_group, prems_group, frees) rule ->
        let _, notexp, prems = rule.it in
        let _, exps = notexp in
        let exps_input, exps_output =
          Runtime_static.Rel.Hint.split_exps_without_idx inputs exps
        in
        let exps_input_group = exps_input_group @ [ exps_input ] in
        let exps_output_group = exps_output_group @ [ exps_output ] in
        let prems_group = prems_group @ [ prems ] in
        let frees = rule |> Il.Free.free_rule |> IdSet.union frees in
        (exps_input_group, exps_output_group, prems_group, frees))
      ([], [], [], IdSet.empty) rules
  in
  let unifiers, exps_input_template =
    antiunify_exps_group frees exps_input_group
  in
  let prems_group =
    List.map2
      (fun exps_input prems ->
        let prems_template =
          populate_exps_templates unifiers exps_input_template exps_input
        in
        prems_template @ prems)
      exps_input_group prems_group
  in
  let paths = List.combine prems_group exps_output_group in
  (exps_input_template, paths)

(* Anti-unification of clauses *)

let antiunify_clauses (clauses : clause list) :
    arg list * (prem list * exp) list =
  let args_input_group, exp_output_group, prems_group, frees =
    List.fold_left
      (fun (args_input_group, exp_output_group, prems_group, frees) clause ->
        let args_input, exp_output, prems = clause.it in
        let args_input_group = args_input_group @ [ args_input ] in
        let exp_output_group = exp_output_group @ [ exp_output ] in
        let prems_group = prems_group @ [ prems ] in
        let frees = clause |> Il.Free.free_clause |> IdSet.union frees in
        (args_input_group, exp_output_group, prems_group, frees))
      ([], [], [], IdSet.empty) clauses
  in
  let unifiers, args_input_template =
    antiunify_args_group frees args_input_group
  in
  let prems_group =
    List.map2
      (fun args_input prems ->
        let prems_template =
          populate_args_templates unifiers args_input_template args_input
        in
        prems_template @ prems)
      args_input_group prems_group
  in
  let paths = List.combine prems_group exp_output_group in
  (args_input_template, paths)
