open Xl
open Il.Ast
module Hint = Runtime_static.Rel.Hint
module Typ = Runtime_dynamic.Typ
module Value = Runtime_dynamic.Value
module Rel = Runtime_dynamic.Rel
open Error
open Attempt
open Util.Source

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg

(* Expansion of type aliases *)

let rec expand_typ (ctx : Ctx.t) (typ : Typ.t) : Typ.t =
  match typ.it with
  | VarT (tid, _targs) -> (
      let _tparams, deftyp = Ctx.find_typdef ctx tid in
      match deftyp.it with PlainT typ -> expand_typ ctx typ | _ -> typ)
  | _ -> typ

(* Type equivalence and subtyping *)

let rec equiv_typ (ctx : Ctx.t) (typ_a : Typ.t) (typ_b : Typ.t) : bool =
  let typ_a = expand_typ ctx typ_a in
  let typ_b = expand_typ ctx typ_b in
  match (typ_a.it, typ_b.it) with
  | BoolT, BoolT -> true
  | NumT numtyp_a, NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
  | TextT, TextT -> true
  | VarT (tid_a, targs_a), VarT (tid_b, targs_b) ->
      tid_a.it = tid_b.it
      && List.length targs_a = List.length targs_b
      && List.for_all2 (equiv_typ ctx) targs_a targs_b
  | TupleT typs_a, TupleT typs_b ->
      List.length typs_a = List.length typs_b
      && List.for_all2 (equiv_typ ctx) typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
      equiv_typ ctx typ_a typ_b && iter_a = iter_b
  | _ -> false

and sub_typ (ctx : Ctx.t) (typ_a : Typ.t) (typ_b : Typ.t) : bool =
  equiv_typ ctx typ_a typ_b || sub_typ' ctx typ_a typ_b

and sub_typ' (ctx : Ctx.t) (typ_a : Typ.t) (typ_b : Typ.t) : bool =
  let typ_a = expand_typ ctx typ_a in
  let typ_b = expand_typ ctx typ_b in
  match (typ_a.it, typ_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.sub numtyp_a numtyp_b
  | TupleT typs_a, TupleT typs_b ->
      List.length typs_a = List.length typs_b
      && List.for_all2 (sub_typ ctx) typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) when iter_a = iter_b ->
      sub_typ ctx typ_a typ_b
  | IterT (typ_a, Opt), IterT (typ_b, List) -> sub_typ ctx typ_a typ_b
  | _, IterT (typ_b, Opt) -> sub_typ ctx typ_a typ_b
  | _, IterT (typ_b, List) -> sub_typ ctx typ_a typ_b
  | _ -> false

(* Assignments *)

(* Transpose a matrix of values, as a list of value batches
   that are to be each fed into an iterated expression *)

let transpose (value_matrix : Value.t list list) : Value.t list list =
  match value_matrix with
  | [] -> []
  | _ ->
      let width = List.length (List.hd value_matrix) in
      check
        (List.for_all
           (fun value_row -> List.length value_row = width)
           value_matrix)
        no_region "value matrix is not rectangular";
      List.init width (fun j ->
          List.init (List.length value_matrix) (fun i ->
              List.nth (List.nth value_matrix i) j))

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : Value.t) : Ctx.t attempt =
  match (exp.it, value) with
  | VarE id, _ ->
      let ctx = Ctx.add_value ctx (id, []) value in
      Ok ctx
  | TupleE exps, TupleV values -> assign_exps ctx exps values
  | CaseE notexp, CaseV (mixop, values) ->
      let mixop_exp, exps = notexp in
      let mixop_exp = List.map (List.map it) mixop_exp in
      if List.compare (List.compare Atom.compare) mixop_exp mixop <> 0 then
        fail exp.at
          (Format.asprintf "mismatch in case expression: %s expected but got %s"
             (Il.Print.string_of_exp exp)
             (Value.to_string value))
      else assign_exps ctx exps values
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp, Some value -> assign_exp ctx exp value
      | None, None -> Ok ctx
      | Some _, None ->
          fail exp.at
            (Format.asprintf "cannot assign a none value into %s"
               (Il.Print.string_of_exp exp))
      | None, Some _ ->
          fail exp.at
            (Format.asprintf "cannot assign a value %s into a none expression"
               (Value.to_string value)))
  | ListE exps, ListV values -> assign_exps ctx exps values
  | ConsE (exp_h, exp_t), ListV values ->
      if values = [] then
        fail exp.at "cannot assign an empty list into a cons expression"
      else
        let value_h = List.hd values in
        let value_t = Value.ListV (List.tl values) in
        let* ctx = assign_exp ctx exp_h value_h in
        assign_exp ctx exp_t value_t
  | IterE (_, (Opt, vars)), OptV None ->
      let ctx =
        List.fold_left
          (fun ctx (id, _typ, iters) ->
            Ctx.add_value ctx (id, iters @ [ Opt ]) value)
          ctx vars
      in
      Ok ctx
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let* ctx = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      let ctx =
        List.fold_left
          (fun ctx (id, _typ, iters) ->
            let value = Ctx.find_value ctx (id, iters) in
            let value = Value.OptV (Some value) in
            Ctx.add_value ctx (id, iters @ [ Opt ]) value)
          ctx vars
      in
      Ok ctx
  | IterE (exp, (List, vars)), ListV values ->
      (* Map over the value list elements,
         and assign each value to the iterated expression *)
      let* ctxs =
        List.fold_left
          (fun ctxs value ->
            let* ctxs = ctxs in
            let* ctx = assign_exp Ctx.empty exp value in
            Ok (ctxs @ [ ctx ]))
          (Ok []) values
      in
      (* Per iterated variable, collect its elementwise value,
         then make a sequence out of them *)
      let ctx =
        List.fold_left
          (fun ctx (id, _typ, iters) ->
            let values =
              List.map (fun ctx -> Ctx.find_value ctx (id, iters)) ctxs
            in
            let value = Value.ListV values in
            Ctx.add_value ctx (id, iters @ [ List ]) value)
          ctx vars
      in
      Ok ctx
  (* (TODO) Need runtime check for subtype relation *)
  | CastE (exp, _), _ -> assign_exp ctx exp value
  | _ ->
      fail exp.at
        (Format.asprintf "(TODO) match failed %s <- %s"
           (Il.Print.string_of_exp exp)
           (Value.to_string value))

and assign_exps (ctx : Ctx.t) (exps : exp list) (values : Value.t list) :
    Ctx.t attempt =
  check
    (List.length exps = List.length values)
    no_region
    (Format.asprintf
       "mismatch in number of expressions and values while assigning, expected \
        %d value(s) but got %d"
       (List.length exps) (List.length values));
  List.fold_left2
    (fun ctx exp value ->
      let* ctx = ctx in
      assign_exp ctx exp value)
    (Ok ctx) exps values

and assign_arg (ctx : Ctx.t) (arg : arg) (value : Value.t) : Ctx.t attempt =
  match arg.it with
  | ExpA exp -> assign_exp ctx exp value
  | DefA _ -> fail arg.at "(TODO) assign_arg"

(* Expression evaluation *)

let rec eval_exp (ctx : Ctx.t) (exp : exp) : Value.t =
  match exp.it with
  | BoolE b -> BoolV b
  | NumE n -> NumV n
  | TextE s -> TextV s
  | VarE id -> eval_var_exp ctx id
  | UnE (unop, optyp, exp) -> eval_un_exp ctx unop optyp exp
  | BinE (binop, optyp, exp_l, exp_r) ->
      eval_bin_exp ctx binop optyp exp_l exp_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      eval_cmp_exp ctx cmpop optyp exp_l exp_r
  | TupleE exps -> eval_tuple_exp ctx exps
  | CaseE notexp -> eval_case_exp ctx notexp
  | OptE exp_opt -> eval_opt_exp ctx exp_opt
  | StrE fields -> eval_str_exp ctx fields
  | DotE (exp_b, atom) -> eval_dot_exp ctx exp_b atom
  | ListE exps -> eval_list_exp ctx exps
  | ConsE (exp_h, exp_t) -> eval_cons_exp ctx exp_h exp_t
  | MemE (exp_e, exp_s) -> eval_mem_exp ctx exp_e exp_s
  | UpdE (exp_b, path, exp_f) -> eval_upd_exp ctx exp_b path exp_f
  | CallE (id, targs, args) -> eval_call_exp ctx id targs args
  | IterE (exp, iterexp) -> eval_iter_exp ctx exp iterexp
  | _ ->
      error exp.at
        (Format.asprintf "(TODO) eval_exp %s" (Il.Print.string_of_exp exp))

and eval_exps (ctx : Ctx.t) (exps : exp list) : Value.t list =
  List.map (eval_exp ctx) exps

(* Variable expression evaluation *)

and eval_var_exp (ctx : Ctx.t) (id : id) : Value.t = Ctx.find_value ctx (id, [])

(* Unary expression evaluation *)

and eval_un_bool (unop : Bool.unop) (value : Value.t) : Value.t =
  match unop with `NotOp -> BoolV (not (Value.get_bool value))

and eval_un_exp (ctx : Ctx.t) (unop : unop) (_optyp : optyp) (exp : exp) :
    Value.t =
  let value = eval_exp ctx exp in
  match unop with
  | #Bool.unop as unop -> eval_un_bool unop value
  | #Num.unop -> failwith "(TODO) eval_un_exp"

(* Binary expression evaluation *)

and eval_bin_bool (binop : Bool.binop) (value_l : Value.t) (value_r : Value.t) :
    Value.t =
  let bool_l = Value.get_bool value_l in
  let bool_r = Value.get_bool value_r in
  match binop with
  | `AndOp -> BoolV (bool_l && bool_r)
  | `OrOp -> BoolV (bool_l || bool_r)
  | `ImplOp -> BoolV ((not bool_l) || bool_r)
  | `EquivOp -> BoolV (bool_l = bool_r)

and eval_bin_exp (ctx : Ctx.t) (binop : binop) (_optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Value.t =
  let value_l = eval_exp ctx exp_l in
  let value_r = eval_exp ctx exp_r in
  match binop with
  | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
  | #Num.binop -> failwith "(TODO) eval_bin_exp"

(* Comparison expression evaluation *)

and eval_cmp_bool (cmpop : Bool.cmpop) (value_l : Value.t) (value_r : Value.t) :
    Value.t =
  let eq = Value.eq value_l value_r in
  match cmpop with `EqOp -> BoolV eq | `NeOp -> BoolV (not eq)

and eval_cmp_exp (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Value.t =
  let value_l = eval_exp ctx exp_l in
  let value_r = eval_exp ctx exp_r in
  match cmpop with
  | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
  | #Num.cmpop -> failwith "(TODO) eval_cmp_exp"

(* Tuple expression evaluation *)

and eval_tuple_exp (ctx : Ctx.t) (exps : exp list) : Value.t =
  let values = eval_exps ctx exps in
  TupleV values

(* Case expression evaluation *)

and eval_case_exp (ctx : Ctx.t) (notexp : notexp) : Value.t =
  let mixop, exps = notexp in
  let mixop = List.map (List.map it) mixop in
  let values = eval_exps ctx exps in
  CaseV (mixop, values)

(* Option expression evaluation *)

and eval_opt_exp (ctx : Ctx.t) (exp_opt : exp option) : Value.t =
  match exp_opt with
  | Some exp ->
      let value = eval_exp ctx exp in
      OptV (Some value)
  | None -> OptV None

(* Struct expression evaluation *)

and eval_str_exp (ctx : Ctx.t) (fields : (atom * exp) list) : Value.t =
  let atoms, exps = List.split fields in
  let atoms = List.map it atoms in
  let values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  StrV fields

(* Dot expression evaluation *)

and eval_dot_exp (ctx : Ctx.t) (exp_b : exp) (atom : atom) : Value.t =
  let fields = eval_exp ctx exp_b |> Value.get_str in
  List.assoc atom.it fields

(* List expression evaluation *)

and eval_list_exp (ctx : Ctx.t) (exps : exp list) : Value.t =
  let values = eval_exps ctx exps in
  ListV values

(* Cons expression evaluation *)

and eval_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) : Value.t =
  let value_h = eval_exp ctx exp_h in
  let values_t = eval_exp ctx exp_t |> Value.unseq in
  ListV (value_h :: values_t)

(* Membership expression evaluation *)

and eval_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) : Value.t =
  let value_e = eval_exp ctx exp_e in
  let values_s = eval_exp ctx exp_s |> Value.unseq in
  BoolV (List.mem value_e values_s)

(* Update expression evaluation *)

and access_path (value_b : Value.t) (path : path) : Value.t =
  match path.it with
  | RootP -> value_b
  | DotP (path, atom) ->
      let value = access_path value_b path in
      let fields = value |> Value.get_str in
      List.assoc atom.it fields
  | _ -> failwith "(TODO) access_path"

and update_path (value_b : Value.t) (path : path) (value_n : Value.t) : Value.t
    =
  match path.it with
  | RootP -> value_n
  | DotP (path, atom) ->
      let value = access_path value_b path in
      let fields = value |> Value.get_str in
      let fields =
        List.map
          (fun (atom_f, value_f) ->
            if atom_f = atom.it then (atom_f, value_n) else (atom_f, value_f))
          fields
      in
      let value = Value.StrV fields in
      update_path value_b path value
  | _ -> failwith "(TODO) update"

and eval_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    Value.t =
  let value_b = eval_exp ctx exp_b in
  let value_f = eval_exp ctx exp_f in
  update_path value_b path value_f

(* Function call expression evaluation *)

and eval_call_exp (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
    : Value.t =
  let+ value = invoke_func ctx id targs args in
  value

(* Iterated expression evaluation *)

and eval_iter_exp_opt (ctx : Ctx.t) (exp : exp) (vars : var list) : Value.t =
  (* First collect the values that are to be iterated over *)
  let values =
    List.map
      (fun var ->
        let id, _typ, iters = var in
        Ctx.find_value ctx (id, iters @ [ Opt ]) |> Value.unopt)
      vars
  in
  (* Evaluate the expression for each value,
     followed by an option constructor *)
  match
    (List.for_all Option.is_some values, List.for_all Option.is_none values)
  with
  | true, true -> assert false
  | true, _ ->
      let ctx =
        List.fold_left2
          (fun ctx var value ->
            let id, _typ, iters = var in
            let value = Option.get value in
            Ctx.add_value ctx (id, iters @ [ Opt ]) value)
          ctx vars values
      in
      let value = eval_exp ctx exp in
      Value.OptV (Some value)
  | _, true -> Value.OptV None
  | _ -> error exp.at "mismatch in optionality of iterated variables"

and eval_iter_exp_list (ctx : Ctx.t) (exp : exp) (vars : var list) : Value.t =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let values_batch =
    List.map
      (fun var ->
        let id, _typ, iters = var in
        Ctx.find_value ctx (id, iters @ [ List ]) |> Value.unseq)
      vars
    |> transpose
  in
  (* Then evaluate the expression for each batch of values,
     followed by a sequencing operation *)
  List.map
    (fun values ->
      let ctx =
        List.fold_left2
          (fun ctx var value ->
            let id, _typ, iters = var in
            Ctx.add_value ctx (id, iters) value)
          ctx vars values
      in
      eval_exp ctx exp)
    values_batch
  |> Value.seq

and eval_iter_exp (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) : Value.t =
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt ctx exp vars
  | List -> eval_iter_exp_list ctx exp vars

(* Argument evaluation *)

and eval_arg (ctx : Ctx.t) (arg : arg) : Value.t =
  match arg.it with
  | ExpA exp -> eval_exp ctx exp
  | DefA _ -> error arg.at "(TODO) eval_arg"

and eval_args (ctx : Ctx.t) (args : arg list) : Value.t list =
  List.map (eval_arg ctx) args

(* Premise evaluation *)

and eval_prem (ctx : Ctx.t) (prem : prem) : Ctx.t attempt =
  match prem.it with
  | RulePr (id, notexp) -> eval_rule_prem ctx id notexp
  | IfPr exp -> eval_if_prem ctx exp
  | ElsePr -> Ok ctx
  | LetPr (exp_l, exp_r) -> eval_let_prem ctx exp_l exp_r
  | IterPr (prem, iterexp) -> eval_iter_prem ctx prem iterexp

and eval_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t attempt =
  List.fold_left
    (fun ctx prem ->
      let* ctx = ctx in
      eval_prem ctx prem)
    (Ok ctx) prems

(* Rule premise evaluation *)

and eval_rule_prem (ctx : Ctx.t) (id : id) (notexp : notexp) : Ctx.t attempt =
  let rel = Ctx.find_rel ctx id in
  let exps_input, exps_output =
    let _, inputs, _ = rel in
    let _, exps = notexp in
    Hint.split_exps_without_idx inputs exps
  in
  let values_input = eval_exps ctx exps_input in
  let* values_output = invoke_rel ctx id values_input in
  assign_exps ctx exps_output values_output

(* If premise evaluation *)

and eval_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t attempt =
  let cond = eval_exp ctx exp |> Value.get_bool in
  if cond then Ok ctx
  else
    fail exp.at
      (Format.asprintf "condition %s was not met" (Il.Print.string_of_exp exp))

(* Let premise evaluation *)

and eval_let_prem (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t attempt =
  let value = eval_exp ctx exp_r in
  assign_exp ctx exp_l value

(* Iterated premise evaluation *)

and eval_iter_prem (ctx : Ctx.t) (prem : prem) (iterexp : iterexp) :
    Ctx.t attempt =
  let iter, vars = iterexp in
  match iter with
  | Opt -> error prem.at "(TODO) eval_iter_prem"
  | List ->
      (* Discriminate between bound and binding variables *)
      let vars_bound, vars_binding =
        List.partition
          (fun (id, _typ, iters) -> Ctx.bound_value ctx (id, iters @ [ List ]))
          vars
      in
      (* First break the bound values that are to be iterated over,
         into a batch of values *)
      let values_bound_batch =
        List.map
          (fun (id, _typ, iters) ->
            Ctx.find_value ctx (id, iters @ [ List ]) |> Value.unseq)
          vars_bound
        |> transpose
      in
      let ctx =
        match values_bound_batch with
        (* If the bound variable supposed to guide the iteration is already empty,
           then the binding variables are also empty *)
        | [] ->
            List.fold_left
              (fun ctx (id, _typ, iters) ->
                Ctx.add_value ctx (id, iters @ [ List ]) (Value.ListV []))
              ctx vars_binding
        (* Otherwise, evaluate the premise for each batch of bound values,
           and collect the resulting binding batches *)
        | _ ->
            let values_binding_batch =
              List.map
                (fun values_bound ->
                  let ctx =
                    List.fold_left2
                      (fun ctx var_bound value_bound ->
                        let id, _typ, iters = var_bound in
                        Ctx.add_value ctx (id, iters) value_bound)
                      ctx vars_bound values_bound
                  in
                  let+ ctx = eval_prem ctx prem in
                  List.map
                    (fun var_binding ->
                      let id, _typ, iters = var_binding in
                      Ctx.find_value ctx (id, iters))
                    vars_binding)
                values_bound_batch
            in
            let values_binding = values_binding_batch |> transpose in
            (* Finally, bind the resulting binding batches *)
            List.fold_left2
              (fun ctx (id, _typ, iters) values_binding ->
                let value_binding = Value.seq values_binding in
                Ctx.add_value ctx (id, iters @ [ List ]) value_binding)
              ctx vars_binding values_binding
      in
      Ok ctx

(* Invoke a relation *)

and match_rule (ctx : Ctx.t) (inputs : Hint.t) (rule : rule)
    (values_input : Value.t list) : (Ctx.t * id * prem list * exp list) attempt
    =
  let id, notexp, prems = rule.it in
  let exps_input, exps_output =
    let _, exps = notexp in
    Hint.split_exps_without_idx inputs exps
  in
  check
    (List.length exps_input = List.length values_input)
    rule.at "arity mismatch in rule";
  let ctx = Ctx.localize ctx in
  let* ctx = assign_exps ctx exps_input values_input in
  Ok (ctx, id, prems, exps_output)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : Value.t list) :
    Value.t list attempt =
  invoke_rel' ctx id values_input
  |> nest id.at (Format.asprintf "invocation of relation %s failed" id.it)

and invoke_rel' (ctx : Ctx.t) (id : id) (values_input : Value.t list) :
    Value.t list attempt =
  let _, inputs, rules = Ctx.find_rel ctx id in
  guard (rules <> []) id.at "relation has no rules";
  (* Find rules that match the input values *)
  let rules =
    List.map
      (fun rule ->
        let rule_match = match_rule ctx inputs rule values_input in
        match rule_match with
        | Ok (ctx, id, prems, exps_output) -> Some (ctx, id, prems, exps_output)
        | Fail _ -> None)
      rules
    |> List.filter_map Fun.id
  in
  (* Apply the first matching rule *)
  let attempt_rules =
    List.map
      (fun (ctx, id_rule, prems, exps_output) ->
        let attempt_rule' () : Value.t list attempt =
          let* ctx = eval_prems ctx prems in
          let values_output = eval_exps ctx exps_output in
          Ok values_output
        in
        let attempt_rule () : Value.t list attempt =
          attempt_rule' ()
          |> nest id.at
               (Format.asprintf "application of rule %s/%s failed" id.it
                  id_rule.it)
        in
        attempt_rule)
      rules
  in
  choice attempt_rules

(* Invoke a function *)

and match_clause (ctx : Ctx.t) (clause : clause) (values_input : Value.t list) :
    (Ctx.t * arg list * prem list * exp) attempt =
  let args_input, exp_output, prems = clause.it in
  check
    (List.length args_input = List.length values_input)
    clause.at "arity mismatch while matching clause";
  let ctx = Ctx.localize ctx in
  let* ctx =
    List.fold_left2
      (fun ctx arg_input value_input ->
        let* ctx = ctx in
        assign_arg ctx arg_input value_input)
      (Ok ctx) args_input values_input
  in
  Ok (ctx, args_input, prems, exp_output)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    Value.t attempt =
  invoke_func' ctx id targs args
  |> nest id.at (Format.asprintf "invocation of function %s failed" id.it)

and invoke_func' (ctx : Ctx.t) (id : id) (_targs : targ list) (args : arg list)
    : Value.t attempt =
  let func = Ctx.find_func ctx id in
  let _tparams, _params, _typ_ret, clauses = func in
  guard (clauses <> []) id.at "function has no clauses";
  (* Find clauses that match the input values *)
  let values_input = eval_args ctx args in
  let clauses =
    List.map
      (fun clause ->
        let clause_match = match_clause ctx clause values_input in
        match clause_match with
        | Ok (ctx, args_input, prems, exp_output) ->
            Some (ctx, args_input, prems, exp_output)
        | Fail _ -> None)
      clauses
    |> List.filter_map Fun.id
  in
  (* Apply the first matching clause *)
  let attempt_clauses =
    List.map
      (fun (ctx, args_input, prems, exp_output) ->
        let attempt_clause' () : Value.t attempt =
          let* ctx = eval_prems ctx prems in
          let value_output = eval_exp ctx exp_output in
          Ok value_output
        in
        let attempt_clause () : Value.t attempt =
          attempt_clause' ()
          |> nest id.at
               (Format.asprintf "application of clause %s%s failed" id.it
                  (Il.Print.string_of_args args_input))
        in
        attempt_clause)
      clauses
  in
  choice attempt_clauses

(* Load definitions into a context *)

let load_def (ctx : Ctx.t) (def : def) : Ctx.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      Ctx.add_typdef ctx id typdef
  | RelD (id, nottyp, inputs, rules) ->
      let rel = (nottyp, inputs, rules) in
      Ctx.add_rel ctx id rel
  | DecD (id, tparams, params, typ, clauses) ->
      let func = (tparams, params, typ, clauses) in
      Ctx.add_func ctx id func

let load_spec (spec : spec) : Ctx.t = List.fold_left load_def Ctx.empty spec

(* Entry point: run typing rule from `Prog_ok` relation *)

let run_typing (spec : spec) (program : Value.t) : Value.t list =
  let ctx = load_spec spec in
  let+ values = invoke_rel ctx ("Prog_ok" $ no_region) [ program ] in
  List.iter (fun value -> Format.printf "%s\n" (Value.to_string value)) values;
  values
