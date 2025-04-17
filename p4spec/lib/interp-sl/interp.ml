open Domain.Lib
open Xl
open Sl.Ast
module Hint = Runtime_static.Rel.Hint
module Typ = Runtime_dynamic_il.Typ
module Value = Runtime_dynamic_il.Value
module Rel = Runtime_dynamic_sl.Rel
open Runtime_dynamic_sl.Envs
open Error
module F = Format
open Util.Source

(* Assignments *)

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
  match (exp.it, value) with
  | VarE id, _ ->
      let ctx = Ctx.add_value Local ctx (id, []) value in
      ctx
  | TupleE exps, TupleV values -> assign_exps ctx exps values
  | CaseE notexp, CaseV (_mixop_value, values) ->
      let _mixop_exp, exps = notexp in
      assign_exps ctx exps values
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp, Some value -> assign_exp ctx exp value
      | None, None -> ctx
      | _ -> assert false)
  | ListE exps, ListV values -> assign_exps ctx exps values
  | ConsE (exp_h, exp_t), ListV values ->
      let value_h = List.hd values in
      let value_t = Il.Ast.ListV (List.tl values) in
      let ctx = assign_exp ctx exp_h value_h in
      assign_exp ctx exp_t value_t
  | IterE (_, (Opt, vars)), OptV None ->
      (* Per iterated variable, make an option out of the value *)
      List.fold_left
        (fun ctx (id, iters) ->
          let value = Il.Ast.OptV None in
          Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value)
        ctx vars
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let ctx = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      List.fold_left
        (fun ctx (id, iters) ->
          let value =
            let value = Ctx.find_value Local ctx (id, iters) in
            Il.Ast.OptV (Some value)
          in
          Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value)
        ctx vars
  | IterE (exp, (List, vars)), ListV values ->
      (* Map over the value list elements,
         and assign each value to the iterated expression *)
      let ctxs =
        List.fold_left
          (fun ctxs value ->
            let ctx =
              { ctx with local = { ctx.local with venv = VEnv.empty } }
            in
            let ctx = assign_exp ctx exp value in
            ctxs @ [ ctx ])
          [] values
      in
      (* Per iterated variable, collect its elementwise value,
         then make a sequence out of them *)
      List.fold_left
        (fun ctx (id, iters) ->
          let values =
            List.map (fun ctx -> Ctx.find_value Local ctx (id, iters)) ctxs
          in
          let value = Il.Ast.ListV values in
          Ctx.add_value Local ctx (id, iters @ [ Il.Ast.List ]) value)
        ctx vars
  | _ ->
      error exp.at
        (F.asprintf "(TODO) match failed %s <- %s"
           (Sl.Print.string_of_exp exp)
           (Sl.Print.string_of_value ~short:true value))

and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t =
  check
    (List.length exps = List.length values)
    (over_region (List.map at exps))
    (F.asprintf
       "mismatch in number of expressions and values while assigning, expected \
        %d value(s) but got %d"
       (List.length exps) (List.length values));
  List.fold_left2 assign_exp ctx exps values

(* Assigning a value to an argument *)

and assign_arg (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (arg : arg)
    (value : value) : Ctx.t =
  match arg.it with
  | ExpA exp -> assign_arg_exp ctx_callee exp value
  | DefA id -> assign_arg_def ctx_caller ctx_callee id value

and assign_args (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (args : arg list)
    (values : value list) : Ctx.t =
  check
    (List.length args = List.length values)
    (over_region (List.map at args))
    (F.asprintf
       "mismatch in number of arguments and values while assigning, expected \
        %d value(s) but got %d"
       (List.length args) (List.length values));
  List.fold_left2 (assign_arg ctx_caller) ctx_callee args values

and assign_arg_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
  assign_exp ctx exp value

and assign_arg_def (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (id : id)
    (value : value) : Ctx.t =
  match value with
  | FuncV id_f ->
      let func = Ctx.find_func Local ctx_caller id_f in
      Ctx.add_func Local ctx_callee id func
  | _ ->
      error id.at
        (F.asprintf "cannot assign a value %s to a definition %s"
           (Sl.Print.string_of_value ~short:true value)
           id.it)

(* Expression evaluation *)

(* DownCastE and SubE performs subtype checks that are not guaranteed by the type system,
    because in SpecTec assignment should be able to revert the type cast expression

     - Numeric subtyping:
       - e.g., -- if (int) n = $foo() when $foo() returns a positive integer +2
     - Variant subtyping:
       - e.g., -- if (typ) objtyp = $foo() when $foo() returns a variant of objtyp specifically
     - Tuple subtyping: recursive, but the type system guarantees that their lengths are equal
     - Iteration subtyping

   Note that structs are invariant in SpecTec, so we do not need to check for subtyping *)

let rec eval_exp (ctx : Ctx.t) (exp : exp) : value =
  let at = exp.at in
  match exp.it with
  | BoolE b -> eval_bool_exp b
  | NumE n -> eval_num_exp n
  | TextE s -> eval_text_exp s
  | VarE id -> eval_var_exp ctx id
  | UnE (unop, optyp, exp) -> eval_un_exp ctx unop optyp exp
  | BinE (binop, optyp, exp_l, exp_r) ->
      eval_bin_exp ctx binop optyp exp_l exp_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      eval_cmp_exp ctx cmpop optyp exp_l exp_r
  | UpCastE (typ, exp) -> eval_upcast_exp ctx typ exp
  | DownCastE (typ, exp) -> eval_downcast_exp ctx typ exp
  | SubE (exp, typ) -> eval_sub_exp ctx exp typ
  | MatchE (exp, pattern) -> eval_match_exp ctx exp pattern
  | TupleE exps -> eval_tuple_exp ctx exps
  | CaseE notexp -> eval_case_exp ctx notexp
  | StrE fields -> eval_str_exp ctx fields
  | OptE exp_opt -> eval_opt_exp ctx exp_opt
  | ListE exps -> eval_list_exp ctx exps
  | ConsE (exp_h, exp_t) -> eval_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> eval_cat_exp ctx at exp_l exp_r
  | MemE (exp_e, exp_s) -> eval_mem_exp ctx exp_e exp_s
  | LenE exp -> eval_len_exp ctx exp
  | DotE (exp_b, atom) -> eval_dot_exp ctx exp_b atom
  | IdxE (exp_b, exp_i) -> eval_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> eval_slice_exp ctx exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> eval_upd_exp ctx exp_b path exp_f
  | CallE (id, targs, args) -> eval_call_exp ctx id targs args
  | HoldE (id, notexp) -> eval_hold_exp ctx id notexp
  | IterE (exp, iterexp) -> eval_iter_exp ctx exp iterexp

and eval_exps (ctx : Ctx.t) (exps : exp list) : value list =
  List.map (eval_exp ctx) exps

(* Boolean expression evaluation *)

and eval_bool_exp (b : bool) : value = BoolV b

(* Numeric expression evaluation *)

and eval_num_exp (n : Num.t) : value = NumV n

(* Text expression evaluation *)

and eval_text_exp (s : string) : value = TextV s

(* Variable expression evaluation *)

and eval_var_exp (ctx : Ctx.t) (id : id) : value =
  Ctx.find_value Local ctx (id, [])

(* Unary expression evaluation *)

and eval_un_bool (unop : Bool.unop) (value : value) : value =
  match unop with `NotOp -> BoolV (not (Value.get_bool value))

and eval_un_num (unop : Num.unop) (value : value) : value =
  let num = Value.get_num value in
  let num = Num.un unop num in
  NumV num

and eval_un_exp (ctx : Ctx.t) (unop : unop) (_optyp : optyp) (exp : exp) : value
    =
  let value = eval_exp ctx exp in
  match unop with
  | #Bool.unop as unop -> eval_un_bool unop value
  | #Num.unop as unop -> eval_un_num unop value

(* Binary expression evaluation *)

and eval_bin_bool (ctx : Ctx.t) (binop : Bool.binop) (exp_l : exp) (exp_r : exp)
    : value =
  let value_l = eval_exp ctx exp_l in
  let bool_l = Value.get_bool value_l in
  match binop with
  | `AndOp when not bool_l -> BoolV false
  | `AndOp ->
      let value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      BoolV (bool_l && bool_r)
  | `OrOp when bool_l -> BoolV true
  | `OrOp ->
      let value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      BoolV (bool_l || bool_r)
  | `ImplOp when not bool_l -> BoolV true
  | `ImplOp ->
      let value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      BoolV ((not bool_l) || bool_r)
  | `EquivOp ->
      let value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      BoolV (bool_l = bool_r)

and eval_bin_num (ctx : Ctx.t) (binop : Num.binop) (exp_l : exp) (exp_r : exp) :
    value =
  let value_l = eval_exp ctx exp_l in
  let num_l = Value.get_num value_l in
  let value_r = eval_exp ctx exp_r in
  let num_r = Value.get_num value_r in
  let num = Num.bin binop num_l num_r in
  NumV num

and eval_bin_exp (ctx : Ctx.t) (binop : binop) (_optyp : optyp) (exp_l : exp)
    (exp_r : exp) : value =
  match binop with
  | #Bool.binop as binop -> eval_bin_bool ctx binop exp_l exp_r
  | #Num.binop as binop -> eval_bin_num ctx binop exp_l exp_r

(* Comparison expression evaluation *)

and eval_cmp_bool (cmpop : Bool.cmpop) (value_l : value) (value_r : value) :
    value =
  let eq = Value.eq value_l value_r in
  match cmpop with `EqOp -> BoolV eq | `NeOp -> BoolV (not eq)

and eval_cmp_num (cmpop : Num.cmpop) (value_l : value) (value_r : value) : value
    =
  let num_l = Value.get_num value_l in
  let num_r = Value.get_num value_r in
  BoolV (Num.cmp cmpop num_l num_r)

and eval_cmp_exp (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp) (exp_l : exp)
    (exp_r : exp) : value =
  let value_l = eval_exp ctx exp_l in
  let value_r = eval_exp ctx exp_r in
  match cmpop with
  | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
  | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r

(* Upcast expression evaluation *)

and upcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
  match typ.it with
  | NumT `IntT -> (
      match value with
      | NumV (`Nat n) -> NumV (`Int n)
      | NumV (`Int _) -> value
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value) with
      | PlainT typ, _ ->
          let typ = Typ.subst_typ theta typ in
          upcast ctx typ value
      | _ -> value)
  | TupleT typs -> (
      match value with
      | TupleV values ->
          let values = List.map2 (upcast ctx) typs values in
          TupleV values
      | _ -> assert false)
  | _ -> value

and eval_upcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : value =
  let value = eval_exp ctx exp in
  upcast ctx typ value

(* Downcast expression evaluation *)

and downcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
  match typ.it with
  | NumT `NatT -> (
      match value with
      | NumV (`Nat _) -> value
      | NumV (`Int i) when Bigint.(i >= zero) -> NumV (`Nat i)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value) with
      | PlainT typ, _ ->
          let typ = Typ.subst_typ theta typ in
          downcast ctx typ value
      | _ -> value)
  | TupleT typs -> (
      match value with
      | TupleV values ->
          let values = List.map2 (downcast ctx) typs values in
          TupleV values
      | _ -> assert false)
  | _ -> value

and eval_downcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : value =
  let value = eval_exp ctx exp in
  downcast ctx typ value

(* Subtype check expression evaluation *)

and subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
  match typ.it with
  | NumT `NatT -> (
      match value with
      | NumV (`Nat _) -> true
      | NumV (`Int i) -> Bigint.(i >= zero)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value) with
      | PlainT typ, _ ->
          let typ = Typ.subst_typ theta typ in
          subtyp ctx typ value
      | VariantT typcases, CaseV (mixop_v, _) ->
          List.exists
            (fun nottyp ->
              let mixop_t, _ = nottyp.it in
              Mixop.eq mixop_t mixop_v)
            typcases
      | _ -> true)
  | TupleT typs -> (
      match value with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (subtyp ctx) typs values
      | _ -> false)
  | _ -> true

and eval_sub_exp (ctx : Ctx.t) (exp : exp) (typ : typ) : value =
  let value = eval_exp ctx exp in
  let sub = subtyp ctx typ value in
  BoolV sub

(* Pattern match check expression evaluation *)

and eval_match_exp (ctx : Ctx.t) (exp : exp) (pattern : pattern) : value =
  let value = eval_exp ctx exp in
  let matches =
    match (pattern, value) with
    | CaseP mixop_p, CaseV (mixop_v, _) -> Mixop.eq mixop_p mixop_v
    | ListP listpattern, ListV values -> (
        let len_v = List.length values in
        match listpattern with
        | `Cons -> len_v > 0
        | `Fixed len_p -> len_v = len_p
        | `Nil -> len_v = 0)
    | OptP `Some, OptV (Some _) -> true
    | OptP `None, OptV None -> true
    | _ -> false
  in
  BoolV matches

(* Tuple expression evaluation *)

and eval_tuple_exp (ctx : Ctx.t) (exps : exp list) : value =
  let values = eval_exps ctx exps in
  TupleV values

(* Case expression evaluation *)

and eval_case_exp (ctx : Ctx.t) (notexp : notexp) : value =
  let mixop, exps = notexp in
  let values = eval_exps ctx exps in
  CaseV (mixop, values)

(* Struct expression evaluation *)

and eval_str_exp (ctx : Ctx.t) (fields : (atom * exp) list) : value =
  let atoms, exps = List.split fields in
  let values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  StructV fields

(* Option expression evaluation *)

and eval_opt_exp (ctx : Ctx.t) (exp_opt : exp option) : value =
  match exp_opt with
  | Some exp ->
      let value = eval_exp ctx exp in
      OptV (Some value)
  | None -> OptV None

(* List expression evaluation *)

and eval_list_exp (ctx : Ctx.t) (exps : exp list) : value =
  let values = eval_exps ctx exps in
  ListV values

(* Cons expression evaluation *)

and eval_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) : value =
  let value_h = eval_exp ctx exp_h in
  let value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  ListV (value_h :: values_t)

(* Concatenation expression evaluation *)

and eval_cat_exp (ctx : Ctx.t) (at : region) (exp_l : exp) (exp_r : exp) : value
    =
  let value_l = eval_exp ctx exp_l in
  let value_r = eval_exp ctx exp_r in
  match (value_l, value_r) with
  | TextV s_l, TextV s_r -> TextV (s_l ^ s_r)
  | ListV values_l, ListV values_r -> ListV (values_l @ values_r)
  | _ -> error at "concatenation expects either two texts or two lists"

(* Membership expression evaluation *)

and eval_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) : value =
  let value_e = eval_exp ctx exp_e in
  let value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  BoolV (List.exists (Value.eq value_e) values_s)

(* Length expression evaluation *)

and eval_len_exp (ctx : Ctx.t) (exp : exp) : value =
  let value = eval_exp ctx exp in
  let len = value |> Value.get_list |> List.length |> Bigint.of_int in
  NumV (`Nat len)

(* Dot expression evaluation *)

and eval_dot_exp (ctx : Ctx.t) (exp_b : exp) (atom : atom) : value =
  let value_b = eval_exp ctx exp_b in
  let fields = Value.get_struct value_b in
  fields
  |> List.map (fun (atom, value) -> (atom.it, value))
  |> List.assoc atom.it

(* Index expression evaluation *)

and eval_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) : value =
  let value_b = eval_exp ctx exp_b in
  let value_i = eval_exp ctx exp_i in
  let values = Value.get_list value_b in
  let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  List.nth values idx

(* Slice expression evaluation *)

and eval_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) (exp_n : exp) :
    value =
  let value_b = eval_exp ctx exp_b in
  let values = Value.get_list value_b in
  let value_i = eval_exp ctx exp_i in
  let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let value_n = eval_exp ctx exp_n in
  let idx_n = value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let idx_h = idx_l + idx_n in
  let values_slice =
    List.mapi
      (fun idx value ->
        if idx_l <= idx && idx < idx_h then Some value else None)
      values
    |> List.filter_map Fun.id
  in
  ListV values_slice

(* Update expression evaluation *)

and eval_access_path (value_b : value) (path : path) : value =
  match path.it with
  | RootP -> value_b
  | DotP (path, atom) ->
      let value = eval_access_path value_b path in
      let fields = value |> Value.get_struct in
      fields
      |> List.map (fun (atom, value) -> (atom.it, value))
      |> List.assoc atom.it
  | _ -> failwith "(TODO) access_path"

and eval_update_path (value_b : value) (path : path) (value_n : value) : value =
  match path.it with
  | RootP -> value_n
  | DotP (path, atom) ->
      let value = eval_access_path value_b path in
      let fields = value |> Value.get_struct in
      let fields =
        List.map
          (fun (atom_f, value_f) ->
            if atom_f.it = atom.it then (atom_f, value_n) else (atom_f, value_f))
          fields
      in
      let value = Il.Ast.StructV fields in
      eval_update_path value_b path value
  | _ -> failwith "(TODO) update"

and eval_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) : value
    =
  let value_b = eval_exp ctx exp_b in
  let value_f = eval_exp ctx exp_f in
  eval_update_path value_b path value_f

(* Function call expression evaluation *)

and eval_call_exp (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
    : value =
  invoke_func ctx id targs args

(* Conditional relation holds expression evaluation *)

and eval_hold_exp (ctx : Ctx.t) (id : id) (notexp : notexp) : value =
  let _, exps_input = notexp in
  let values_input = eval_exps ctx exps_input in
  try
    invoke_rel ctx id values_input |> ignore;
    BoolV true
  with _ -> BoolV false

(* Iterated expression evaluation *)

and eval_iter_exp_opt (ctx : Ctx.t) (exp : exp) (vars : var list) : value =
  let ctx_sub_opt = Ctx.sub_opt ctx vars in
  match ctx_sub_opt with
  | Some ctx_sub ->
      let value = eval_exp ctx_sub exp in
      OptV (Some value)
  | None -> OptV None

and eval_iter_exp_list (ctx : Ctx.t) (exp : exp) (vars : var list) : value =
  let ctxs_sub = Ctx.sub_list ctx vars in
  let values =
    List.fold_left
      (fun values ctx_sub ->
        let value = eval_exp ctx_sub exp in
        values @ [ value ])
      [] ctxs_sub
  in
  ListV values

and eval_iter_exp (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) : value =
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt ctx exp vars
  | List -> eval_iter_exp_list ctx exp vars

(* Argument evaluation *)

and eval_arg (ctx : Ctx.t) (arg : arg) : value =
  match arg.it with ExpA exp -> eval_exp ctx exp | DefA id -> FuncV id

and eval_args (ctx : Ctx.t) (args : arg list) : value list =
  List.map (eval_arg ctx) args

(* Instruction evaluation *)

and eval_instr (ctx : Ctx.t) (instr : instr) : Ctx.t * Sign.t =
  match instr.it with
  | RuleI (id, notexp, iterexps) -> eval_rule_instr ctx id notexp iterexps
  | IfI (exp_cond, iterexps, instrs_then, instrs_else) ->
      eval_if_instr ctx exp_cond iterexps instrs_then instrs_else
  | OtherwiseI instr -> eval_instr ctx instr
  | LetI (exp_l, exp_r, iterexps) -> eval_let_instr ctx exp_l exp_r iterexps
  | ResultI exps -> eval_result_instr ctx exps
  | ReturnI exp -> eval_return_instr ctx exp
  | PhantomI _ -> (ctx, Cont)

and eval_instrs (ctx : Ctx.t) (sign : Sign.t) (instrs : instr list) :
    Ctx.t * Sign.t =
  List.fold_left
    (fun (ctx, sign) instr ->
      match sign with Sign.Cont -> eval_instr ctx instr | _ -> (ctx, sign))
    (ctx, sign) instrs

(* Rule instruction evaluation *)

and eval_rule (ctx : Ctx.t) (id : id) (notexp : notexp) : Ctx.t =
  let rel = Ctx.find_rel Local ctx id in
  let exps_input, exps_output =
    let inputs, _, _ = rel in
    let _, exps = notexp in
    Hint.split_exps_without_idx inputs exps
  in
  let values_input = eval_exps ctx exps_input in
  let values_output = invoke_rel ctx id values_input in
  assign_exps ctx exps_output values_output

and eval_rule_opt (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
    (_vars : var list) (_iterexps : iterexp list) : Ctx.t =
  failwith "(TODO) eval_rule_opt"

and eval_rule_list (ctx : Ctx.t) (id : id) (notexp : notexp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun (id, iters) ->
        Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.List ]))
      vars
  in
  (* Create a subcontext for each batch of bound values *)
  let ctxs_sub = Ctx.sub_list ctx vars_bound in
  let values_binding =
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] -> List.init (List.length vars_binding) (fun _ -> [])
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        ctxs_sub
        |> List.fold_left
             (fun values_binding_batch ctx_sub ->
               let ctx_sub = eval_rule_iter' ctx_sub id notexp iterexps in
               let value_binding_batch =
                 List.map (Ctx.find_value Local ctx_sub) vars_binding
               in
               values_binding_batch @ [ value_binding_batch ])
             []
        |> Ctx.transpose
  in
  (* Finally, bind the resulting binding batches *)
  List.fold_left2
    (fun ctx (id, iters) values_binding ->
      let value_binding = Il.Ast.ListV values_binding in
      Ctx.add_value Local ctx (id, iters @ [ Il.Ast.List ]) value_binding)
    ctx vars_binding values_binding

and eval_rule_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps : iterexp list) : Ctx.t =
  match iterexps with
  | [] -> eval_rule ctx id notexp
  | iterexp_h :: iterexps_t -> (
      let iter_h, vars_h = iterexp_h in
      match iter_h with
      | Opt -> eval_rule_opt ctx id notexp vars_h iterexps_t
      | List -> eval_rule_list ctx id notexp vars_h iterexps_t)

and eval_rule_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps : iterexp list) : Ctx.t =
  let iterexps = List.rev iterexps in
  eval_rule_iter' ctx id notexp iterexps

and eval_rule_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps : iterexp list) : Ctx.t * Sign.t =
  let ctx = eval_rule_iter ctx id notexp iterexps in
  (ctx, Cont)

(* If instruction evaluation *)

and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) : bool =
  let value_cond = eval_exp ctx exp_cond in
  Value.get_bool value_cond

and eval_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
    (iterexps : iterexp list) : bool =
  Ctx.sub_list ctx vars
  |> List.for_all (fun ctx_sub -> eval_if_cond_iter' ctx_sub exp_cond iterexps)

and eval_if_cond_iter' (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    : bool =
  match iterexps with
  | [] -> eval_if_cond ctx exp_cond
  | iterexp_h :: iterexps_t -> (
      let iter_h, vars_h = iterexp_h in
      match iter_h with
      | Opt -> error no_region "(TODO)"
      | List -> eval_if_cond_list ctx exp_cond vars_h iterexps_t)

and eval_if_cond_iter (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list) :
    bool =
  let iterexps = List.rev iterexps in
  eval_if_cond_iter' ctx exp_cond iterexps

and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (instrs_then : instr list) (instrs_else : instr list) : Ctx.t * Sign.t =
  let cond = eval_if_cond_iter ctx exp_cond iterexps in
  if cond then eval_instrs ctx Cont instrs_then
  else eval_instrs ctx Cont instrs_else

(* Let instruction evaluation *)

and eval_let (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t =
  let value = eval_exp ctx exp_r in
  assign_exp ctx exp_l value

and eval_let_opt (_ctx : Ctx.t) (_exp_l : exp) (_exp_r : exp) (_vars : var list)
    (_iterexps : iterexp list) : Ctx.t =
  failwith "(TODO) eval_let_opt"

and eval_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun (id, iters) ->
        Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.List ]))
      vars
  in
  (* Create a subcontext for each batch of bound values *)
  let ctxs_sub = Ctx.sub_list ctx vars_bound in
  let values_binding =
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] -> List.init (List.length vars_binding) (fun _ -> [])
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        ctxs_sub
        |> List.fold_left
             (fun values_binding_batch ctx_sub ->
               let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
               let value_binding_batch =
                 List.map (Ctx.find_value Local ctx_sub) vars_binding
               in
               values_binding_batch @ [ value_binding_batch ])
             []
        |> Ctx.transpose
  in
  (* Finally, bind the resulting binding batches *)
  List.fold_left2
    (fun ctx (id, iters) values_binding ->
      let value_binding = Il.Ast.ListV values_binding in
      Ctx.add_value Local ctx (id, iters @ [ Il.Ast.List ]) value_binding)
    ctx vars_binding values_binding

and eval_let_iter' (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterexps : iterexp list) : Ctx.t =
  match iterexps with
  | [] -> eval_let ctx exp_l exp_r
  | iterexp_h :: iterexps_t -> (
      let iter_h, vars_h = iterexp_h in
      match iter_h with
      | Opt -> eval_let_opt ctx exp_l exp_r vars_h iterexps_t
      | List -> eval_let_list ctx exp_l exp_r vars_h iterexps_t)

and eval_let_iter (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterexps : iterexp list) : Ctx.t =
  let iterexps = List.rev iterexps in
  eval_let_iter' ctx exp_l exp_r iterexps

and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterexps : iterexp list) : Ctx.t * Sign.t =
  let ctx = eval_let_iter ctx exp_l exp_r iterexps in
  (ctx, Cont)

(* Result instruction evaluation *)

and eval_result_instr (ctx : Ctx.t) (exps : exp list) : Ctx.t * Sign.t =
  let values = eval_exps ctx exps in
  (ctx, Res values)

(* Return instruction evaluation *)

and eval_return_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Sign.t =
  let value = eval_exp ctx exp in
  (ctx, Ret value)

(* Return instruction evaluation *)

(* Invoke a relation *)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) : value list
    =
  let _inputs, exps_input, instrs = Ctx.find_rel Local ctx id in
  check (instrs <> []) id.at "relation has no instructions";
  let ctx_local = Ctx.localize ctx in
  let ctx_local = assign_exps ctx_local exps_input values_input in
  let _ctx_local, sign = eval_instrs ctx_local Cont instrs in
  match sign with
  | Res values -> values
  | _ -> error id.at "relation was not matched"

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    value =
  if Builtin.is_builtin id then invoke_func_builtin ctx id targs args
  else invoke_func_def ctx id targs args

and invoke_func_builtin (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : value =
  let values_input = eval_args ctx args in
  Builtin.invoke id targs values_input

and invoke_func_def (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : value =
  let tparams, args_input, instrs = Ctx.find_func Local ctx id in
  check (instrs <> []) id.at "function has no instructions";
  let ctx_local = Ctx.localize ctx in
  check
    (List.length targs = List.length tparams)
    id.at "arity mismatch in type arguments";
  let targs =
    let theta =
      TDEnv.bindings ctx.global.tdenv @ TDEnv.bindings ctx.local.tdenv
      |> List.filter_map (fun (tid, (_tparams, deftyp)) ->
             match deftyp.it with
             | Il.Ast.PlainT typ -> Some (tid, typ)
             | _ -> None)
      |> TIdMap.of_list
    in
    List.map (Typ.subst_typ theta) targs
  in
  let ctx_local =
    List.fold_left2
      (fun ctx_local tparam targ ->
        Ctx.add_typdef Local ctx_local tparam ([], Il.Ast.PlainT targ $ targ.at))
      ctx_local tparams targs
  in
  let values_input = eval_args ctx args in
  let ctx_local = assign_args ctx ctx_local args_input values_input in
  let _ctx_local, sign = eval_instrs ctx_local Cont instrs in
  match sign with
  | Ret value -> value
  | _ -> error id.at "function was not matched"

(* Load definitions into the context *)

let load_def (ctx : Ctx.t) (def : def) : Ctx.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      Ctx.add_typdef Global ctx id typdef
  | RelD (id, (_, inputs), exps_input, instrs) ->
      let rel = (inputs, exps_input, instrs) in
      Ctx.add_rel Global ctx id rel
  | DecD (id, tparams, args_input, instrs) ->
      let func = (tparams, args_input, instrs) in
      Ctx.add_func Global ctx id func

let load_spec (ctx : Ctx.t) (spec : spec) : Ctx.t =
  List.fold_left load_def ctx spec

(* Entry point: run typing rule from `Prog_ok` relation *)

let run_typing (spec : spec) (includes_p4 : string list) (filename_p4 : string)
    : value list =
  Builtin.init ();
  let program = P4.In.in_program includes_p4 filename_p4 in
  let ctx = Ctx.empty () in
  let ctx = load_spec ctx spec in
  invoke_rel ctx ("Prog_ok" $ no_region) [ program ]
