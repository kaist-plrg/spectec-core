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

let rec eval_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let at = exp.at in
  match exp.it with
  | BoolE b -> eval_bool_exp ctx b
  | NumE n -> eval_num_exp ctx n
  | TextE s -> eval_text_exp ctx s
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

and eval_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) exp ->
      let ctx, value = eval_exp ctx exp in
      (ctx, values @ [ value ]))
    (ctx, []) exps

(* Boolean expression evaluation *)

and eval_bool_exp (ctx : Ctx.t) (b : bool) : Ctx.t * value = (ctx, BoolV b)

(* Numeric expression evaluation *)

and eval_num_exp (ctx : Ctx.t) (n : Num.t) : Ctx.t * value = (ctx, NumV n)

(* Text expression evaluation *)

and eval_text_exp (ctx : Ctx.t) (s : string) : Ctx.t * value = (ctx, TextV s)

(* Variable expression evaluation *)

and eval_var_exp (ctx : Ctx.t) (id : id) : Ctx.t * value =
  let value = Ctx.find_value Local ctx (id, []) in
  (ctx, value)

(* Unary expression evaluation *)

and eval_un_bool (unop : Bool.unop) (value : value) : value =
  match unop with `NotOp -> BoolV (not (Value.get_bool value))

and eval_un_num (unop : Num.unop) (value : value) : value =
  let num = Value.get_num value in
  let num = Num.un unop num in
  NumV num

and eval_un_exp (ctx : Ctx.t) (unop : unop) (_optyp : optyp) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value =
    match unop with
    | #Bool.unop as unop -> eval_un_bool unop value
    | #Num.unop as unop -> eval_un_num unop value
  in
  (ctx, value)

(* Binary expression evaluation *)

and eval_bin_bool (ctx : Ctx.t) (binop : Bool.binop) (exp_l : exp) (exp_r : exp)
    : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let bool_l = Value.get_bool value_l in
  match binop with
  | `AndOp when not bool_l -> (ctx, BoolV false)
  | `AndOp ->
      let ctx, value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      let value = Il.Ast.BoolV (bool_l && bool_r) in
      (ctx, value)
  | `OrOp when bool_l -> (ctx, BoolV true)
  | `OrOp ->
      let ctx, value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      let value = Il.Ast.BoolV (bool_l || bool_r) in
      (ctx, value)
  | `ImplOp when not bool_l -> (ctx, BoolV true)
  | `ImplOp ->
      let ctx, value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      let value = Il.Ast.BoolV ((not bool_l) || bool_r) in
      (ctx, value)
  | `EquivOp ->
      let ctx, value_r = eval_exp ctx exp_r in
      let bool_r = Value.get_bool value_r in
      let value = Il.Ast.BoolV (bool_l = bool_r) in
      (ctx, value)

and eval_bin_num (ctx : Ctx.t) (binop : Num.binop) (exp_l : exp) (exp_r : exp) :
    Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let num_l = Value.get_num value_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let num_r = Value.get_num value_r in
  let num = Num.bin binop num_l num_r in
  let value = Il.Ast.NumV num in
  (ctx, value)

and eval_bin_exp (ctx : Ctx.t) (binop : binop) (_optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Ctx.t * value =
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
    (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value =
    match cmpop with
    | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
    | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r
  in
  (ctx, value)

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

and eval_upcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value = upcast ctx typ value in
  (ctx, value)

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

and eval_downcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value = downcast ctx typ value in
  (ctx, value)

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

and eval_sub_exp (ctx : Ctx.t) (exp : exp) (typ : typ) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let sub = subtyp ctx typ value in
  let value = Il.Ast.BoolV sub in
  (ctx, value)

(* Pattern match check expression evaluation *)

and eval_match_exp (ctx : Ctx.t) (exp : exp) (pattern : pattern) : Ctx.t * value
    =
  let ctx, value = eval_exp ctx exp in
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
  let value = Il.Ast.BoolV matches in
  (ctx, value)

(* Tuple expression evaluation *)

and eval_tuple_exp (ctx : Ctx.t) (exps : exp list) : Ctx.t * value =
  let ctx, values = eval_exps ctx exps in
  let value = Il.Ast.TupleV values in
  (ctx, value)

(* Case expression evaluation *)

and eval_case_exp (ctx : Ctx.t) (notexp : notexp) : Ctx.t * value =
  let mixop, exps = notexp in
  let ctx, values = eval_exps ctx exps in
  let value = Il.Ast.CaseV (mixop, values) in
  (ctx, value)

(* Struct expression evaluation *)

and eval_str_exp (ctx : Ctx.t) (fields : (atom * exp) list) : Ctx.t * value =
  let atoms, exps = List.split fields in
  let ctx, values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  let value = Il.Ast.StructV fields in
  (ctx, value)

(* Option expression evaluation *)

and eval_opt_exp (ctx : Ctx.t) (exp_opt : exp option) : Ctx.t * value =
  match exp_opt with
  | Some exp ->
      let ctx, value = eval_exp ctx exp in
      let value = Il.Ast.OptV (Some value) in
      (ctx, value)
  | None -> (ctx, OptV None)

(* List expression evaluation *)

and eval_list_exp (ctx : Ctx.t) (exps : exp list) : Ctx.t * value =
  let ctx, values = eval_exps ctx exps in
  let value = Il.Ast.ListV values in
  (ctx, value)

(* Cons expression evaluation *)

and eval_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) : Ctx.t * value =
  let ctx, value_h = eval_exp ctx exp_h in
  let ctx, value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  let value = Il.Ast.ListV (value_h :: values_t) in
  (ctx, value)

(* Concatenation expression evaluation *)

and eval_cat_exp (ctx : Ctx.t) (at : region) (exp_l : exp) (exp_r : exp) :
    Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value =
    match (value_l, value_r) with
    | TextV s_l, TextV s_r -> Il.Ast.TextV (s_l ^ s_r)
    | ListV values_l, ListV values_r -> Il.Ast.ListV (values_l @ values_r)
    | _ -> error at "concatenation expects either two texts or two lists"
  in
  (ctx, value)

(* Membership expression evaluation *)

and eval_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) : Ctx.t * value =
  let ctx, value_e = eval_exp ctx exp_e in
  let ctx, value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  let mem = List.exists (Value.eq value_e) values_s in
  let value = Il.Ast.BoolV mem in
  (ctx, value)

(* Length expression evaluation *)

and eval_len_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let len = value |> Value.get_list |> List.length |> Bigint.of_int in
  let value = Il.Ast.NumV (`Nat len) in
  (ctx, value)

(* Dot expression evaluation *)

and eval_dot_exp (ctx : Ctx.t) (exp_b : exp) (atom : atom) : Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let fields = Value.get_struct value_b in
  let value =
    fields
    |> List.map (fun (atom, value) -> (atom.it, value))
    |> List.assoc atom.it
  in
  (ctx, value)

(* Index expression evaluation *)

and eval_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) : Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let values = Value.get_list value_b in
  let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let value = List.nth values idx in
  (ctx, value)

(* Slice expression evaluation *)

and eval_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) (exp_n : exp) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let values = Value.get_list value_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let ctx, value_n = eval_exp ctx exp_n in
  let idx_n = value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let idx_h = idx_l + idx_n in
  let values_slice =
    List.mapi
      (fun idx value ->
        if idx_l <= idx && idx < idx_h then Some value else None)
      values
    |> List.filter_map Fun.id
  in
  let value = Il.Ast.ListV values_slice in
  (ctx, value)

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

and eval_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_f = eval_exp ctx exp_f in
  let value = eval_update_path value_b path value_f in
  (ctx, value)

(* Function call expression evaluation *)

and eval_call_exp (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
    : Ctx.t * value =
  invoke_func ctx id targs args

(* Conditional relation holds expression evaluation *)

and eval_hold_exp (ctx : Ctx.t) (id : id) (notexp : notexp) : Ctx.t * value =
  let _, exps_input = notexp in
  let ctx, values_input = eval_exps ctx exps_input in
  try
    let ctx, _ = invoke_rel ctx id values_input in
    (ctx, BoolV true)
  with _ -> (ctx, BoolV false)

(* Iterated expression evaluation *)

and eval_iter_exp_opt (ctx : Ctx.t) (exp : exp) (vars : var list) :
    Ctx.t * value =
  let ctx_sub_opt = Ctx.sub_opt ctx vars in
  match ctx_sub_opt with
  | Some ctx_sub ->
      let ctx_sub, value = eval_exp ctx_sub exp in
      let ctx = Ctx.commit ctx ctx_sub in
      let value = Il.Ast.OptV (Some value) in
      (ctx, value)
  | None -> (ctx, OptV None)

and eval_iter_exp_list (ctx : Ctx.t) (exp : exp) (vars : var list) :
    Ctx.t * value =
  let ctxs_sub = Ctx.sub_list ctx vars in
  let ctx, values =
    List.fold_left
      (fun (ctx, values) ctx_sub ->
        let ctx_sub, value = eval_exp ctx_sub exp in
        let ctx = Ctx.commit ctx ctx_sub in
        (ctx, values @ [ value ]))
      (ctx, []) ctxs_sub
  in
  let value = Il.Ast.ListV values in
  (ctx, value)

and eval_iter_exp (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) : Ctx.t * value
    =
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt ctx exp vars
  | List -> eval_iter_exp_list ctx exp vars

(* Argument evaluation *)

and eval_arg (ctx : Ctx.t) (arg : arg) : Ctx.t * value =
  match arg.it with ExpA exp -> eval_exp ctx exp | DefA id -> (ctx, FuncV id)

and eval_args (ctx : Ctx.t) (args : arg list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) arg ->
      let ctx, value = eval_arg ctx arg in
      (ctx, values @ [ value ]))
    (ctx, []) args

(* Instruction evaluation *)

and eval_instr (ctx : Ctx.t) (instr : instr) : Ctx.t * Sign.t =
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then, phantom_opt) ->
      eval_if_instr ctx exp_cond iterexps instrs_then phantom_opt
  | CaseI (exp, cases, phantom_opt) -> eval_case_instr ctx exp cases phantom_opt
  | OtherwiseI instr -> eval_instr ctx instr
  | LetI (exp_l, exp_r, iterexps) -> eval_let_instr ctx exp_l exp_r iterexps
  | RuleI (id, notexp, iterexps) -> eval_rule_instr ctx id notexp iterexps
  | ResultI exps -> eval_result_instr ctx exps
  | ReturnI exp -> eval_return_instr ctx exp

and eval_instrs (ctx : Ctx.t) (sign : Sign.t) (instrs : instr list) :
    Ctx.t * Sign.t =
  List.fold_left
    (fun (ctx, sign) instr ->
      match sign with Sign.Cont -> eval_instr ctx instr | _ -> (ctx, sign))
    (ctx, sign) instrs

(* If instruction evaluation *)

and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) : Ctx.t * bool =
  let ctx, value_cond = eval_exp ctx exp_cond in
  let cond = Value.get_bool value_cond in
  (ctx, cond)

and eval_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t * bool =
  let ctxs_sub = Ctx.sub_list ctx vars in
  List.fold_left
    (fun (ctx, cond) ctx_sub ->
      if not cond then (ctx, cond)
      else
        let ctx_sub, cond = eval_if_cond_iter' ctx_sub exp_cond iterexps in
        let ctx = Ctx.commit ctx ctx_sub in
        (ctx, cond))
    (ctx, true) ctxs_sub

and eval_if_cond_iter' (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    : Ctx.t * bool =
  match iterexps with
  | [] -> eval_if_cond ctx exp_cond
  | iterexp_h :: iterexps_t -> (
      let iter_h, vars_h = iterexp_h in
      match iter_h with
      | Opt -> error no_region "(TODO)"
      | List -> eval_if_cond_list ctx exp_cond vars_h iterexps_t)

and eval_if_cond_iter (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list) :
    Ctx.t * bool =
  let iterexps = List.rev iterexps in
  eval_if_cond_iter' ctx exp_cond iterexps

and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (instrs_then : instr list) (phantom_opt : phantom option) : Ctx.t * Sign.t =
  let ctx, cond = eval_if_cond_iter ctx exp_cond iterexps in
  let ctx =
    match phantom_opt with
    | Some (pid, _) -> Ctx.cover ctx (not cond) pid
    | None -> ctx
  in
  if cond then eval_instrs ctx Cont instrs_then else (ctx, Cont)

(* Case analysis instruction evaluation *)

and eval_cases (ctx : Ctx.t) (exp : exp) (cases : case list) :
    Ctx.t * instr list option =
  cases
  |> List.fold_left
       (fun (ctx, block_match) (guard, block) ->
         match block_match with
         | Some _ -> (ctx, block_match)
         | None ->
             let exp_cond =
               match guard with
               | BoolG true -> exp.it
               | BoolG false -> Il.Ast.UnE (`NotOp, `BoolT, exp)
               | CmpG (cmpop, optyp, exp_r) ->
                   Il.Ast.CmpE (cmpop, optyp, exp, exp_r)
               | SubG typ -> Il.Ast.SubE (exp, typ)
               | MatchG pattern -> Il.Ast.MatchE (exp, pattern)
             in
             let exp_cond = exp_cond $$ (exp.at, Il.Ast.BoolT) in
             let ctx, value_cond = eval_exp ctx exp_cond in
             let cond = Value.get_bool value_cond in
             if cond then (ctx, Some block) else (ctx, None))
       (ctx, None)

and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list)
    (phantom_opt : phantom option) : Ctx.t * Sign.t =
  let ctx, instrs_opt = eval_cases ctx exp cases in
  let ctx =
    match phantom_opt with
    | Some (pid, _) -> Ctx.cover ctx (Option.is_none instrs_opt) pid
    | None -> ctx
  in
  match instrs_opt with
  | Some instrs -> eval_instrs ctx Cont instrs
  | None -> (ctx, Cont)

(* Let instruction evaluation *)

and eval_let (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t =
  let ctx, value = eval_exp ctx exp_r in
  assign_exp ctx exp_l value

and eval_let_opt (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun (id, iters) ->
        Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.Opt ]))
      vars
  in
  let ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
  let ctx, values_binding =
    match ctx_sub_opt with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | None ->
        let values_binding =
          List.init (List.length vars_binding) (fun _ -> Il.Ast.OptV None)
        in
        (ctx, values_binding)
    (* Otherwise, evaluate the premise for the subcontext *)
    | Some ctx_sub ->
        let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
        let ctx = Ctx.commit ctx ctx_sub in
        let values_binding =
          List.map
            (fun var_binding ->
              let value_binding = Ctx.find_value Local ctx_sub var_binding in
              Il.Ast.OptV (Some value_binding))
            vars_binding
        in
        (ctx, values_binding)
  in
  (* Finally, bind the resulting values *)
  List.fold_left2
    (fun ctx (id, iters) value_binding ->
      Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value_binding)
    ctx vars_binding values_binding

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
  let ctx, values_binding =
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] ->
        let values_binding =
          List.init (List.length vars_binding) (fun _ -> [])
        in
        (ctx, values_binding)
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        let ctx, values_binding_batch =
          List.fold_left
            (fun (ctx, values_binding_batch) ctx_sub ->
              let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
              let ctx = Ctx.commit ctx ctx_sub in
              let value_binding_batch =
                List.map (Ctx.find_value Local ctx_sub) vars_binding
              in
              let values_binding_batch =
                values_binding_batch @ [ value_binding_batch ]
              in
              (ctx, values_binding_batch))
            (ctx, []) ctxs_sub
        in
        let values_binding = values_binding_batch |> Ctx.transpose in
        (ctx, values_binding)
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

(* Rule instruction evaluation *)

and eval_rule (ctx : Ctx.t) (id : id) (notexp : notexp) : Ctx.t =
  let rel = Ctx.find_rel Local ctx id in
  let exps_input, exps_output =
    let inputs, _, _ = rel in
    let _, exps = notexp in
    Hint.split_exps_without_idx inputs exps
  in
  let ctx, values_input = eval_exps ctx exps_input in
  let ctx, values_output = invoke_rel ctx id values_input in
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
  let ctx, values_binding =
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] ->
        let values_binding =
          List.init (List.length vars_binding) (fun _ -> [])
        in
        (ctx, values_binding)
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        let ctx, values_binding_batch =
          List.fold_left
            (fun (ctx, values_binding_batch) ctx_sub ->
              let ctx_sub = eval_rule_iter' ctx_sub id notexp iterexps in
              let ctx = Ctx.commit ctx ctx_sub in
              let value_binding_batch =
                List.map (Ctx.find_value Local ctx_sub) vars_binding
              in
              let values_binding_batch =
                values_binding_batch @ [ value_binding_batch ]
              in
              (ctx, values_binding_batch))
            (ctx, []) ctxs_sub
        in
        let values_binding = values_binding_batch |> Ctx.transpose in
        (ctx, values_binding)
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

(* Result instruction evaluation *)

and eval_result_instr (ctx : Ctx.t) (exps : exp list) : Ctx.t * Sign.t =
  let ctx, values = eval_exps ctx exps in
  (ctx, Res values)

(* Return instruction evaluation *)

and eval_return_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Sign.t =
  let ctx, value = eval_exp ctx exp in
  (ctx, Ret value)

(* Invoke a relation *)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
    Ctx.t * value list =
  let _inputs, exps_input, instrs = Ctx.find_rel Local ctx id in
  check (instrs <> []) id.at "relation has no instructions";
  let ctx_local = Ctx.localize ctx in
  let ctx_local = assign_exps ctx_local exps_input values_input in
  let ctx_local, sign = eval_instrs ctx_local Cont instrs in
  let ctx = Ctx.commit ctx ctx_local in
  match sign with
  | Res values -> (ctx, values)
  | _ -> error id.at "relation was not matched"

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    Ctx.t * value =
  if Builtin.is_builtin id then invoke_func_builtin ctx id targs args
  else invoke_func_def ctx id targs args

and invoke_func_builtin (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
  let ctx, values_input = eval_args ctx args in
  let value = Builtin.invoke id targs values_input in
  (ctx, value)

and invoke_func_def (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
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
  let ctx, values_input = eval_args ctx args in
  let ctx_local = assign_args ctx ctx_local args_input values_input in
  let ctx_local, sign = eval_instrs ctx_local Cont instrs in
  let ctx = Ctx.commit ctx ctx_local in
  match sign with
  | Ret value -> (ctx, value)
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

let run_typing ?(cover : Coverage.Cover.t ref = ref Coverage.Cover.empty)
    (spec : spec) (includes_p4 : string list) (filename_p4 : string) :
    Ctx.t * value list =
  Builtin.init ();
  let program = P4.In.in_program includes_p4 filename_p4 in
  let ctx = Ctx.empty filename_p4 cover in
  let ctx = load_spec ctx spec in
  invoke_rel ctx ("Prog_ok" $ no_region) [ program ]

let cover_typing (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) (dirname_closest_miss_opt : string option) : unit =
  let cover = ref Coverage.Cover.empty in
  List.iter
    (fun filename_p4 ->
      try run_typing ~cover spec includes_p4 filename_p4 |> ignore
      with _ -> ())
    filenames_p4;
  Coverage.log spec !cover dirname_closest_miss_opt
