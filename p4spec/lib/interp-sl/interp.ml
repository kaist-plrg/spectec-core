open Domain.Lib
open Xl
open Sl.Ast
module Hint = Runtime_static.Rel.Hint
module Typ = Runtime_dynamic_sl.Typ
module Value = Runtime_dynamic_sl.Value
module Rel = Runtime_dynamic_sl.Rel
open Runtime_dynamic_sl.Envs
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
module Cache = Cache.Cache
open Error
module F = Format
open Util.Source

(* Caches *)
let is_cached_func = function
  | "subst_typ" | "subst_typdef_poly" | "specialize_typdef" | "canon_typ"
  | "free_typ" | "is_nominal" | "find_map" | "update_map" | "dom_map"
  | "bound_tids" | "in_set" | "merge_cstr'" | "merge_cstr"
  | "find_matching_funcs" | "nestable_structt" | "nestable_structt_in_headert"
    ->
      true
  | _ -> false

let func_cache = ref (Cache.create 1000)

let is_cached_rule = function
  | "Sub_expl" | "Sub_expl_canon" | "Sub_expl_canon_neq" | "Sub_impl"
  | "Sub_impl_canon" | "Sub_impl_canon_neq" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false

let rule_cache = ref (Cache.create 50)
let ( let* ) = Option.bind

(* Assignments *)

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
  let note = value.note.typ in
  match (exp.it, value.it) with
  | VarE id, _ ->
      let ctx = Ctx.add_value Local ctx (id, []) value in
      ctx
  | TupleE exps_inner, TupleV values_inner ->
      let ctx = assign_exps ctx exps_inner values_inner in
      List.iter
        (fun value_inner -> Ctx.add_edge ctx value_inner value Dep.Edges.Assign)
        values_inner;
      ctx
  | CaseE notexp, CaseV (_mixop_value, values_inner) ->
      let _mixop_exp, exps_inner = notexp in
      let ctx = assign_exps ctx exps_inner values_inner in
      List.iter
        (fun value_inner -> Ctx.add_edge ctx value_inner value Dep.Edges.Assign)
        values_inner;
      ctx
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp_inner, Some value_inner ->
          let ctx = assign_exp ctx exp_inner value_inner in
          Ctx.add_edge ctx value_inner value Dep.Edges.Assign;
          ctx
      | None, None -> ctx
      | _ -> assert false)
  | ListE exps_inner, ListV values_inner ->
      let ctx = assign_exps ctx exps_inner values_inner in
      List.iter
        (fun value_inner -> Ctx.add_edge ctx value_inner value Dep.Edges.Assign)
        values_inner;
      ctx
  | ConsE (exp_h, exp_t), ListV values_inner ->
      let value_h = List.hd values_inner in
      let value_t =
        let vid = Dep.Graph.fresh () in
        let typ = note in
        ListV (List.tl values_inner) $$$ { vid; typ }
      in
      Ctx.add_node ctx value_t;
      let ctx = assign_exp ctx exp_h value_h in
      Ctx.add_edge ctx value_h value Dep.Edges.Assign;
      let ctx = assign_exp ctx exp_t value_t in
      Ctx.add_edge ctx value_t value Dep.Edges.Assign;
      ctx
  | IterE (_, (Opt, vars)), OptV None ->
      (* Per iterated variable, make an option out of the value *)
      List.fold_left
        (fun ctx (id, typ, iters) ->
          let value_sub =
            let vid = Dep.Graph.fresh () in
            let typ = Typ.iterate typ (iters @ [ Il.Ast.Opt ]) in
            OptV None $$$ { vid; typ = typ.it }
          in
          Ctx.add_node ctx value_sub;
          Ctx.add_edge ctx value_sub value Dep.Edges.Assign;
          Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value_sub)
        ctx vars
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let ctx = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      List.fold_left
        (fun ctx (id, typ, iters) ->
          let value_sub =
            let value = Ctx.find_value Local ctx (id, iters) in
            let vid = Dep.Graph.fresh () in
            let typ = Typ.iterate typ (iters @ [ Il.Ast.Opt ]) in
            OptV (Some value) $$$ { vid; typ = typ.it }
          in
          Ctx.add_node ctx value_sub;
          Ctx.add_edge ctx value_sub value Dep.Edges.Assign;
          Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value_sub)
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
        (fun ctx (id, typ, iters) ->
          let values =
            List.map (fun ctx -> Ctx.find_value Local ctx (id, iters)) ctxs
          in
          let value_sub =
            let vid = Dep.Graph.fresh () in
            let typ = Typ.iterate typ (iters @ [ Il.Ast.List ]) in
            ListV values $$$ { vid; typ = typ.it }
          in
          Ctx.add_node ctx value_sub;
          Ctx.add_edge ctx value_sub value Dep.Edges.Assign;
          Ctx.add_value Local ctx (id, iters @ [ Il.Ast.List ]) value_sub)
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
  match value.it with
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
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE b -> eval_bool_exp note ctx b
  | NumE n -> eval_num_exp note ctx n
  | TextE s -> eval_text_exp note ctx s
  | VarE id -> eval_var_exp ctx id
  | UnE (unop, optyp, exp) -> eval_un_exp note ctx unop optyp exp
  | BinE (binop, optyp, exp_l, exp_r) ->
      eval_bin_exp note ctx binop optyp exp_l exp_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      eval_cmp_exp note ctx cmpop optyp exp_l exp_r
  | UpCastE (typ, exp) -> eval_upcast_exp note ctx typ exp
  | DownCastE (typ, exp) -> eval_downcast_exp note ctx typ exp
  | SubE (exp, typ) -> eval_sub_exp note ctx exp typ
  | MatchE (exp, pattern) -> eval_match_exp note ctx exp pattern
  | TupleE exps -> eval_tuple_exp note ctx exps
  | CaseE notexp -> eval_case_exp note ctx notexp
  | StrE fields -> eval_str_exp note ctx fields
  | OptE exp_opt -> eval_opt_exp note ctx exp_opt
  | ListE exps -> eval_list_exp note ctx exps
  | ConsE (exp_h, exp_t) -> eval_cons_exp note ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> eval_cat_exp note ctx at exp_l exp_r
  | MemE (exp_e, exp_s) -> eval_mem_exp note ctx exp_e exp_s
  | LenE exp -> eval_len_exp note ctx exp
  | DotE (exp_b, atom) -> eval_dot_exp note ctx exp_b atom
  | IdxE (exp_b, exp_i) -> eval_idx_exp note ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> eval_slice_exp note ctx exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> eval_upd_exp note ctx exp_b path exp_f
  | CallE (id, targs, args) -> eval_call_exp note ctx id targs args
  | HoldE (id, notexp) -> eval_hold_exp note ctx id notexp
  | IterE (exp, iterexp) -> eval_iter_exp note ctx exp iterexp

and eval_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) exp ->
      let ctx, value = eval_exp ctx exp in
      (ctx, values @ [ value ]))
    (ctx, []) exps

(* Boolean expression evaluation *)

and eval_bool_exp (note : typ') (ctx : Ctx.t) (b : bool) : Ctx.t * value =
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    BoolV b $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  List.iter
    (fun value_input ->
      Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
    ctx.local.values_input;
  (ctx, value_res)

(* Numeric expression evaluation *)

and eval_num_exp (note : typ') (ctx : Ctx.t) (n : Num.t) : Ctx.t * value =
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    NumV n $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  List.iter
    (fun value_input ->
      Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
    ctx.local.values_input;
  (ctx, value_res)

(* Text expression evaluation *)

and eval_text_exp (note : typ') (ctx : Ctx.t) (s : string) : Ctx.t * value =
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    TextV s $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  List.iter
    (fun value_input ->
      Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
    ctx.local.values_input;
  (ctx, value_res)

(* Variable expression evaluation *)

and eval_var_exp (ctx : Ctx.t) (id : id) : Ctx.t * value =
  let value = Ctx.find_value Local ctx (id, []) in
  (ctx, value)

(* Unary expression evaluation *)

and eval_un_bool (unop : Bool.unop) (value : value) : value' =
  match unop with `NotOp -> BoolV (not (Value.get_bool value))

and eval_un_num (unop : Num.unop) (value : value) : value' =
  let num = Value.get_num value in
  let num = Num.un unop num in
  NumV num

and eval_un_exp (note : typ') (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
    (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value_res =
    match unop with
    | #Bool.unop as unop -> eval_un_bool unop value
    | #Num.unop as unop -> eval_un_num unop value
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    value_res $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value (Dep.Edges.Op (UnOp unop));
  (ctx, value_res)

(* Binary expression evaluation *)

and eval_bin_bool (binop : Bool.binop) (value_l : value) (value_r : value) :
    value' =
  let bool_l = Value.get_bool value_l in
  let bool_r = Value.get_bool value_r in
  match binop with
  | `AndOp -> BoolV (bool_l && bool_r)
  | `OrOp -> BoolV (bool_l || bool_r)
  | `ImplOp -> BoolV ((not bool_l) || bool_r)
  | `EquivOp -> BoolV (bool_l = bool_r)

and eval_bin_num (binop : Num.binop) (value_l : value) (value_r : value) :
    value' =
  let num_l = Value.get_num value_l in
  let num_r = Value.get_num value_r in
  NumV (Num.bin binop num_l num_r)

and eval_bin_exp (note : typ') (ctx : Ctx.t) (binop : binop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match binop with
    | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
    | #Num.binop as binop -> eval_bin_num binop value_l value_r
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    value_res $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value_l (Dep.Edges.Op (BinOp binop));
  Ctx.add_edge ctx value_res value_r (Dep.Edges.Op (BinOp binop));
  (ctx, value_res)

(* Comparison expression evaluation *)

and eval_cmp_bool (cmpop : Bool.cmpop) (value_l : value) (value_r : value) :
    value' =
  let eq = Value.eq value_l value_r in
  match cmpop with `EqOp -> BoolV eq | `NeOp -> BoolV (not eq)

and eval_cmp_num (cmpop : Num.cmpop) (value_l : value) (value_r : value) :
    value' =
  let num_l = Value.get_num value_l in
  let num_r = Value.get_num value_r in
  BoolV (Num.cmp cmpop num_l num_r)

and eval_cmp_exp (note : typ') (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match cmpop with
    | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
    | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    value_res $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value_l (Dep.Edges.Op (CmpOp cmpop));
  Ctx.add_edge ctx value_res value_r (Dep.Edges.Op (CmpOp cmpop));
  (ctx, value_res)

(* Upcast expression evaluation *)

and upcast (ctx : Ctx.t) (typ : typ) (value : value) : Ctx.t * value =
  match typ.it with
  | NumT `IntT -> (
      match value.it with
      | NumV (`Nat n) ->
          let value_res =
            let vid = Dep.Graph.fresh () in
            let typ = typ.it in
            NumV (`Int n) $$$ { vid; typ }
          in
          Ctx.add_node ctx value_res;
          Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
          (ctx, value_res)
      | NumV (`Int _) -> (ctx, value)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match deftyp.it with
      | PlainT typ ->
          let typ = Typ.subst_typ theta typ in
          upcast ctx typ value
      | _ -> (ctx, value))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          let ctx, values =
            List.fold_left2
              (fun (ctx, values) typ value ->
                let ctx, value = upcast ctx typ value in
                (ctx, values @ [ value ]))
              (ctx, []) typs values
          in
          let value_res =
            let vid = Dep.Graph.fresh () in
            let typ = typ.it in
            TupleV values $$$ { vid; typ }
          in
          Ctx.add_node ctx value_res;
          Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
          (ctx, value_res)
      | _ -> assert false)
  | _ -> (ctx, value)

and eval_upcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let ctx, value_res = upcast ctx typ value in
  (ctx, value_res)

(* Downcast expression evaluation *)

and downcast (ctx : Ctx.t) (typ : typ) (value : value) : Ctx.t * value =
  match typ.it with
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> (ctx, value)
      | NumV (`Int i) when Bigint.(i >= zero) ->
          let value_res =
            let vid = Dep.Graph.fresh () in
            let typ = typ.it in
            NumV (`Nat i) $$$ { vid; typ }
          in
          Ctx.add_node ctx value_res;
          Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
          (ctx, value_res)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match deftyp.it with
      | PlainT typ ->
          let typ = Typ.subst_typ theta typ in
          downcast ctx typ value
      | _ -> (ctx, value))
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          let ctx, values =
            List.fold_left2
              (fun (ctx, values) typ value ->
                let ctx, value = downcast ctx typ value in
                (ctx, values @ [ value ]))
              (ctx, []) typs values
          in
          let value_res =
            let vid = Dep.Graph.fresh () in
            let typ = typ.it in
            TupleV values $$$ { vid; typ }
          in
          Ctx.add_node ctx value_res;
          Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
          (ctx, value_res)
      | _ -> assert false)
  | _ -> (ctx, value)

and eval_downcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let ctx, value = downcast ctx typ value in
  (ctx, value)

(* Subtype check expression evaluation *)

and subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
  match typ.it with
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> true
      | NumV (`Int i) -> Bigint.(i >= zero)
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value.it) with
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
      match value.it with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (subtyp ctx) typs values
      | _ -> false)
  | _ -> true

and eval_sub_exp (note : typ') (ctx : Ctx.t) (exp : exp) (typ : typ) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let sub = subtyp ctx typ value in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    BoolV sub $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value (Dep.Edges.Op (SubOp typ));
  (ctx, value_res)

(* Pattern match check expression evaluation *)

and eval_match_exp (note : typ') (ctx : Ctx.t) (exp : exp) (pattern : pattern) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let matches =
    match (pattern, value.it) with
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
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    BoolV matches $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value (Dep.Edges.Op (MatchOp pattern));
  (ctx, value_res)

(* Tuple expression evaluation *)

and eval_tuple_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    TupleV values $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  if List.length values = 0 then
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      ctx.local.values_input;
  (ctx, value_res)

(* Case expression evaluation *)

and eval_case_exp (note : typ') (ctx : Ctx.t) (notexp : notexp) : Ctx.t * value
    =
  let mixop, exps = notexp in
  let ctx, values = eval_exps ctx exps in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    CaseV (mixop, values) $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  if List.length values = 0 then
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      ctx.local.values_input;
  (ctx, value_res)

(* Struct expression evaluation *)

and eval_str_exp (note : typ') (ctx : Ctx.t) (fields : (atom * exp) list) :
    Ctx.t * value =
  let atoms, exps = List.split fields in
  let ctx, values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    StructV fields $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  if List.length values = 0 then
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      ctx.local.values_input;
  (ctx, value_res)

(* Option expression evaluation *)

and eval_opt_exp (note : typ') (ctx : Ctx.t) (exp_opt : exp option) :
    Ctx.t * value =
  let ctx, value_opt =
    match exp_opt with
    | Some exp ->
        let ctx, value = eval_exp ctx exp in
        (ctx, Some value)
    | None -> (ctx, None)
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    OptV value_opt $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  if Option.is_none value_opt then
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      ctx.local.values_input;
  (ctx, value_res)

(* List expression evaluation *)

and eval_list_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    ListV values $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  if List.length values = 0 then
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      ctx.local.values_input;
  (ctx, value_res)

(* Cons expression evaluation *)

and eval_cons_exp (note : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    Ctx.t * value =
  let ctx, value_h = eval_exp ctx exp_h in
  let ctx, value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    ListV (value_h :: values_t) $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  (ctx, value_res)

(* Concatenation expression evaluation *)

and eval_cat_exp (note : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
    (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match (value_l.it, value_r.it) with
    | TextV s_l, TextV s_r -> TextV (s_l ^ s_r)
    | ListV values_l, ListV values_r -> ListV (values_l @ values_r)
    | _ -> error at "concatenation expects either two texts or two lists"
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    value_res $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value_l (Dep.Edges.Op CatOp);
  Ctx.add_edge ctx value_res value_r (Dep.Edges.Op CatOp);
  (ctx, value_res)

(* Membership expression evaluation *)

and eval_mem_exp (note : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    Ctx.t * value =
  let ctx, value_e = eval_exp ctx exp_e in
  let ctx, value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    BoolV (List.exists (Value.eq value_e) values_s) $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value_e (Dep.Edges.Op MemOp);
  Ctx.add_edge ctx value_res value_s (Dep.Edges.Op MemOp);
  (ctx, value_res)

(* Length expression evaluation *)

and eval_len_exp (note : typ') (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let len = value |> Value.get_list |> List.length |> Bigint.of_int in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    NumV (`Nat len) $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  Ctx.add_edge ctx value_res value (Dep.Edges.Op LenOp);
  (ctx, value_res)

(* Dot expression evaluation *)

and eval_dot_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (atom : atom) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let fields = Value.get_struct value_b in
  let value_res =
    fields
    |> List.map (fun (atom, value) -> (atom.it, value))
    |> List.assoc atom.it
  in
  (ctx, value_res)

(* Index expression evaluation *)

and eval_idx_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let values = Value.get_list value_b in
  let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let value_res = List.nth values idx in
  (ctx, value_res)

(* Slice expression evaluation *)

and eval_slice_exp (note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
    (exp_n : exp) : Ctx.t * value =
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
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    ListV values_slice $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  (ctx, value_res)

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

and eval_update_path (ctx : Ctx.t) (value_b : value) (path : path)
    (value_n : value) : value =
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
      let value =
        let vid = Dep.Graph.fresh () in
        let typ = path.note in
        StructV fields $$$ { vid; typ }
      in
      Ctx.add_node ctx value;
      eval_update_path ctx value_b path value
  | _ -> failwith "(TODO eval_update_path)"

and eval_upd_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (path : path)
    (exp_f : exp) : Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_f = eval_exp ctx exp_f in
  let value_res = eval_update_path ctx value_b path value_f in
  (ctx, value_res)

(* Function call expression evaluation *)

and eval_call_exp (_note : typ') (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
  let ctx, value_res = invoke_func ctx id targs args in
  (ctx, value_res)

(* Conditional relation holds expression evaluation *)

and eval_hold_exp (note : typ') (ctx : Ctx.t) (id : id) (notexp : notexp) :
    Ctx.t * value =
  let _, exps_input = notexp in
  let ctx, values_input = eval_exps ctx exps_input in
  let ctx, hold =
    match invoke_rel ctx id values_input with
    | Some (ctx, _) -> (ctx, true)
    | None -> (ctx, false)
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    BoolV hold $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  List.iteri
    (fun idx value_input ->
      Ctx.add_edge ctx value_res value_input (Dep.Edges.Rel (id, idx)))
    values_input;
  (ctx, value_res)

(* Iterated expression evaluation *)

and eval_iter_exp_opt (note : typ') (ctx : Ctx.t) (exp : exp) (vars : var list)
    : Ctx.t * value =
  let ctx_sub_opt = Ctx.sub_opt ctx vars in
  let ctx, value_res =
    match ctx_sub_opt with
    | Some ctx_sub ->
        let ctx_sub, value = eval_exp ctx_sub exp in
        let ctx = Ctx.commit ctx ctx_sub in
        let value_res =
          let vid = Dep.Graph.fresh () in
          let typ = note in
          OptV (Some value) $$$ { vid; typ }
        in
        (ctx, value_res)
    | None ->
        let value_res =
          let vid = Dep.Graph.fresh () in
          let typ = note in
          OptV None $$$ { vid; typ }
        in
        (ctx, value_res)
  in
  Ctx.add_node ctx value_res;
  List.iter
    (fun (id, _typ, iters) ->
      let value_sub = Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ]) in
      Ctx.add_edge ctx value_res value_sub Dep.Edges.Iter)
    vars;
  (ctx, value_res)

and eval_iter_exp_list (note : typ') (ctx : Ctx.t) (exp : exp) (vars : var list)
    : Ctx.t * value =
  let ctxs_sub = Ctx.sub_list ctx vars in
  let ctx, values =
    List.fold_left
      (fun (ctx, values) ctx_sub ->
        let ctx_sub, value = eval_exp ctx_sub exp in
        let ctx = Ctx.commit ctx ctx_sub in
        (ctx, values @ [ value ]))
      (ctx, []) ctxs_sub
  in
  let value_res =
    let vid = Dep.Graph.fresh () in
    let typ = note in
    ListV values $$$ { vid; typ }
  in
  Ctx.add_node ctx value_res;
  List.iter
    (fun (id, _typ, iters) ->
      let value_sub = Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ]) in
      Ctx.add_edge ctx value_res value_sub Dep.Edges.Iter)
    vars;
  (ctx, value_res)

and eval_iter_exp (note : typ') (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) :
    Ctx.t * value =
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt note ctx exp vars
  | List -> eval_iter_exp_list note ctx exp vars

(* Argument evaluation *)

and eval_arg (ctx : Ctx.t) (arg : arg) : Ctx.t * value =
  match arg.it with
  | ExpA exp -> eval_exp ctx exp
  | DefA id ->
      let value_res =
        let vid = Dep.Graph.fresh () in
        let typ = Il.Ast.FuncT in
        FuncV id $$$ { vid; typ }
      in
      Ctx.add_node ctx value_res;
      (ctx, value_res)

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

and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) : Ctx.t * bool * value =
  let ctx, value_cond = eval_exp ctx exp_cond in
  let cond = Value.get_bool value_cond in
  (ctx, cond, value_cond)

and eval_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t * bool * value list =
  let ctxs_sub = Ctx.sub_list ctx vars in
  List.fold_left
    (fun (ctx, cond, values_cond) ctx_sub ->
      if not cond then (ctx, cond, values_cond)
      else
        let ctx_sub, cond, value_cond =
          eval_if_cond_iter' ctx_sub exp_cond iterexps
        in
        let ctx = Ctx.commit ctx ctx_sub in
        let values_cond = values_cond @ [ value_cond ] in
        (ctx, cond, values_cond))
    (ctx, true, []) ctxs_sub

and eval_if_cond_iter' (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    : Ctx.t * bool * value =
  match iterexps with
  | [] -> eval_if_cond ctx exp_cond
  | iterexp_h :: iterexps_t -> (
      let iter_h, vars_h = iterexp_h in
      match iter_h with
      | Opt -> error no_region "(TODO)"
      | List ->
          let ctx, cond, values_cond =
            eval_if_cond_list ctx exp_cond vars_h iterexps_t
          in
          let value_cond =
            let vid = Dep.Graph.fresh () in
            let typ = Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List) in
            ListV values_cond $$$ { vid; typ }
          in
          Ctx.add_node ctx value_cond;
          List.iter
            (fun (id, _typ, iters) ->
              let value_sub =
                Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
              in
              Ctx.add_edge ctx value_cond value_sub Dep.Edges.Iter)
            vars_h;
          (ctx, cond, value_cond))

and eval_if_cond_iter (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list) :
    Ctx.t * bool * value =
  let iterexps = List.rev iterexps in
  eval_if_cond_iter' ctx exp_cond iterexps

and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (instrs_then : instr list) (phantom_opt : phantom option) : Ctx.t * Sign.t =
  let ctx, cond, value_cond = eval_if_cond_iter ctx exp_cond iterexps in
  let vid = value_cond.note.vid in
  let ctx =
    match phantom_opt with
    | Some (pid, _) -> Ctx.cover ctx (not cond) pid vid
    | None -> ctx
  in
  if cond then eval_instrs ctx Cont instrs_then else (ctx, Cont)

(* Case analysis instruction evaluation *)

and eval_cases (ctx : Ctx.t) (exp : exp) (cases : case list) :
    Ctx.t * instr list option * value =
  cases
  |> List.fold_left
       (fun (ctx, block_match, values_cond) (guard, block) ->
         match block_match with
         | Some _ -> (ctx, block_match, values_cond)
         | None ->
             let exp_cond =
               match guard with
               | BoolG true -> exp.it
               | BoolG false -> Il.Ast.UnE (`NotOp, `BoolT, exp)
               | CmpG (cmpop, optyp, exp_r) ->
                   Il.Ast.CmpE (cmpop, optyp, exp, exp_r)
               | SubG typ -> Il.Ast.SubE (exp, typ)
               | MatchG pattern -> Il.Ast.MatchE (exp, pattern)
               | MemG exp_s -> Il.Ast.MemE (exp, exp_s)
             in
             let exp_cond = exp_cond $$ (exp.at, Il.Ast.BoolT) in
             let ctx, value_cond = eval_exp ctx exp_cond in
             let values_cond = values_cond @ [ value_cond ] in
             let cond = Value.get_bool value_cond in
             if cond then (ctx, Some block, values_cond)
             else (ctx, None, values_cond))
       (ctx, None, [])
  |> fun (ctx, block_match, values_cond) ->
  let value_cond =
    let vid = Dep.Graph.fresh () in
    let typ = Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List) in
    ListV values_cond $$$ { vid; typ }
  in
  Ctx.add_node ctx value_cond;
  (ctx, block_match, value_cond)

and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list)
    (phantom_opt : phantom option) : Ctx.t * Sign.t =
  let ctx, instrs_opt, value_cond = eval_cases ctx exp cases in
  let vid = value_cond.note.vid in
  let ctx =
    match phantom_opt with
    | Some (pid, _) -> Ctx.cover ctx (Option.is_none instrs_opt) pid vid
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
      (fun (id, _typ, iters) ->
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
          List.map
            (fun (_id_binding, typ_binding, iters_binding) ->
              let value_binding =
                let vid = Dep.Graph.fresh () in
                let typ =
                  Typ.iterate typ_binding (iters_binding @ [ Il.Ast.Opt ])
                in
                OptV None $$$ { vid; typ = typ.it }
              in
              Ctx.add_node ctx value_binding;
              List.iter
                (fun (id, _typ, iters) ->
                  let value_sub =
                    Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ])
                  in
                  Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
                vars_bound;
              value_binding)
            vars_binding
        in
        (ctx, values_binding)
    (* Otherwise, evaluate the premise for the subcontext *)
    | Some ctx_sub ->
        let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
        let ctx = Ctx.commit ctx ctx_sub in
        let values_binding =
          List.map
            (fun (id_binding, typ_binding, iters_binding) ->
              let value_binding =
                Ctx.find_value Local ctx_sub (id_binding, iters_binding)
              in
              let value_binding =
                let vid = Dep.Graph.fresh () in
                let typ =
                  Typ.iterate typ_binding (iters_binding @ [ Il.Ast.Opt ])
                in
                OptV (Some value_binding) $$$ { vid; typ = typ.it }
              in
              Ctx.add_node ctx value_binding;
              List.iter
                (fun (id, _typ, iters) ->
                  let value_sub =
                    Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ])
                  in
                  Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
                vars_bound;
              value_binding)
            vars_binding
        in
        (ctx, values_binding)
  in
  (* Finally, bind the resulting values *)
  List.fold_left2
    (fun ctx (id_binding, _typ_binding, iters_binding) value_binding ->
      Ctx.add_value Local ctx
        (id_binding, iters_binding @ [ Il.Ast.Opt ])
        value_binding)
    ctx vars_binding values_binding

and eval_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun (id, _typ, iters) ->
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
                List.map
                  (fun (id_binding, _typ_binding, iters_binding) ->
                    Ctx.find_value Local ctx_sub (id_binding, iters_binding))
                  vars_binding
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
    (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
      let value_binding =
        let vid = Dep.Graph.fresh () in
        let typ = Typ.iterate typ_binding (iters_binding @ [ Il.Ast.List ]) in
        ListV values_binding $$$ { vid; typ = typ.it }
      in
      Ctx.add_node ctx value_binding;
      List.iter
        (fun (id, _typ, iters) ->
          let value_sub =
            Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
          in
          Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
        vars_bound;
      Ctx.add_value Local ctx
        (id_binding, iters_binding @ [ Il.Ast.List ])
        value_binding)
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
  let ctx, values_output =
    match invoke_rel ctx id values_input with
    | Some (ctx, values_output) -> (ctx, values_output)
    | None -> error id.at "relation was not matched"
  in
  assign_exps ctx exps_output values_output

and eval_rule_opt (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
    (_vars : var list) (_iterexps : iterexp list) : Ctx.t =
  failwith "(TODO) eval_rule_opt"

and eval_rule_list (ctx : Ctx.t) (id : id) (notexp : notexp) (vars : var list)
    (iterexps : iterexp list) : Ctx.t =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun (id, _typ, iters) ->
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
                List.map
                  (fun (id_binding, _typ_binding, iters_binding) ->
                    Ctx.find_value Local ctx_sub (id_binding, iters_binding))
                  vars_binding
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
    (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
      let value_binding =
        let vid = Dep.Graph.fresh () in
        let typ = Typ.iterate typ_binding (iters_binding @ [ Il.Ast.List ]) in
        ListV values_binding $$$ { vid; typ = typ.it }
      in
      Ctx.add_node ctx value_binding;
      List.iter
        (fun (id, _typ, iters) ->
          let value_sub =
            Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
          in
          Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
        vars_bound;
      Ctx.add_value Local ctx
        (id_binding, iters_binding @ [ Il.Ast.List ])
        value_binding)
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
    (Ctx.t * value list) option =
  let _inputs, exps_input, instrs = Ctx.find_rel Local ctx id in
  check (instrs <> []) id.at "relation has no instructions";
  let attempt_rules () =
    let ctx_local = Ctx.localize ctx in
    let ctx_local = Ctx.localize_inputs ctx_local values_input in
    let ctx_local = assign_exps ctx_local exps_input values_input in
    let ctx_local, sign = eval_instrs ctx_local Cont instrs in
    let ctx = Ctx.commit ctx ctx_local in
    match sign with
    | Res values_output ->
        List.iteri
          (fun idx_arg value_input ->
            List.iter
              (fun value_output ->
                Ctx.add_edge ctx value_output value_input
                  (Dep.Edges.Rel (id, idx_arg)))
              values_output)
          values_input;
        Some (ctx, values_output)
    | _ -> None
  in
  if (not ctx.derive) && is_cached_rule id.it then (
    let cache_result = Cache.find_opt !rule_cache (id.it, values_input) in
    match cache_result with
    | Some values_output -> Some (ctx, values_output)
    | None ->
        let* ctx, values_output = attempt_rules () in
        Cache.add !rule_cache (id.it, values_input) values_output;
        Some (ctx, values_output))
  else attempt_rules ()

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    Ctx.t * value =
  if Builtin.is_builtin id then invoke_func_builtin ctx id targs args
  else invoke_func_def ctx id targs args

and invoke_func_builtin (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
  let ctx, values_input = eval_args ctx args in
  let value_output = Builtin.invoke ctx id targs values_input in
  List.iteri
    (fun idx_arg value_input ->
      Ctx.add_edge ctx value_output value_input (Dep.Edges.Func (id, idx_arg)))
    values_input;
  (ctx, value_output)

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
  let attempt_clauses () =
    let ctx_local = Ctx.localize_inputs ctx_local values_input in
    let ctx_local = assign_args ctx ctx_local args_input values_input in
    let ctx_local, sign = eval_instrs ctx_local Cont instrs in
    let ctx = Ctx.commit ctx ctx_local in
    match sign with
    | Ret value_output ->
        List.iteri
          (fun idx_arg value_input ->
            Ctx.add_edge ctx value_output value_input
              (Dep.Edges.Func (id, idx_arg)))
          values_input;
        (ctx, value_output)
    | _ -> error id.at "function was not matched"
  in
  if (not ctx.derive) && is_cached_func id.it then (
    let cache_result = Cache.find_opt !func_cache (id.it, values_input) in
    match cache_result with
    | Some value_output -> (ctx, value_output)
    | None ->
        let ctx, value_output = attempt_clauses () in
        Cache.add !func_cache (id.it, values_input) value_output;
        (ctx, value_output))
  else attempt_clauses ()

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
