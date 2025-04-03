open Domain.Lib
open Xl
open Il.Ast
module Hint = Runtime_static.Rel.Hint
module Typ = Runtime_dynamic.Typ
module Value = Runtime_dynamic.Value
module Rel = Runtime_dynamic.Rel
open Runtime_dynamic.Envs
open Error
open Attempt
module F = Format
open Util.Source
module Cache = Cache.Cache
module Pp = Il.Print

(* Assignments *)

(* Subtype checks that are not guaranteed by the type system,
    because in SpecTec assignment should be able to revert the type cast expression

     - Numeric subtyping:
       - e.g., -- if (int) n = $foo() when $foo() returns a positive integer +2
     - Variant subtyping:
       - e.g., -- if (typ) objtyp = $foo() when $foo() returns a variant of objtyp specifically
     - Tuple subtyping: recursive, but the type system guarantees that their lengths are equal
     - Iteration subtyping

   Note that structs are invariant in SpecTec, so we do not need to check for subtyping *)

let cache = ref (Cache.create 101)
let is_cache_enabled = ref false
let hits = ref 0

(* module CachedFuncs = Set.Make (String)
let cached_funcs = CachedFuncs.of_list
    [ "specialize_typdef";
      "free_typ";
      "nestable_structt";
      "canon_typ";
      "subst_typ";
      "is_nominal";
      "nestable_structt_in_headert" ] *)

let is_func_cached = function
  | "specialize_typdef" 
  | "free_typ"
  | "nestable_structt"
  | "canon_typ"
  | "subst_typ"
  | "is_nominal"
  | "find_map"
  | "nestable_structt_in_headert" -> true
  | _ -> false

let rec downcast (ctx : Ctx.t) (typ : typ) (value : value) : value attempt =
  match typ.it with
  | NumT `NatT -> (
      match value with
      | NumV (`Nat _) -> Ok value
      | NumV (`Int i) ->
          let* _ =
            check_fail
              Bigint.(i >= zero)
              typ.at "cannot downcast a negative integer to natural number"
          in
          Ok (NumV (`Nat i))
      | _ -> assert false)
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value) with
      | PlainT typ, _ ->
          let typ = Typ.subst_typ theta typ in
          downcast ctx typ value
      | VariantT typcases, CaseV (mixop_v, _) ->
          let* _ =
            check_fail
              (List.exists
                 (fun nottyp ->
                   let mixop_t, _ = nottyp.it in
                   Mixop.eq mixop_t mixop_v)
                 typcases)
              typ.at
              (Format.asprintf "cannot downcast %s to %s"
                 (Il.Print.string_of_value ~short:true value)
                 (Il.Print.string_of_typ typ))
          in
          Ok value
      | _ -> Ok value)
  | TupleT typs -> (
      match value with
      | TupleV values ->
          let* values = downcasts ctx typs values in
          Ok (TupleV values)
      | _ -> assert false)
  | _ -> Ok value

and downcasts (ctx : Ctx.t) (typs : typ list) (values : value list) :
    value list attempt =
  List.fold_left2
    (fun values typ value ->
      let* values = values in
      let* value = downcast ctx typ value in
      Ok (values @ [ value ]))
    (Ok []) typs values

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t attempt =
  match (exp.it, value) with
  | VarE id, _ ->
      let ctx = Ctx.add_value Local ctx (id, []) value in
      Ok ctx
  | TupleE exps, TupleV values -> assign_exps ctx exps values
  | CaseE notexp, CaseV (mixop_value, values) ->
      let mixop_exp, exps = notexp in
      let mixop_exp = List.map (List.map it) mixop_exp in
      let mixop_value = List.map (List.map it) mixop_value in
      if List.compare (List.compare Atom.compare) mixop_exp mixop_value <> 0
      then
        fail exp.at
          (F.asprintf "mismatch in case expression: %s expected but got %s"
             (Il.Print.string_of_exp exp)
             (Il.Print.string_of_value ~short:true value))
      else assign_exps ctx exps values
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp, Some value -> assign_exp ctx exp value
      | None, None -> Ok ctx
      | Some _, None ->
          fail exp.at
            (F.asprintf "cannot assign a none value into %s"
               (Il.Print.string_of_exp exp))
      | None, Some _ ->
          fail exp.at
            (F.asprintf "cannot assign a value %s into a none expression"
               (Il.Print.string_of_value ~short:true value)))
  | ListE exps, ListV values -> assign_exps ctx exps values
  | ConsE (exp_h, exp_t), ListV values ->
      if values = [] then
        fail exp.at "cannot assign an empty list into a cons expression"
      else
        let value_h = List.hd values in
        let value_t = ListV (List.tl values) in
        let* ctx = assign_exp ctx exp_h value_h in
        assign_exp ctx exp_t value_t
  | IterE (_, (Opt, vars)), OptV None ->
      let ctx =
        List.fold_left
          (fun ctx (id, iters) ->
            let value = OptV None in
            Ctx.add_value Local ctx (id, iters @ [ Opt ]) value)
          ctx vars
      in
      Ok ctx
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let* ctx = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      let ctx =
        List.fold_left
          (fun ctx (id, iters) ->
            let value =
              let value = Ctx.find_value Local ctx (id, iters) in
              OptV (Some value)
            in
            Ctx.add_value Local ctx (id, iters @ [ Opt ]) value)
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
            let ctx =
              { ctx with local = { ctx.local with venv = VEnv.empty } }
            in
            let* ctx = assign_exp ctx exp value in
            Ok (ctxs @ [ ctx ]))
          (Ok []) values
      in
      (* Per iterated variable, collect its elementwise value,
         then make a sequence out of them *)
      let ctx =
        List.fold_left
          (fun ctx (id, iters) ->
            let values =
              List.map (fun ctx -> Ctx.find_value Local ctx (id, iters)) ctxs
            in
            let value = ListV values in
            Ctx.add_value Local ctx (id, iters @ [ List ]) value)
          ctx vars
      in
      Ok ctx
  | CastE (exp, _), _ ->
      let typ_exp = exp.note $ exp.at in
      let* value = downcast ctx typ_exp value in
      assign_exp ctx exp value
  | _ ->
      fail exp.at
        (F.asprintf "(TODO) match failed %s <- %s"
           (Il.Print.string_of_exp exp)
           (Il.Print.string_of_value ~short:true value))

and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) :
    Ctx.t attempt =
  let* _ =
    check_fail
      (List.length exps = List.length values)
      (over_region (List.map at exps))
      (F.asprintf
         "mismatch in number of expressions and values while assigning, \
          expected %d value(s) but got %d"
         (List.length exps) (List.length values))
  in
  List.fold_left2
    (fun ctx exp value ->
      let* ctx = ctx in
      assign_exp ctx exp value)
    (Ok ctx) exps values

(* Assigning a value to an argument *)

and assign_arg (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (arg : arg)
    (value : value) : Ctx.t attempt =
  match arg.it with
  | ExpA exp -> assign_arg_exp ctx_callee exp value
  | DefA id -> assign_arg_def ctx_caller ctx_callee id value

and assign_args (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (args : arg list)
    (values : value list) : Ctx.t attempt =
  let* _ =
    check_fail
      (List.length args = List.length values)
      (over_region (List.map at args))
      (F.asprintf
         "mismatch in number of arguments and values while assigning, expected \
          %d value(s) but got %d"
         (List.length args) (List.length values))
  in
  List.fold_left2
    (fun ctx_callee arg value ->
      let* ctx_callee = ctx_callee in
      assign_arg ctx_caller ctx_callee arg value)
    (Ok ctx_callee) args values

and assign_arg_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t attempt =
  assign_exp ctx exp value

and assign_arg_def (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (id : id)
    (value : value) : Ctx.t attempt =
  match value with
  | FuncV id_f ->
      let func = Ctx.find_func Local ctx_caller id_f in
      let ctx_callee = Ctx.add_func Local ctx_callee id func in
      Ok ctx_callee
  | _ ->
      fail id.at
        (F.asprintf "cannot assign a value %s to a definition %s"
           (Il.Print.string_of_value ~short:true value)
           id.it)

(* Expression evaluation *)

let rec eval_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let wrap_ctx value = (ctx, value) in
  let at = exp.at in
  match exp.it with
  | BoolE b -> eval_bool_exp b |> wrap_ctx
  | NumE n -> eval_num_exp n |> wrap_ctx
  | TextE s -> eval_text_exp s |> wrap_ctx
  | VarE id -> eval_var_exp ctx id |> wrap_ctx
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
  | CatE (exp_l, exp_r) -> eval_cat_exp ctx at exp_l exp_r
  | MemE (exp_e, exp_s) -> eval_mem_exp ctx exp_e exp_s
  | SliceE (exp_b, exp_l, exp_h) -> eval_slice_exp ctx exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> eval_upd_exp ctx exp_b path exp_f
  | CallE (id, targs, args) -> eval_call_exp ctx id targs args
  | LenE exp -> eval_len_exp ctx exp
  | IdxE (exp_b, exp_i) -> eval_idx_exp ctx exp_b exp_i
  | IterE (exp, iterexp) -> eval_iter_exp ctx exp iterexp
  | CastE (exp, typ) -> eval_cast_exp ctx exp typ

and eval_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) exp ->
      let ctx, value = eval_exp ctx exp in
      (ctx, values @ [ value ]))
    (ctx, []) exps

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

and eval_un_exp (ctx : Ctx.t) (unop : unop) (_optyp : optyp) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  match unop with
  | #Bool.unop as unop ->
      let value = eval_un_bool unop value in
      (ctx, value)
  | #Num.unop as unop ->
      let value = eval_un_num unop value in
      (ctx, value)

(* Binary expression evaluation *)

and eval_bin_bool (binop : Bool.binop) (value_l : value) (value_r : value) :
    value =
  let bool_l = Value.get_bool value_l in
  let bool_r = Value.get_bool value_r in
  match binop with
  | `AndOp -> BoolV (bool_l && bool_r)
  | `OrOp -> BoolV (bool_l || bool_r)
  | `ImplOp -> BoolV ((not bool_l) || bool_r)
  | `EquivOp -> BoolV (bool_l = bool_r)

and eval_bin_num (binop : Num.binop) (value_l : value) (value_r : value) : value
    =
  let num_l = Value.get_num value_l in
  let num_r = Value.get_num value_r in
  let num = Num.bin binop num_l num_r in
  NumV num

and eval_bin_exp (ctx : Ctx.t) (binop : binop) (_optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value =
    match binop with
    | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
    | #Num.binop as binop -> eval_bin_num binop value_l value_r
  in
  (ctx, value)

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

(* Tuple expression evaluation *)

and eval_tuple_exp (ctx : Ctx.t) (exps : exp list) : Ctx.t * value =
  let ctx, values = eval_exps ctx exps in
  let value = TupleV values in
  (ctx, value)

(* Case expression evaluation *)

and eval_case_exp (ctx : Ctx.t) (notexp : notexp) : Ctx.t * value =
  let mixop, exps = notexp in
  let ctx, values = eval_exps ctx exps in
  let value = CaseV (mixop, values) in
  (ctx, value)

(* Option expression evaluation *)

and eval_opt_exp (ctx : Ctx.t) (exp_opt : exp option) : Ctx.t * value =
  match exp_opt with
  | Some exp ->
      let ctx, value = eval_exp ctx exp in
      let value = OptV (Some value) in
      (ctx, value)
  | None ->
      let value = OptV None in
      (ctx, value)

(* Struct expression evaluation *)

and eval_str_exp (ctx : Ctx.t) (fields : (atom * exp) list) : Ctx.t * value =
  let atoms, exps = List.split fields in
  let ctx, values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  let value = StructV fields in
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

(* List expression evaluation *)

and eval_list_exp (ctx : Ctx.t) (exps : exp list) : Ctx.t * value =
  let ctx, values = eval_exps ctx exps in
  let value = ListV values in
  (ctx, value)

(* Cons expression evaluation *)

and eval_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) : Ctx.t * value =
  let ctx, value_h = eval_exp ctx exp_h in
  let ctx, value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  let value = ListV (value_h :: values_t) in
  (ctx, value)

(* Concatenation expression evaluation *)

and eval_cat_exp (ctx : Ctx.t) (at : region) (exp_l : exp) (exp_r : exp) :
    Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value =
    match (value_l, value_r) with
    | TextV s_l, TextV s_r -> TextV (s_l ^ s_r)
    | ListV values_l, ListV values_r -> ListV (values_l @ values_r)
    | _ -> error at "concatenation expects either two texts or two lists"
  in
  (ctx, value)

(* Membership expression evaluation *)

and eval_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) : Ctx.t * value =
  let ctx, value_e = eval_exp ctx exp_e in
  let ctx, value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  let value = BoolV (List.exists (Value.eq value_e) values_s) in
  (ctx, value)

(* Length expression evaluation *)

and eval_len_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let len = value |> Value.get_list |> List.length |> Bigint.of_int in
  let value = NumV (`Nat len) in
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
  let value = ListV values_slice in
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
      let value = StructV fields in
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
  let+ ctx, value = invoke_func ctx id targs args in
  (ctx, value)

(* Iterated expression evaluation *)

and eval_iter_exp_opt (ctx : Ctx.t) (exp : exp) (vars : var list) :
    Ctx.t * value =
  let+ ctx_sub_opt = Ctx.sub_opt ctx vars in
  match ctx_sub_opt with
  | Some ctx_sub ->
      let ctx_sub = Ctx.trace_open_iter ctx_sub (Il.Print.string_of_exp exp) in
      let ctx_sub, value = eval_exp ctx_sub exp in
      let ctx_sub = Ctx.trace_close ctx_sub in
      let ctx = Ctx.trace_commit ctx ctx_sub.trace in
      let value = OptV (Some value) in
      (ctx, value)
  | None ->
      let value = OptV None in
      (ctx, value)

and eval_iter_exp_list (ctx : Ctx.t) (exp : exp) (vars : var list) :
    Ctx.t * value =
  let+ ctxs_sub = Ctx.sub_list ctx vars in
  let ctx, values =
    List.fold_left
      (fun (ctx, values) ctx_sub ->
        let ctx_sub =
          Ctx.trace_open_iter ctx_sub (Il.Print.string_of_exp exp)
        in
        let ctx_sub, value = eval_exp ctx_sub exp in
        let ctx_sub = Ctx.trace_close ctx_sub in
        let ctx = Ctx.trace_commit ctx ctx_sub.trace in
        (ctx, values @ [ value ]))
      (ctx, []) ctxs_sub
  in
  let value = ListV values in
  (ctx, value)

and eval_iter_exp (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) : Ctx.t * value
    =
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt ctx exp vars
  | List -> eval_iter_exp_list ctx exp vars

(* Cast expression evaluation *)

and cast (ctx : Ctx.t) (typ : typ) (value : value) : value =
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
          cast ctx typ value
      | _ -> value)
  | TupleT typs -> (
      match value with
      | TupleV values ->
          let values = List.map2 (cast ctx) typs values in
          TupleV values
      | _ -> assert false)
  | _ -> value

and eval_cast_exp (ctx : Ctx.t) (exp : exp) (typ : typ) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value = cast ctx typ value in
  (ctx, value)

(* Argument evaluation *)

and eval_arg (ctx : Ctx.t) (arg : arg) : Ctx.t * value =
  match arg.it with
  | ExpA exp -> eval_exp ctx exp
  | DefA id ->
      let value = FuncV id in
      (ctx, value)

and eval_args (ctx : Ctx.t) (args : arg list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) arg ->
      let ctx, value = eval_arg ctx arg in
      (ctx, values @ [ value ]))
    (ctx, []) args

(* Premise evaluation *)

and eval_prem (ctx : Ctx.t) (prem : prem) : Ctx.t attempt =
  let ctx = Ctx.trace_extend ctx prem in
  eval_prem' ctx prem

and eval_prem' (ctx : Ctx.t) (prem : prem) : Ctx.t attempt =
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
  let rel = Ctx.find_rel Local ctx id in
  let exps_input, exps_output =
    let _, inputs, _ = rel in
    let _, exps = notexp in
    Hint.split_exps_without_idx inputs exps
  in
  let ctx, values_input = eval_exps ctx exps_input in
  let* ctx, values_output = invoke_rel ctx id values_input in
  assign_exps ctx exps_output values_output

(* If premise evaluation *)

and eval_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t attempt =
  let ctx, value = eval_exp ctx exp in
  let cond = Value.get_bool value in
  if cond then Ok ctx
  else
    fail exp.at
      (F.asprintf "condition %s was not met" (Il.Print.string_of_exp exp))

(* Let premise evaluation *)

and eval_let_prem (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t attempt =
  let ctx, value = eval_exp ctx exp_r in
  assign_exp ctx exp_l value

(* Iterated premise evaluation *)

and eval_iter_prem_list (ctx : Ctx.t) (prem : prem) (vars : var list) :
    Ctx.t attempt =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun (id, iters) -> Ctx.bound_value Local ctx (id, iters @ [ List ]))
      vars
  in
  (* Create a subcontext for each batch of bound values *)
  let* ctxs_sub = Ctx.sub_list ctx vars_bound in
  let* ctx, values_binding =
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] ->
        let values_binding =
          List.init (List.length vars_binding) (fun _ -> [])
        in
        Ok (ctx, values_binding)
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        let* ctx, values_binding_batch =
          List.fold_left
            (fun ctx_values_binding_batch ctx_sub ->
              let* ctx, values_binding_batch = ctx_values_binding_batch in
              let ctx_sub =
                Ctx.trace_open_iter ctx_sub (Il.Print.string_of_prem prem)
              in
              let* ctx_sub = eval_prem ctx_sub prem in
              let ctx_sub = Ctx.trace_close ctx_sub in
              let ctx = Ctx.trace_commit ctx ctx_sub.trace in
              let value_binding_batch =
                List.map (Ctx.find_value Local ctx_sub) vars_binding
              in
              let values_binding_batch =
                values_binding_batch @ [ value_binding_batch ]
              in
              Ok (ctx, values_binding_batch))
            (Ok (ctx, []))
            ctxs_sub
        in
        let* values_binding = values_binding_batch |> Ctx.transpose in
        Ok (ctx, values_binding)
  in
  (* Finally, bind the resulting binding batches *)
  let ctx =
    List.fold_left2
      (fun ctx (id, iters) values_binding ->
        let value_binding = ListV values_binding in
        Ctx.add_value Local ctx (id, iters @ [ List ]) value_binding)
      ctx vars_binding values_binding
  in
  Ok ctx

and eval_iter_prem (ctx : Ctx.t) (prem : prem) (iterexp : iterexp) :
    Ctx.t attempt =
  let iter, vars = iterexp in
  match iter with
  | Opt -> error prem.at "(TODO) eval_iter_prem"
  | List -> eval_iter_prem_list ctx prem vars

(* Invoke a relation *)

and match_rule (ctx : Ctx.t) (inputs : Hint.t) (rule : rule)
    (values_input : value list) : (Ctx.t * prem list * exp list) attempt =
  let _, notexp, prems = rule.it in
  let exps_input, exps_output =
    let _, exps = notexp in
    Hint.split_exps_without_idx inputs exps
  in
  check
    (List.length exps_input = List.length values_input)
    rule.at "arity mismatch in rule";
  let* ctx = assign_exps ctx exps_input values_input in
  Ok (ctx, prems, exps_output)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
    (Ctx.t * value list) attempt =
  invoke_rel' ctx id values_input
  |> nest id.at (F.asprintf "invocation of relation %s failed" id.it)

and invoke_rel' (ctx : Ctx.t) (id : id) (values_input : value list) :
    (Ctx.t * value list) attempt =
  (* Find the relation *)
  let _, inputs, rules = Ctx.find_rel Local ctx id in
  guard (rules <> []) id.at "relation has no rules";
  (* Apply the first matching rule *)
  let attempt_rules =
    List.map
      (fun rule ->
        let id_rule, _, _ = rule.it in
        let attempt_rule' (ctx_local : Ctx.t) (prems : prem list)
            (exps_output : exp list) : (Ctx.t * value list) attempt =
          let* ctx_local = eval_prems ctx_local prems in
          let ctx_local, values_output = eval_exps ctx_local exps_output in
          let ctx_local = Ctx.trace_close ctx_local in
          let ctx = Ctx.trace_commit ctx ctx_local.trace in
          Ok (ctx, values_output)
        in
        let attempt_rule () : (Ctx.t * value list) attempt =
          (* Create a subtrace for the rule *)
          let ctx_local = Ctx.localize ctx in
          let ctx_local =
            Ctx.trace_open_rel ctx_local id id_rule values_input
          in
          (* Try to match the rule *)
          let* ctx_local, prems, exps_output =
            match_rule ctx_local inputs rule values_input
          in
          (* Try evaluating the rule *)
          attempt_rule' ctx_local prems exps_output
          |> nest id.at
               (F.asprintf "application of rule %s/%s failed" id.it id_rule.it)
        in
        attempt_rule)
      rules
  in
  choice attempt_rules

(* Invoke a function *)

and match_clause (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (clause : clause)
    (values_input : value list) : (Ctx.t * arg list * prem list * exp) attempt =
  let args_input, exp_output, prems = clause.it in
  check
    (List.length args_input = List.length values_input)
    clause.at "arity mismatch while matching clause";
  let* ctx = assign_args ctx_caller ctx_callee args_input values_input in
  Ok (ctx, args_input, prems, exp_output)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    (Ctx.t * value) attempt =
  invoke_func' ctx id targs args
  |> nest id.at
       (F.asprintf "invocation of function %s%s%s failed"
          (Il.Print.string_of_defid id)
          (Il.Print.string_of_targs targs)
          (Il.Print.string_of_args args))

and invoke_func' (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    (Ctx.t * value) attempt =
  if Builtin.is_builtin id then invoke_func_builtin ctx id targs args
  else invoke_func_def ctx id targs args

and invoke_func_builtin (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : (Ctx.t * value) attempt =
  let ctx, values_input = eval_args ctx args in
  let ctx_local = Ctx.localize ctx in
  let ctx_local = Ctx.trace_open_dec ctx_local id 0 values_input in
  let* value_output =
    try
      if !is_cache_enabled && is_func_cached id.it then (
        let val_opt = Cache.find_opt !cache (id.it, values_input)in
        match val_opt with
        | Some value_output -> 
          (* Printf.printf "-- hit: %s %s \n" id.it (Pp.string_of_value value_output) ; *)
          hits := !hits + 1;
          Ok value_output
        | None -> 
          (let value_output = Builtin.invoke id targs values_input in
           Cache.add !cache (id.it, values_input) value_output;
          Ok value_output))
      else 
        let value_output = Builtin.invoke id targs values_input in
        Ok value_output
    with Util.Error.Error (at, msg) -> fail at msg
  in
  let ctx_local = Ctx.trace_close ctx_local in
  let ctx = Ctx.trace_commit ctx ctx_local.trace in
  Ok (ctx, value_output)

and invoke_func_def (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : (Ctx.t * value) attempt =
  (* Find the function *)
  let tparams, _params, _typ_ret, clauses = Ctx.find_func Local ctx id in
  guard (clauses <> []) id.at "function has no clauses";
  (* Evaluate type arguments *)
  let targs =
    let theta =
      TDEnv.bindings ctx.global.tdenv @ TDEnv.bindings ctx.local.tdenv
      |> List.filter_map (fun (tid, (_tparams, deftyp)) ->
             match deftyp.it with PlainT typ -> Some (tid, typ) | _ -> None)
      |> TIdMap.of_list
    in
    List.map (Typ.subst_typ theta) targs
  in
  (* Evaluate arguments *)
  let ctx, values_input = eval_args ctx args in
  let attempt id clauses tparams targs ctx values_input= 
    let attempt_clauses =
      List.mapi
        (fun idx_clause clause ->
           let attempt_clause' (ctx_local : Ctx.t) (prems : prem list)
               (exp_output : exp) : (Ctx.t * value) attempt =
             let* ctx_local = eval_prems ctx_local prems in
             let ctx_local, value_output = eval_exp ctx_local exp_output in
             let ctx_local = Ctx.trace_close ctx_local in
             let ctx = Ctx.trace_commit ctx ctx_local.trace in
             Ok (ctx, value_output)
           in
           let attempt_clause () : (Ctx.t * value) attempt =
             (* Create a subtrace for the clause *)
             let ctx_local = Ctx.localize ctx in
             let ctx_local =
               Ctx.trace_open_dec ctx_local id idx_clause values_input
             in
             (* Add type arguments to the context *)
             check
               (List.length targs = List.length tparams)
               id.at "arity mismatch in type arguments";
             let ctx_local =
               List.fold_left2
                 (fun ctx_local tparam targ ->
                    Ctx.add_typdef Local ctx_local tparam ([], PlainT targ $ targ.at))
                 ctx_local tparams targs
             in
             (* Try to match the clause *)
             let* ctx_local, args_input, prems, exp_output =
               match_clause ctx ctx_local clause values_input
             in
             (* Try evaluating the clause *)
             attempt_clause' ctx_local prems exp_output
             |> nest id.at
               (F.asprintf "application of clause %s%s failed" id.it
                  (Il.Print.string_of_args args_input))
           in
           attempt_clause)
        clauses
    in
    choice attempt_clauses
  in
  try
    if !is_cache_enabled && is_func_cached id.it then (
      let val_opt = Cache.find_opt !cache (id.it, values_input) in
      match val_opt with
      | Some value_output -> 
        (* Printf.printf "-- hit: %s %s \n" id.it (Pp.string_of_value value_output) ; *)
        hits := !hits + 1;
        Ok (ctx, value_output)
      | None -> 
        let* ctx, value_output = attempt id clauses tparams targs ctx values_input in
        Cache.add !cache (id.it, values_input) value_output;
        Ok (ctx, value_output))
    else 
        let* ctx, value_output = attempt id clauses tparams targs ctx values_input in
        Ok (ctx, value_output)
    with Util.Error.Error (at, msg) -> fail at msg
  (* Apply the first matching clause *)

(* Load definitions into a context *)

let load_def (ctx : Ctx.t) (def : def) : Ctx.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      Ctx.add_typdef Global ctx id typdef
  | RelD (id, nottyp, inputs, rules) ->
      let rel = (nottyp, inputs, rules) in
      Ctx.add_rel Global ctx id rel
  | DecD (id, tparams, params, typ, clauses) ->
      let func = (tparams, params, typ, clauses) in
      Ctx.add_func Global ctx id func

let load_spec (ctx : Ctx.t) (spec : spec) : Ctx.t =
  List.fold_left load_def ctx spec

(* Entry point: run typing rule from `Prog_ok` relation *)

let run_typing (debug : bool) (profile : bool) (cache : bool) (spec : spec) (program : value) :
    value list =
  Builtin.init ();
  is_cache_enabled := cache;
  let ctx = Ctx.empty debug profile in
  let ctx = load_spec ctx spec in
  let+ ctx, values = invoke_rel ctx ("Prog_ok" $ no_region) [ program ] in
  Ctx.profile ctx;
  Printf.printf "caching hits: %d" !hits;
  values
