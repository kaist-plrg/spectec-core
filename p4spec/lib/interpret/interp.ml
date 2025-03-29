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

(* Expansion of type aliases *)

let rec expand_typ (ctx : Ctx.t) (typ : Typ.t) : Typ.t =
  match typ.it with
  | VarT (tid, targs) -> (
      let tparams, deftyp = Ctx.find_typdef Local ctx tid in
      match deftyp.it with
      | PlainT typ ->
          check
            (List.length targs = List.length tparams)
            tid.at "type arguments do not match";
          let theta = List.combine tparams targs |> TIdMap.of_list in
          let typ = Typ.subst_typ theta typ in
          expand_typ ctx typ
      | _ -> typ)
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

and equiv_nottyp (ctx : Ctx.t) (nottyp_a : nottyp) (nottyp_b : nottyp) : bool =
  let mixop_a, typs_a = nottyp_a.it in
  let mixop_b, typs_b = nottyp_b.it in
  Mixop.eq mixop_a mixop_b
  && List.length typs_a = List.length typs_b
  && List.for_all2 (equiv_typ ctx) typs_a typs_b

and sub_typ (ctx : Ctx.t) (typ_a : Typ.t) (typ_b : Typ.t) : bool =
  equiv_typ ctx typ_a typ_b || sub_typ' ctx typ_a typ_b

and sub_typ' (ctx : Ctx.t) (typ_a : Typ.t) (typ_b : Typ.t) : bool =
  let typ_a = expand_typ ctx typ_a in
  let typ_b = expand_typ ctx typ_b in
  match (typ_a.it, typ_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.sub numtyp_a numtyp_b
  | VarT (tid_a, _targs_a), VarT (tid_b, _targs_b) -> (
      let _tparams_a, deftyp_a = Ctx.find_typdef Local ctx tid_a in
      let _tparams_b, deftyp_b = Ctx.find_typdef Local ctx tid_b in
      match (deftyp_a.it, deftyp_b.it) with
      | VariantT typcases_a, VariantT typcases_b ->
          List.for_all
            (fun nottyp_a -> List.exists (equiv_nottyp ctx nottyp_a) typcases_b)
            typcases_a
      | _ -> false)
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

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t attempt =
  let typ_exp = exp.note $ exp.at in
  let typ_value = value.note $ value.at in
  match (exp.it, value.it) with
  | VarE _, _ when not (sub_typ ctx typ_value typ_exp) ->
      fail exp.at
        (F.asprintf "mismatch in type: %s expected but got %s of type %s"
           (Il.Print.string_of_typ typ_exp)
           (Il.Print.string_of_value ~short:true value)
           (Il.Print.string_of_typ typ_value))
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
        let value_t = ListV (List.tl values) $$ (value.at, value.note) in
        let* ctx = assign_exp ctx exp_h value_h in
        assign_exp ctx exp_t value_t
  | IterE (_, (Opt, vars)), OptV None ->
      let ctx =
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let value =
              let typ_value = Typ.iterate typ (iters @ [ Opt ]) in
              OptV None $$ (value.at, typ_value.it)
            in
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
          (fun ctx (id, typ, iters) ->
            let value =
              let typ_value = Typ.iterate typ (iters @ [ Opt ]) in
              let value = Ctx.find_value Local ctx (id, iters) in
              OptV (Some value) $$ (value.at, typ_value.it)
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
          (fun ctx (id, typ, iters) ->
            let values =
              List.map (fun ctx -> Ctx.find_value Local ctx (id, iters)) ctxs
            in
            let value =
              let typ_value = Typ.iterate typ (iters @ [ List ]) in
              ListV values $$ (value.at, typ_value.it)
            in
            Ctx.add_value Local ctx (id, iters @ [ List ]) value)
          ctx vars
      in
      Ok ctx
  | CastE (exp, _), _ -> assign_exp ctx exp value
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
  match value.it with
  | FuncV id_f ->
      let func = Ctx.find_func Local ctx_caller id_f in
      let ctx_callee = Ctx.add_func Local ctx_callee id func in
      Ok ctx_callee
  | _ ->
      fail id.at
        (F.asprintf "cannot assign a value %s to a definition %s"
           (Il.Print.string_of_value ~short:true value)
           id.it)

(* Expression evaluation:

   An expression evaluates to a value, which is annotated with
   its runtime type. To maintain the smallest possible runtime type,
   the types are first determined by the value itself,
   and in cases where ambiguous (e.g., an empty list), it takes
   the type of the expression.

   We may add a runtime check for the subtype relation between the
   produced value and the expected type of the expression,
   but for now, we assume that the type system is sound. *)

let rec eval_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let wrap_ctx value = (ctx, value) in
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE b -> eval_bool_exp at b |> wrap_ctx
  | NumE n -> eval_num_exp at n |> wrap_ctx
  | TextE s -> eval_text_exp at s |> wrap_ctx
  | VarE id -> eval_var_exp ctx at id |> wrap_ctx
  | UnE (unop, optyp, exp) -> eval_un_exp ctx at unop optyp exp
  | BinE (binop, optyp, exp_l, exp_r) ->
      eval_bin_exp ctx at binop optyp exp_l exp_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      eval_cmp_exp ctx at cmpop optyp exp_l exp_r
  | TupleE exps -> eval_tuple_exp ctx at exps
  | CaseE notexp -> eval_case_exp ctx at note notexp
  | OptE exp_opt -> eval_opt_exp ctx at note exp_opt
  | StrE fields -> eval_str_exp ctx at note fields
  | DotE (exp_b, atom) -> eval_dot_exp ctx at exp_b atom
  | ListE exps -> eval_list_exp ctx at note exps
  | ConsE (exp_h, exp_t) -> eval_cons_exp ctx at note exp_h exp_t
  | CatE (exp_l, exp_r) -> eval_cat_exp ctx at note exp_l exp_r
  | MemE (exp_e, exp_s) -> eval_mem_exp ctx at exp_e exp_s
  | SliceE (exp_b, exp_l, exp_h) -> eval_slice_exp ctx at note exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> eval_upd_exp ctx at exp_b path exp_f
  | CallE (id, targs, args) -> eval_call_exp ctx at id targs args
  | LenE exp -> eval_len_exp ctx at exp
  | IdxE (exp_b, exp_i) -> eval_idx_exp ctx at exp_b exp_i
  | IterE (exp, iterexp) -> eval_iter_exp ctx at note exp iterexp
  | CastE (exp, typ) -> eval_cast_exp ctx at exp typ

and eval_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * value list =
  List.fold_left
    (fun (ctx, values) exp ->
      let ctx, value = eval_exp ctx exp in
      (ctx, values @ [ value ]))
    (ctx, []) exps

(* Boolean expression evaluation *)

and eval_bool_exp (at : region) (b : bool) : value = BoolV b $$ (at, BoolT)

(* Numeric expression evaluation *)

and eval_num_exp (at : region) (n : Num.t) : value =
  NumV n $$ (at, NumT (Num.to_typ n))

(* Text expression evaluation *)

and eval_text_exp (at : region) (s : string) : value = TextV s $$ (at, TextT)

(* Variable expression evaluation *)

and eval_var_exp (ctx : Ctx.t) (at : region) (id : id) : value =
  let value = Ctx.find_value Local ctx (id, []) in
  value.it $$ (at, value.note)

(* Unary expression evaluation *)

and eval_un_bool (at : region) (unop : Bool.unop) (value : value) : value =
  match unop with `NotOp -> BoolV (not (Value.get_bool value)) $$ (at, BoolT)

and eval_un_num (at : region) (unop : Num.unop) (value : value) : value =
  let num = Value.get_num value in
  let num = Num.un unop num in
  NumV num $$ (at, NumT (Num.to_typ num))

and eval_un_exp (ctx : Ctx.t) (at : region) (unop : unop) (_optyp : optyp)
    (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  match unop with
  | #Bool.unop as unop ->
      let value = eval_un_bool at unop value in
      (ctx, value)
  | #Num.unop as unop ->
      let value = eval_un_num at unop value in
      (ctx, value)

(* Binary expression evaluation *)

and eval_bin_bool (at : region) (binop : Bool.binop) (value_l : value)
    (value_r : value) : value =
  let bool_l = Value.get_bool value_l in
  let bool_r = Value.get_bool value_r in
  let value =
    match binop with
    | `AndOp -> BoolV (bool_l && bool_r)
    | `OrOp -> BoolV (bool_l || bool_r)
    | `ImplOp -> BoolV ((not bool_l) || bool_r)
    | `EquivOp -> BoolV (bool_l = bool_r)
  in
  value $$ (at, BoolT)

and eval_bin_num (at : region) (binop : Num.binop) (value_l : value)
    (value_r : value) : value =
  let num_l = Value.get_num value_l in
  let num_r = Value.get_num value_r in
  let num = Num.bin binop num_l num_r in
  NumV num $$ (at, NumT (Num.to_typ num))

and eval_bin_exp (ctx : Ctx.t) (at : region) (binop : binop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  match binop with
  | #Bool.binop as binop ->
      let value = eval_bin_bool at binop value_l value_r in
      (ctx, value)
  | #Num.binop as binop ->
      let value = eval_bin_num at binop value_l value_r in
      (ctx, value)

(* Comparison expression evaluation *)

and eval_cmp_bool (at : region) (cmpop : Bool.cmpop) (value_l : value)
    (value_r : value) : value =
  let eq = Value.eq value_l value_r in
  let value = match cmpop with `EqOp -> BoolV eq | `NeOp -> BoolV (not eq) in
  value $$ (at, BoolT)

and eval_cmp_num (at : region) (cmpop : Num.cmpop) (value_l : value)
    (value_r : value) : value =
  let num_l = Value.get_num value_l in
  let num_r = Value.get_num value_r in
  BoolV (Num.cmp cmpop num_l num_r) $$ (at, BoolT)

and eval_cmp_exp (ctx : Ctx.t) (at : region) (cmpop : cmpop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value =
    match cmpop with
    | #Bool.cmpop as cmpop -> eval_cmp_bool at cmpop value_l value_r
    | #Num.cmpop as cmpop -> eval_cmp_num at cmpop value_l value_r
  in
  (ctx, value)

(* Tuple expression evaluation *)

and eval_tuple_exp (ctx : Ctx.t) (at : region) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let typs = List.map (fun value -> value.note $ value.at) values in
  let value = TupleV values $$ (at, TupleT typs) in
  (ctx, value)

(* Case expression evaluation

   Variant typing is nominal, so we take the type of the expression itself
   Elaboration has already found the smallest possible type for the variant expression *)

and eval_case_exp (ctx : Ctx.t) (at : region) (typ : typ') (notexp : notexp) :
    Ctx.t * value =
  let mixop, exps = notexp in
  let ctx, values = eval_exps ctx exps in
  let value = CaseV (mixop, values) $$ (at, typ) in
  (ctx, value)

(* Option expression evaluation

   To deal with none values, we take the type of the expression itself *)

and eval_opt_exp (ctx : Ctx.t) (at : region) (typ : typ') (exp_opt : exp option)
    : Ctx.t * value =
  match exp_opt with
  | Some exp ->
      let ctx, value = eval_exp ctx exp in
      let typ = IterT (value.note $ value.at, Opt) in
      let value = OptV (Some value) $$ (at, typ) in
      (ctx, value)
  | None ->
      let value = OptV None $$ (at, typ) in
      (ctx, value)

(* Struct expression evaluation

   Struct typing is nominal, so we take the type of the expression itself
   For now, all structs are invariant *)

and eval_str_exp (ctx : Ctx.t) (at : region) (typ : typ')
    (fields : (atom * exp) list) : Ctx.t * value =
  let atoms, exps = List.split fields in
  let ctx, values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  let value = StructV fields $$ (at, typ) in
  (ctx, value)

(* Dot expression evaluation *)

and eval_dot_exp (ctx : Ctx.t) (at : region) (exp_b : exp) (atom : atom) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let fields = Value.get_struct value_b in
  let value =
    fields
    |> List.map (fun (atom, value) -> (atom.it, value))
    |> List.assoc atom.it
  in
  let value = value.it $$ (at, value.note) in
  (ctx, value)

(* List expression evaluation

   Lists are tricky, because it requires to find the smallest type within the list
   Naive implementation would require N^2 time complexity
   For now, to avoid this, we take the type of the expression itself
   This may result in loss of runtime type precision, which may in turn lead to
   failures in runtime subtype checks when it really should not *)

and eval_list_exp (ctx : Ctx.t) (at : region) (typ : typ') (exps : exp list) :
    Ctx.t * value =
  let ctx, values = eval_exps ctx exps in
  let value = ListV values $$ (at, typ) in
  (ctx, value)

(* Cons expression evaluation

   Similarly as in lists, we take the type of the expression itself for now *)

and eval_cons_exp (ctx : Ctx.t) (at : region) (typ : typ') (exp_h : exp)
    (exp_t : exp) : Ctx.t * value =
  let ctx, value_h = eval_exp ctx exp_h in
  let ctx, value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  let value = ListV (value_h :: values_t) $$ (at, typ) in
  (ctx, value)

(* Concatenation expression evaluation

   For concatenation of lists, we take the type of the expression itself for now *)

and eval_cat_exp (ctx : Ctx.t) (at : region) (typ : typ') (exp_l : exp)
    (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value =
    match (value_l.it, value_r.it) with
    | TextV s_l, TextV s_r -> TextV (s_l ^ s_r) $$ (at, TextT)
    | ListV values_l, ListV values_r -> ListV (values_l @ values_r) $$ (at, typ)
    | _ -> error at "concatenation expects either two texts or two lists"
  in
  (ctx, value)

(* Membership expression evaluation *)

and eval_mem_exp (ctx : Ctx.t) (at : region) (exp_e : exp) (exp_s : exp) :
    Ctx.t * value =
  let ctx, value_e = eval_exp ctx exp_e in
  let ctx, value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  let value = BoolV (List.exists (Value.eq value_e) values_s) $$ (at, BoolT) in
  (ctx, value)

(* Length expression evaluation *)

and eval_len_exp (ctx : Ctx.t) (at : region) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let len = value |> Value.get_list |> List.length |> Bigint.of_int in
  let value = NumV (`Nat len) $$ (at, NumT `NatT) in
  (ctx, value)

(* Index expression evaluation *)

and eval_idx_exp (ctx : Ctx.t) (at : region) (exp_b : exp) (exp_i : exp) :
    Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let values = Value.get_list value_b in
  let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let value = List.nth values idx in
  let value = value.it $$ (at, value.note) in
  (ctx, value)

(* Slice expression evaluation

   Similarly as in lists, we take the type of the expression itself for now *)

and eval_slice_exp (ctx : Ctx.t) (at : region) (typ : typ') (exp_b : exp)
    (exp_i : exp) (exp_n : exp) : Ctx.t * value =
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
  let value = ListV values_slice $$ (at, typ) in
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

and eval_update_path (at : region) (value_b : value) (path : path)
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
      let value = StructV fields $$ (value_b.at, value_b.note) in
      eval_update_path at value_b path value
  | _ -> failwith "(TODO) update"

and eval_upd_exp (ctx : Ctx.t) (at : region) (exp_b : exp) (path : path)
    (exp_f : exp) : Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_f = eval_exp ctx exp_f in
  let value = eval_update_path at value_b path value_f in
  let value = value.it $$ (at, value.note) in
  (ctx, value)

(* Function call expression evaluation *)

and eval_call_exp (ctx : Ctx.t) (at : region) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
  let+ ctx, value = invoke_func ctx id targs args in
  let value = value.it $$ (at, value.note) in
  (ctx, value)

(* Iterated expression evaluation

   To deal with iteration by none option or list, we take the type of the expression itself *)

and eval_iter_exp_opt (ctx : Ctx.t) (at : region) (typ : typ') (exp : exp)
    (vars : var list) : Ctx.t * value =
  let+ ctx_sub_opt = Ctx.sub_opt ctx vars in
  match ctx_sub_opt with
  | Some ctx_sub ->
      let ctx_sub = Ctx.trace_open_iter ctx_sub (Il.Print.string_of_exp exp) in
      let ctx_sub, value = eval_exp ctx_sub exp in
      let ctx_sub = Ctx.trace_close ctx_sub in
      let ctx = Ctx.trace_commit ctx ctx_sub.trace in
      let typ = IterT (value.note $ value.at, Opt) in
      let value = OptV (Some value) $$ (at, typ) in
      (ctx, value)
  | None ->
      let value = OptV None $$ (at, typ) in
      (ctx, value)

and eval_iter_exp_list (ctx : Ctx.t) (at : region) (typ : typ') (exp : exp)
    (vars : var list) : Ctx.t * value =
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
  let value = ListV values $$ (at, typ) in
  (ctx, value)

and eval_iter_exp (ctx : Ctx.t) (at : region) (typ : typ') (exp : exp)
    (iterexp : iterexp) : Ctx.t * value =
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt ctx at typ exp vars
  | List -> eval_iter_exp_list ctx at typ exp vars

(* Cast expression evaluation

   For now this is treated as a no-op *)

and eval_cast_exp (ctx : Ctx.t) (at : region) (exp : exp) (_typ : typ) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value = value.it $$ (at, value.note) in
  (ctx, value)

(* Argument evaluation *)

and eval_arg (ctx : Ctx.t) (arg : arg) : Ctx.t * value =
  match arg.it with
  | ExpA exp -> eval_exp ctx exp
  | DefA id ->
      (* (TODO) Give appropriate type to the function value *)
      let value = FuncV id $$ (arg.at, FuncT) in
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
      (fun (id, _typ, iters) ->
        Ctx.bound_value Local ctx (id, iters @ [ List ]))
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
                List.map
                  (fun (id, _typ, iters) ->
                    Ctx.find_value Local ctx_sub (id, iters))
                  vars_binding
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
      (fun ctx (id, typ, iters) values_binding ->
        let value_binding =
          let typ_value = Typ.iterate typ (iters @ [ List ]) in
          ListV values_binding $$ (no_region, typ_value.it)
        in
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
  (* Apply the first matching clause *)
  let attempt_clauses =
    List.mapi
      (fun idx_clause clause ->
        let attempt_clause' (ctx_local : Ctx.t) (prems : prem list)
            (exp_output : exp) : (Ctx.t * value) attempt =
          let* ctx_local = eval_prems ctx_local prems in
          let ctx_local, value_output = eval_exp ctx_local exp_output in
          let value_output =
            let typ_output = value_output.note $ value_output.at in
            let theta = List.combine tparams targs |> TIdMap.of_list in
            let typ_output = Typ.subst_typ theta typ_output in
            value_output.it $$ (value_output.at, typ_output.it)
          in
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

let run_typing (debug : bool) (profile : bool) (spec : spec) (program : value) :
    value list =
  Builtin.init ();
  let ctx = Ctx.empty debug profile in
  let ctx = load_spec ctx spec in
  let+ ctx, values = invoke_rel ctx ("Prog_ok" $ no_region) [ program ] in
  Ctx.profile ctx;
  values
