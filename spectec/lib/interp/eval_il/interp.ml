open Common.Source
open Lang.Xl
open Lang.Il
open Envs.Make
module Typ = Envs.Il.Typ
open Error
open Attempt
module Events = Instrumentation.Event
module F = Format

(* Assignments *)

(* Assigning a value to an expression *)

let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
  let note = value.note.typ in
  match (exp.it, value.it) with
  | VarE id, _ ->
      let ctx = Ctx.add_value ctx (id, []) value in
      ctx
  | TupleE exps, TupleV values -> assign_exps ctx exps values
  | CaseE notexp, CaseV valuecase ->
      let exps = Mixfix.args notexp in
      let values = Mixfix.args valuecase in
      assign_exps ctx exps values
  | StrE expfields, StructV valuefields ->
      let exps = List.map snd expfields in
      let values = List.map snd valuefields in
      assign_exps ctx exps values
  | OptE exp_opt, OptV value_opt -> (
      match (exp_opt, value_opt) with
      | Some exp, Some value -> assign_exp ctx exp value
      | None, None -> ctx
      | _ -> assert false)
  | ListE exps, ListV values -> assign_exps ctx exps values
  | ConsE (exp_h, exp_t), ListV values_inner ->
      let value_h = List.hd values_inner in
      let value_t = List.tl values_inner |> Value.Make.list note in
      let ctx = assign_exp ctx exp_h value_h in
      assign_exp ctx exp_t value_t
  | IterE (_, (Opt, vars)), OptV None ->
      (* Per iterated variable, make an option out of the value *)
      let bindings =
        List.map
          (fun { varid = id; typ; iters } ->
            let value_sub =
              let typ = Lang.Il.Typ.iterate typ (iters @ [ Opt ]) in
              None |> Value.Make.opt typ.it
            in
            ((id, iters @ [ Opt ]), value_sub))
          vars
      in
      Ctx.add_values ctx bindings
  | IterE (exp, (Opt, vars)), OptV (Some value) ->
      (* Assign the value to the iterated expression *)
      let ctx = assign_exp ctx exp value in
      (* Per iterated variable, make an option out of the value *)
      let bindings =
        List.map
          (fun { varid = id; typ; iters } ->
            let value_sub =
              let value = Ctx.find_value ctx (id, iters) in
              let typ = Lang.Il.Typ.iterate typ (iters @ [ Opt ]) in
              Some value |> Value.Make.opt typ.it
            in
            ((id, iters @ [ Opt ]), value_sub))
          vars
      in
      Ctx.add_values ctx bindings
  | IterE (exp, (List, vars)), ListV values ->
      (* Map over the value list elements,
         and assign each value to the iterated expression *)
      let ctxs_rev =
        List.fold_left
          (fun ctxs_rev value ->
            (* Use localize to reuse cached empty local *)
            let ctx = Ctx.localize ctx in
            let ctx = assign_exp ctx exp value in
            ctx :: ctxs_rev)
          [] values
      in
      let ctxs = List.rev ctxs_rev in
      (* Per iterated variable, collect its elementwise value,
         then make a sequence out of them *)
      List.fold_left
        (fun ctx { varid = id; typ; iters } ->
          let values =
            List.map (fun ctx -> Ctx.find_value ctx (id, iters)) ctxs
          in
          let value_sub =
            let typ = Lang.Il.Typ.iterate typ (iters @ [ List ]) in
            values |> Value.Make.list typ.it
          in
          Ctx.add_value ctx (id, iters @ [ List ]) value_sub)
        ctx vars
  | _ ->
      error exp.at
        (F.asprintf "(TODO) match failed %s <- %s" (Print.string_of_exp exp)
           (Print.string_of_value ~short:true value))

and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t =
  let region = over_region (List.map at exps) in
  checkf
    (List.length exps = List.length values)
    region
    "mismatch in number of expressions and values while assigning, expected %d \
     value(s) but got %d"
    (List.length exps) (List.length values);
  List.fold_left2 assign_exp ctx exps values

(* Assigning a value to an argument *)

and assign_arg (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (arg : arg)
    (value : value) : Ctx.t =
  let assign_arg_exp ctx exp value = assign_exp ctx exp value in
  let assign_arg_def ctx_caller ctx_callee id value =
    match value.it with
    | FuncV id_f ->
        let _, func = Ctx.find_func ctx_caller id_f in
        Ctx.add_func ctx_callee id func
    | _ ->
        error id.at
          (F.asprintf "cannot assign a value %s to a definition %s"
             (Print.string_of_value ~short:true value)
             id.it)
  in
  match arg.it with
  | ExpA exp -> assign_arg_exp ctx_callee exp value
  | DefA id -> assign_arg_def ctx_caller ctx_callee id value

and assign_args (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (args : arg list)
    (values : value list) : Ctx.t =
  let region = over_region (List.map at args) in
  checkf
    (List.length args = List.length values)
    region
    "mismatch in number of arguments and values while assigning, expected %d \
     value(s) but got %d"
    (List.length args) (List.length values);
  List.fold_left2 (assign_arg ctx_caller) ctx_callee args values

(* Type coercion and subtyping *)

(* DownCastE and SubE performs subtype checks that are not guaranteed by the type system,
    because in SpecTec assignment should be able to revert the type cast expression

     - Numeric subtyping:
       - e.g., -- if (int) n = $foo() when $foo() returns a positive integer +2
     - Variant subtyping:
       - e.g., -- if (typ) objtyp = $foo() when $foo() returns a variant of objtyp specifically
     - Tuple subtyping: recursive, but the type system guarantees that their lengths are equal
     - Iteration subtyping

   Note that structs are invariant in SpecTec, so we do not need to check for subtyping *)

let rec upcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
  match typ.it with
  | NumT `IntT -> (
      match value.it with
      | NumV (`Nat n) -> Value.int n
      | NumV (`Int _) -> value
      | _ -> assert false)
  | VarT { synid = tid; targs } -> (
      let tparams, deftyp = Ctx.find_typdef ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match deftyp.it with
      | PlainT typ ->
          let typ = Typ.subst_typ theta typ in
          upcast ctx typ value
      | _ -> value)
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          let values =
            List.fold_left2
              (fun values typ value ->
                let value = upcast ctx typ value in
                values @ [ value ])
              [] typs values
          in
          Value.Make.tuple typ.it values
      | _ -> assert false)
  | _ -> value

let rec downcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
  match typ.it with
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> value
      | NumV (`Int i) when Bigint.(i >= zero) -> Value.nat i
      | _ -> assert false)
  | VarT { synid = tid; targs } -> (
      let tparams, deftyp = Ctx.find_typdef ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match deftyp.it with
      | PlainT typ ->
          let typ = Typ.subst_typ theta typ in
          downcast ctx typ value
      | _ -> value)
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          let values =
            List.fold_left2
              (fun values typ value ->
                let value = downcast ctx typ value in
                values @ [ value ])
              [] typs values
          in
          Value.Make.tuple typ.it values
      | _ -> assert false)
  | _ -> value

let rec subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
  match typ.it with
  | BoolT -> ( match value.it with BoolV _ -> true | _ -> false)
  | NumT `NatT -> (
      match value.it with
      | NumV (`Nat _) -> true
      | NumV (`Int i) -> Bigint.(i >= zero)
      | _ -> false)
  | NumT `IntT -> ( match value.it with NumV _ -> true | _ -> false)
  | TextT -> ( match value.it with TextV _ -> true | _ -> false)
  | VarT { synid = tid; targs } -> (
      let tparams, deftyp = Ctx.find_typdef ctx tid in
      let theta = List.combine tparams targs |> TIdMap.of_list in
      match (deftyp.it, value.it) with
      | PlainT typ, _ ->
          let typ = Typ.subst_typ theta typ in
          subtyp ctx typ value
      | VariantT typcases, CaseV valuecase ->
          List.exists
            (fun typcase ->
              let { notation = nottyp; _ } = typcase in
              Mixfix.eq_mixop nottyp.it valuecase
              && subtyps ctx
                   (List.map (Typ.subst_typ theta) (Mixfix.args nottyp.it))
                   (Mixfix.args valuecase))
            typcases
      | _ -> true)
  | TupleT typs -> (
      match value.it with
      | TupleV values ->
          List.length typs = List.length values
          && List.for_all2 (subtyp ctx) typs values
      | _ -> false)
  | IterT { typ = typ_inner; iter = Opt } -> (
      match value.it with
      | OptV value_opt -> (
          match value_opt with
          | Some value_inner -> subtyp ctx typ_inner value_inner
          | None -> true)
      | _ -> false)
  | IterT { typ = typ_inner; iter = List } -> (
      match value.it with
      | ListV values -> List.for_all (subtyp ctx typ_inner) values
      | _ -> false)
  | _ -> false

and subtyps (ctx : Ctx.t) (typs : typ list) (values : value list) : bool =
  List.length typs = List.length values
  && List.for_all2 (subtyp ctx) typs values

(* Expression evaluation *)

let rec eval_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let eval_bool_exp note b = Value.Make.bool note b in
  let eval_num_exp note n = Value.Make.num note n in
  let eval_text_exp note s = Value.Make.text note s in
  let eval_var_exp _note ctx id = Ctx.find_value ctx (id, []) in
  let wrap_ctx value = (ctx, value) in
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE b -> eval_bool_exp note b |> wrap_ctx
  | NumE n -> eval_num_exp note n |> wrap_ctx
  | TextE s -> eval_text_exp note s |> wrap_ctx
  | VarE id -> eval_var_exp note ctx id |> wrap_ctx
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
  | IterE (exp, iterexp) -> eval_iter_exp note ctx exp iterexp

and eval_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * value list =
  let ctx, values_rev =
    List.fold_left
      (fun (ctx, values_rev) exp ->
        let ctx, value = eval_exp ctx exp in
        (ctx, value :: values_rev))
      (ctx, []) exps
  in
  (ctx, List.rev values_rev)
(* Unary expression evaluation *)

and eval_un_exp (note : typ') (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
    (exp : exp) : Ctx.t * value =
  let eval_un_bool note unop value =
    match unop with
    | `NotOp -> (not (Value.get_bool value)) |> Value.Make.bool note
  in
  let eval_un_num note unop value =
    let num = Value.get_num value in
    let num = Num.un unop num in
    num |> Value.Make.num note
  in
  let ctx, value = eval_exp ctx exp in
  let value_res =
    match unop with
    | #Bool.unop as unop -> eval_un_bool note unop value
    | #Num.unop as unop -> eval_un_num note unop value
  in
  (ctx, value_res)

(* Binary expression evaluation *)

and eval_bin_exp (note : typ') (ctx : Ctx.t) (binop : binop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let eval_bin_bool note binop value_l value_r =
    let bool_l = Value.get_bool value_l in
    let bool_r = Value.get_bool value_r in
    let bool_res =
      match binop with
      | `AndOp -> bool_l && bool_r
      | `OrOp -> bool_l || bool_r
      | `ImplOp -> (not bool_l) || bool_r
      | `EquivOp -> bool_l = bool_r
    in
    bool_res |> Value.Make.bool note
  in
  let eval_bin_num note binop value_l value_r =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Num.bin binop num_l num_r |> Value.Make.num note
  in
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match binop with
    | #Bool.binop as binop -> eval_bin_bool note binop value_l value_r
    | #Num.binop as binop -> eval_bin_num note binop value_l value_r
  in
  (ctx, value_res)

(* Comparison expression evaluation *)

and eval_cmp_exp (note : typ') (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * value =
  let eval_cmp_bool note cmpop value_l value_r =
    let eq = Value.eq value_l value_r in
    let bool_res = match cmpop with `EqOp -> eq | `NeOp -> not eq in
    Value.Make.bool note bool_res
  in
  let eval_cmp_num note cmpop value_l value_r =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Num.cmp cmpop num_l num_r |> Value.Make.bool note
  in
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match cmpop with
    | #Bool.cmpop as cmpop -> eval_cmp_bool note cmpop value_l value_r
    | #Num.cmpop as cmpop -> eval_cmp_num note cmpop value_l value_r
  in
  (ctx, value_res)

(* Upcast expression evaluation *)

and eval_upcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value_res = upcast ctx typ value in
  (ctx, value_res)

(* Downcast expression evaluation *)

and eval_downcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let value_res = downcast ctx typ value in
  (ctx, value_res)

(* Subtype check expression evaluation *)

and eval_sub_exp (note : typ') (ctx : Ctx.t) (exp : exp) (typ : typ) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let sub = subtyp ctx typ value in
  let value_res = Value.Make.bool note sub in
  (ctx, value_res)

(* Pattern match check expression evaluation *)

and eval_match_exp (note : typ') (ctx : Ctx.t) (exp : exp) (pattern : pattern) :
    Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let matches =
    match (pattern, value.it) with
    | CaseP mixop_p, CaseV valuecase -> Mixfix.eq_mixop mixop_p valuecase
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
  let value_res = Value.Make.bool note matches in
  (ctx, value_res)

(* Tuple expression evaluation *)

and eval_tuple_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let value_res = Value.Make.tuple note values in
  (ctx, value_res)

(* Case expression evaluation *)

and eval_case_exp (note : typ') (ctx : Ctx.t) (notexp : notexp) : Ctx.t * value
    =
  let mixop, exps = Mixfix.split notexp in
  let ctx, values = eval_exps ctx exps in
  let value_res = Value.Make.case note (Mixfix.fill mixop values) in
  (ctx, value_res)

(* Struct expression evaluation *)

and eval_str_exp (note : typ') (ctx : Ctx.t) (fields : (atom * exp) list) :
    Ctx.t * value =
  let atoms, exps = List.split fields in
  let ctx, values = eval_exps ctx exps in
  let fields = List.combine atoms values in
  let value_res = Value.Make.record note fields in
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
  let value_res = Value.Make.opt note value_opt in
  (ctx, value_res)

(* List expression evaluation *)

and eval_list_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : Ctx.t * value
    =
  let ctx, values = eval_exps ctx exps in
  let value_res = Value.Make.list note values in
  (ctx, value_res)

(* Cons expression evaluation *)

and eval_cons_exp (note : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    Ctx.t * value =
  let ctx, value_h = eval_exp ctx exp_h in
  let ctx, value_t = eval_exp ctx exp_t in
  let values_t = Value.get_list value_t in
  let value_res = Value.Make.list note (value_h :: values_t) in
  (ctx, value_res)

(* Concatenation expression evaluation *)

and eval_cat_exp (note : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
    (exp_r : exp) : Ctx.t * value =
  let ctx, value_l = eval_exp ctx exp_l in
  let ctx, value_r = eval_exp ctx exp_r in
  let value_res =
    match (value_l.it, value_r.it) with
    | TextV s_l, TextV s_r -> s_l ^ s_r |> Value.Make.text note
    | ListV values_l, ListV values_r ->
        values_l @ values_r |> Value.Make.list note
    | _ -> error at "concatenation expects either two texts or two lists"
  in
  (ctx, value_res)

(* Membership expression evaluation *)

and eval_mem_exp (note : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    Ctx.t * value =
  let ctx, value_e = eval_exp ctx exp_e in
  let ctx, value_s = eval_exp ctx exp_s in
  let values_s = Value.get_list value_s in
  let value_res =
    List.exists (Value.eq value_e) values_s |> Value.Make.bool note
  in
  (ctx, value_res)

(* Length expression evaluation *)

and eval_len_exp (note : typ') (ctx : Ctx.t) (exp : exp) : Ctx.t * value =
  let ctx, value = eval_exp ctx exp in
  let len =
    match value.it with
    | TextV s -> s |> String.length |> Bigint.of_int
    | ListV values -> values |> List.length |> Bigint.of_int
    | _ ->
        error exp.at
          (Format.asprintf
             "length operation expects either a text or a list, but got %s"
             (Il.Print.string_of_value ~short:true value))
  in
  let value_res = Value.Make.nat note len in
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
  let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let value_res =
    match value_b.it with
    | TextV s when idx < 0 || idx >= String.length s ->
        error exp_i.at
          (Format.asprintf "index %d out of bounds [0, %d)" idx
             (String.length s))
    | TextV s ->
        let s = String.get s idx |> String.make 1 in
        Value.Make.text Il.TextT s
    | ListV values when idx < 0 || idx >= List.length values ->
        error exp_i.at
          (Format.asprintf "index %d out of bounds [0, %d)" idx
             (List.length values))
    | ListV values -> List.nth values idx
    | _ ->
        error exp_b.at
          (Format.asprintf
             "indexing expects either a text or a list, but got %s"
             (Il.Print.string_of_value ~short:true value_b))
  in
  (ctx, value_res)

(* Slice expression evaluation *)

and eval_slice_exp (note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
    (exp_n : exp) : Ctx.t * value =
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_i = eval_exp ctx exp_i in
  let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let ctx, value_len = eval_exp ctx exp_n in
  let idx_n = value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let idx_h = idx_l + idx_n in
  let value_res =
    match value_b.it with
    | TextV s when idx_l < 0 || idx_h > String.length s ->
        error exp_i.at
          (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
             (String.length s))
    | TextV s ->
        let s_slice = String.sub s idx_l (idx_h - idx_l) in
        Value.Make.text Il.TextT s_slice
    | ListV values when idx_l < 0 || idx_h > List.length values ->
        error exp_n.at
          (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
             (List.length values))
    | ListV values ->
        let values_slice =
          List.mapi
            (fun idx value ->
              if idx_l <= idx && idx < idx_h then Some value else None)
            values
          |> List.filter_map Fun.id
        in
        Value.Make.list note values_slice
    | _ ->
        error exp_b.at
          (Format.asprintf "slicing expects either a text or a list, but got %s"
             (Il.Print.string_of_value ~short:true value_b))
  in
  (ctx, value_res)

(* Update expression evaluation *)

and eval_upd_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (path : path)
    (exp_f : exp) : Ctx.t * value =
  let rec eval_access_path ctx value_b path =
    match path.it with
    | RootP -> (ctx, value_b)
    | IdxP (path, exp_i) -> (
        let ctx, value = eval_access_path ctx value_b path in
        let ctx, value_i = eval_exp ctx exp_i in
        let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
        match value.it with
        | TextV s when idx < 0 || idx >= String.length s ->
            error exp_i.at
              (Format.asprintf "index %d out of bounds [0, %d)" idx
                 (String.length s))
        | TextV s ->
            let s = String.get s idx |> String.make 1 in
            let value_res = Value.Make.text Il.TextT s in
            (ctx, value_res)
        | ListV values when idx < 0 || idx >= List.length values ->
            error exp_i.at
              (Format.asprintf "index %d out of bounds [0, %d)" idx
                 (List.length values))
        | ListV values ->
            let value_res = List.nth values idx in
            (ctx, value_res)
        | _ ->
            error path.at
              (Format.asprintf
                 "indexing expects either a text or a list, but got %s"
                 (Il.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let ctx, value = eval_access_path ctx value_b path in
        let ctx, value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let ctx, value_len = eval_exp ctx exp_n in
        let idx_n =
          value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            error exp_n.at
              (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l
                 idx_h (String.length s))
        | TextV s ->
            let s_slice = String.sub s idx_l (idx_h - idx_l) in
            let value_res = Value.Make.text Il.TextT s_slice in
            (ctx, value_res)
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            error exp_n.at
              (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l
                 idx_h (List.length values))
        | ListV values ->
            let values_slice =
              List.mapi
                (fun idx value ->
                  if idx_l <= idx && idx < idx_h then Some value else None)
                values
              |> List.filter_map Fun.id
            in
            let value_res = Value.Make.list path.note values_slice in
            (ctx, value_res)
        | _ ->
            error path.at
              (Format.asprintf
                 "slicing expects either a text or a list, but got %s"
                 (Il.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let ctx, value = eval_access_path ctx value_b path in
        let fields = value |> Value.get_struct in
        let value =
          fields
          |> List.map (fun (atom, value) -> (atom.it, value))
          |> List.assoc atom.it
        in
        (ctx, value)
  and eval_update_path ctx value_b path value_new =
    match path.it with
    | RootP -> (ctx, value_new)
    | IdxP (path, exp_i) -> (
        let ctx, value = eval_access_path ctx value_b path in
        let ctx, value_i = eval_exp ctx exp_i in
        let idx_target =
          value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        match value.it with
        | TextV s when idx_target < 0 || idx_target >= String.length s ->
            error exp_i.at
              (Format.asprintf "index %d out of bounds [0, %d)" idx_target
                 (String.length s))
        | TextV s ->
            let s_new = Value.get_text value_new in
            if String.length s_new <> 1 then
              error exp_i.at
                (Format.asprintf
                   "updating a character requires a single-character text, but \
                    got %s"
                   (Il.Print.string_of_value ~short:true value_new))
            else
              let s_updated =
                String.sub s 0 idx_target ^ s_new
                ^ String.sub s (idx_target + 1)
                    (String.length s - idx_target - 1)
              in
              let value = Value.Make.text Il.TextT s_updated in
              eval_update_path ctx value_b path value
        | ListV values when idx_target < 0 || idx_target >= List.length values
          ->
            error exp_i.at
              (Format.asprintf "index %d out of bounds [0, %d)" idx_target
                 (List.length values))
        | ListV values ->
            let values_updated =
              List.mapi
                (fun idx value -> if idx = idx_target then value_new else value)
                values
            in
            let value = Value.Make.list path.note values_updated in
            eval_update_path ctx value_b path value
        | _ ->
            error path.at
              (Format.asprintf
                 "indexing expects either a text or a list, but got %s"
                 (Il.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let ctx, value = eval_access_path ctx value_b path in
        let ctx, value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let ctx, value_len = eval_exp ctx exp_n in
        let idx_n =
          value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            error exp_n.at
              (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l
                 idx_h (String.length s))
        | TextV s ->
            let s_new = Value.get_text value_new in
            if String.length s_new <> idx_n then
              error exp_n.at
                (Format.asprintf
                   "updating a slice of length %d requires a text of length \
                    %d, but got %s"
                   idx_n (String.length s_new)
                   (Il.Print.string_of_value ~short:true value_new))
            else
              let s_updated =
                String.sub s 0 idx_l ^ s_new
                ^ String.sub s idx_h (String.length s - idx_h)
              in
              let value = Value.Make.text Il.TextT s_updated in
              eval_update_path ctx value_b path value
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            error exp_n.at
              (Format.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l
                 idx_h (List.length values))
        | ListV values ->
            let values_new = Value.get_list value_new in
            if List.length values_new <> idx_n then
              error exp_n.at
                (Format.asprintf
                   "updating a slice of length %d requires a list of length \
                    %d, but got %s"
                   idx_n (List.length values_new)
                   (Il.Print.string_of_value ~short:true value_new))
            else
              let values_updated =
                List.mapi
                  (fun idx value ->
                    if idx_l <= idx && idx < idx_h then
                      List.nth values_new (idx - idx_l)
                    else value)
                  values
              in
              let value = Value.Make.list path.note values_updated in
              eval_update_path ctx value_b path value
        | _ ->
            error path.at
              (Format.asprintf
                 "slicing expects either a text or a list, but got %s"
                 (Il.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let ctx, value = eval_access_path ctx value_b path in
        let fields = value |> Value.get_struct in
        let fields =
          List.map
            (fun (atom_f, value_f) ->
              if atom_f.it = atom.it then (atom_f, value_new)
              else (atom_f, value_f))
            fields
        in
        let value = Value.Make.record path.note fields in
        eval_update_path ctx value_b path value
  in
  let ctx, value_b = eval_exp ctx exp_b in
  let ctx, value_f = eval_exp ctx exp_f in
  let ctx, value_res = eval_update_path ctx value_b path value_f in
  (ctx, value_res)

(* Function call expression evaluation *)

and eval_call_exp (_note : typ') (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Ctx.t * value =
  let+ ctx, value_res = invoke_func ctx id targs args in
  (ctx, value_res)

(* Iterated expression evaluation *)

and eval_iter_exp (note : typ') (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) :
    Ctx.t * value =
  let eval_iter_exp_opt note ctx exp vars =
    let+ ctx_sub_opt = Ctx.sub_opt ctx vars in
    match ctx_sub_opt with
    | Some ctx_sub ->
        let _, value = eval_exp ctx_sub exp in
        let value_res = Some value |> Value.Make.opt note in
        (ctx, value_res)
    | None ->
        let value_res = None |> Value.Make.opt note in
        (ctx, value_res)
  in
  let eval_iter_exp_list note ctx exp vars =
    let+ ctxs_sub = Ctx.sub_list ctx vars in
    let ctx, values_rev =
      List.fold_left
        (fun (ctx, values_rev) ctx_sub ->
          let _, value = eval_exp ctx_sub exp in
          (ctx, value :: values_rev))
        (ctx, []) ctxs_sub
    in
    let value_res = values_rev |> List.rev |> Value.Make.list note in
    (ctx, value_res)
  in
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_exp_opt note ctx exp vars
  | List -> eval_iter_exp_list note ctx exp vars

(* Argument evaluation *)

and eval_args (ctx : Ctx.t) (args : arg list) : Ctx.t * value list =
  let eval_arg ctx arg =
    match arg.it with
    | ExpA exp -> eval_exp ctx exp
    | DefA id ->
        let value_res = Value.func id in
        (ctx, value_res)
  in
  List.fold_left
    (fun (ctx, values) arg ->
      let ctx, value = eval_arg ctx arg in
      (ctx, values @ [ value ]))
    (ctx, []) args

(* Premise evaluation *)

and eval_prem (ctx : Ctx.t) (prem : prem) : Ctx.t attempt =
  let eval_rule_prem ctx id notexp =
    let reltyp, _ = Ctx.find_rel ctx id in
    let exps_input, exps_output =
      Mode.partition reltyp.it (Mixfix.args notexp)
    in
    let ctx, values_input = eval_exps ctx exps_input in
    let* ctx, values_output = invoke_rel ctx id values_input in
    let ctx = assign_exps ctx exps_output values_output in
    Ok ctx
  in
  let eval_cond_prem fail_with ctx exp_cond =
    let ctx, value_cond = eval_exp ctx exp_cond in
    let cond = Value.get_bool value_cond in
    if cond then Ok ctx
    else
      fail_with exp_cond.at
        (F.asprintf "condition %s was not met" (Print.string_of_exp exp_cond))
  in
  let eval_rel_assert_prem ctx (call : relcall) (expect : bool) =
    let id = call.relid in
    let ctx, values_input = eval_exps ctx (Mixfix.args call.notexp) in
    match (invoke_rel ctx id values_input, expect) with
    | Ok _, true | Error _, false -> Ok ctx
    | Ok _, false ->
        fail id.at (F.asprintf "condition not-hold %s was not met" id.it)
    | Error failtraces, true -> Error failtraces
  in
  let eval_let_prem ctx exp_l exp_r =
    let ctx, value = eval_exp ctx exp_r in
    let ctx = assign_exp ctx exp_l value in
    Ok ctx
  in
  let eval_debug_prem ctx exp =
    let ctx, value = eval_exp ctx exp in
    print_endline
    @@ F.sprintf "%s: %s" (string_of_region exp.at) (Print.string_of_exp exp);
    print_endline @@ Print.string_of_value value;
    Ok ctx
  in
  (match prem.it with
  | IterPr _ -> ()
  | _ ->
      Instrumentation.Dispatcher.emit (Events.Prem_enter { prem; at = prem.at }));
  let result =
    match prem.it with
    | RelPr { relid; notexp } -> eval_rule_prem ctx relid notexp
    | RelAssertPr { call; expect } -> eval_rel_assert_prem ctx call expect
    | IfPr { cond; role } ->
        let fail_with =
          match role with Guard -> fail_guard | Condition -> fail
        in
        eval_cond_prem fail_with ctx cond
    | ElsePr -> Ok ctx
    | LetPr (exp_l, exp_r) -> eval_let_prem ctx exp_l exp_r
    | IterPr (prem, iterexp) -> eval_iter_prem ctx prem iterexp
    | DebugPr exp -> eval_debug_prem ctx exp
  in
  (match prem.it with
  | IterPr _ -> ()
  | _ ->
      Instrumentation.Dispatcher.emit_on_demand @@ fun () ->
      let ctx_out = Result.value ~default:ctx result in
      let bindings =
        Free.free_prem prem |> Common.Domain.IdSet.elements
        |> List.filter_map (fun id ->
               Ctx.find_value_opt ctx_out (id, [])
               |> Option.map (fun value -> (id, value)))
      in
      Events.Prem_exit
        { prem; at = prem.at; success = Result.is_ok result; bindings });
  result

and eval_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t attempt =
  List.fold_left
    (fun ctx prem ->
      let* ctx = ctx in
      eval_prem ctx prem)
    (Ok ctx) prems

(* Iterated premise evaluation *)

and eval_iter_prem_opt (ctx : Ctx.t) (prem : prem) (vars : var list) :
    Ctx.t attempt =
  let vars_bound, vars_binding =
    List.partition
      (fun { varid; iters; _ } -> Ctx.bound_value ctx (varid, iters @ [ Opt ]))
      vars
  in
  let* ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
  match ctx_sub_opt with
  | None ->
      let bindings =
        List.map
          (fun { varid = id_binding; typ = typ_binding; iters = iters_binding }
             ->
            let typ =
              Lang.Il.Typ.iterate typ_binding (iters_binding @ [ Opt ])
            in
            let value_binding = None |> Value.Make.opt typ.it in
            ((id_binding, iters_binding @ [ Opt ]), value_binding))
          vars_binding
      in
      Ok (Ctx.add_values ctx bindings)
  | Some ctx_sub ->
      Instrumentation.Dispatcher.emit
        (Events.Iter_prem_enter { prem; at = prem.at });
      let* ctx_sub = eval_prem ctx_sub prem in
      Instrumentation.Dispatcher.emit (Events.Iter_prem_exit { at = prem.at });
      let bindings =
        List.map
          (fun { varid = id_binding; typ = typ_binding; iters = iters_binding }
             ->
            let typ =
              Lang.Il.Typ.iterate typ_binding (iters_binding @ [ Opt ])
            in
            let value_binding =
              Ctx.find_value ctx_sub (id_binding, iters_binding)
            in
            let value_binding = Some value_binding |> Value.Make.opt typ.it in
            ((id_binding, iters_binding @ [ Opt ]), value_binding))
          vars_binding
      in
      Ok (Ctx.add_values ctx bindings)

and eval_iter_prem_list (ctx : Ctx.t) (prem : prem) (vars : var list) :
    Ctx.t attempt =
  (* Discriminate between bound and binding variables *)
  let vars_bound, vars_binding =
    List.partition
      (fun { varid; iters; _ } -> Ctx.bound_value ctx (varid, iters @ [ List ]))
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
        let* ctx, values_binding_batch_rev =
          List.fold_left
            (fun ctx_values_binding_batch ctx_sub ->
              let* ctx, values_binding_batch_rev = ctx_values_binding_batch in
              Instrumentation.Dispatcher.emit
                (Events.Iter_prem_enter { prem; at = prem.at });
              let* ctx_sub = eval_prem ctx_sub prem in
              Instrumentation.Dispatcher.emit
                (Events.Iter_prem_exit { at = prem.at });
              let value_binding_batch =
                List.map
                  (fun { varid = id_binding; iters = iters_binding; _ } ->
                    Ctx.find_value ctx_sub (id_binding, iters_binding))
                  vars_binding
              in
              let values_binding_batch_rev =
                value_binding_batch :: values_binding_batch_rev
              in
              Ok (ctx, values_binding_batch_rev))
            (Ok (ctx, []))
            ctxs_sub
        in
        let* values_binding =
          values_binding_batch_rev |> List.rev |> Ctx.transpose
        in
        Ok (ctx, values_binding)
  in
  (* Finally, bind the resulting binding batches *)
  let bindings =
    List.map2
      (fun { varid = id_binding; typ = typ_binding; iters = iters_binding }
           values_binding ->
        let value_binding =
          let typ =
            Lang.Il.Typ.iterate typ_binding (iters_binding @ [ List ])
          in
          values_binding |> Value.Make.list typ.it
        in
        ((id_binding, iters_binding @ [ List ]), value_binding))
      vars_binding values_binding
  in
  let ctx = Ctx.add_values ctx bindings in
  Ok ctx

and eval_iter_prem (ctx : Ctx.t) (prem : prem) (iterexp : iterexp) :
    Ctx.t attempt =
  (* List iteration *)
  let eval_iter_prem_list ctx prem vars =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun { varid; iters; _ } ->
          Ctx.bound_value ctx (varid, iters @ [ List ]))
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
          (* Hook: iteration start *)
          let* ctx, values_binding_batch_rev =
            List.fold_left
              (fun ctx_values_binding_batch ctx_sub ->
                let* ctx, values_binding_batch_rev = ctx_values_binding_batch in
                Instrumentation.Dispatcher.emit
                  (Events.Iter_prem_enter { prem; at = prem.at });
                let* ctx_sub = eval_prem ctx_sub prem in
                Instrumentation.Dispatcher.emit
                  (Events.Iter_prem_exit { at = prem.at });
                let value_binding_batch =
                  List.map
                    (fun { varid = id_binding; iters = iters_binding; _ } ->
                      Ctx.find_value ctx_sub (id_binding, iters_binding))
                    vars_binding
                in
                let values_binding_batch_rev =
                  value_binding_batch :: values_binding_batch_rev
                in
                Ok (ctx, values_binding_batch_rev))
              (Ok (ctx, []))
              ctxs_sub
          in
          let* values_binding =
            values_binding_batch_rev |> List.rev |> Ctx.transpose
          in
          Ok (ctx, values_binding)
    in
    (* Finally, bind the resulting binding batches *)
    let ctx =
      List.fold_left2
        (fun ctx
             { varid = id_binding; typ = typ_binding; iters = iters_binding }
             values_binding ->
          let value_binding =
            let typ =
              Lang.Il.Typ.iterate typ_binding (iters_binding @ [ List ])
            in
            values_binding |> Value.Make.list typ.it
          in
          Ctx.add_value ctx (id_binding, iters_binding @ [ List ]) value_binding)
        ctx vars_binding values_binding
    in
    Ok ctx
  in
  let iter, vars = iterexp in
  match iter with
  | Opt -> eval_iter_prem_opt ctx prem vars
  | List -> eval_iter_prem_list ctx prem vars

(* Invoke a relation *)

and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
    (Ctx.t * value list) attempt =
  Instrumentation.Dispatcher.emit
    (Events.Rel_enter { id = id.it; at = id.at; inputs = values_input });
  (* Rule matching *)
  let match_rule ctx reltyp rule values_input =
    let { concl; prems; _ } = rule.it in
    let exps_input, exps_output =
      Mode.partition reltyp.it (Mixfix.args concl)
    in
    checkf
      (List.length exps_input = List.length values_input)
      rule.at "arity mismatch in rule";
    let ctx = assign_exps ctx exps_input values_input in
    (ctx, prems, exps_output)
  in
  let reltyp, rules = Ctx.find_rel ctx id in
  (* Main invocation logic *)
  let invoke_rel' () =
    check_warnf (rules <> []) id.at "relation has no rules";
    (* Apply the first matching rule *)
    let attempt_rules () =
      let attempt_rules' =
        List.map
          (fun rule ->
            let { ruleid = id_rule; _ } = rule.it in
            let attempt_rule' (ctx_local : Ctx.t) (prems : prem list)
                (exps_output : exp list) : (Ctx.t * value list) attempt =
              let* ctx_local = eval_prems ctx_local prems in
              let _, values_output = eval_exps ctx_local exps_output in
              Ok (ctx, values_output)
            in
            let attempt_rule () : (Ctx.t * value list) attempt =
              Instrumentation.Dispatcher.emit
                (Events.Rule_enter
                   { id = id.it; rule_id = id_rule.it; at = id.at });
              (* Create a subtrace for the rule *)
              let ctx_local = Ctx.localize ctx in
              (* Try to match the rule *)
              let ctx_local, prems, exps_output =
                match_rule ctx_local reltyp rule values_input
              in
              (* Try evaluating the rule *)
              let result =
                attempt_rule' ctx_local prems exps_output
                |> nest id.at
                     (F.asprintf "application of rule %s/%s failed" id.it
                        id_rule.it)
              in
              Instrumentation.Dispatcher.emit
                (Events.Rule_exit
                   {
                     id = id.it;
                     rule_id = id_rule.it;
                     at = id.at;
                     success = Result.is_ok result;
                   });
              result
            in
            attempt_rule)
          rules
      in
      choice attempt_rules'
    in
    let invoke () =
      let* _, values_output = attempt_rules () in
      Ok values_output
    in
    let* values_output =
      invoke |> Cache.with_rel_cache ctx.cache (id.it, values_input)
    in
    Ok (ctx, values_output)
  in
  let result = invoke_rel' () in
  Instrumentation.Dispatcher.emit_on_demand (fun () ->
      let outs =
        match result with
        | Ok (_, values_output) -> List.map Option.some values_output
        | Error _ -> List.map (fun _ -> None) (Mode.outputs reltyp.it)
      in
      let conclusion = Mode.fill reltyp.it ~ins:values_input ~outs in
      Events.Rel_exit
        { id = id.it; at = id.at; success = Result.is_ok result; conclusion });
  result |> nest id.at (F.asprintf "invocation of relation %s failed" id.it)

(* Invoke a function *)

and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list) :
    (Ctx.t * value) attempt =
  let ctx, values_input = eval_args ctx args in
  Instrumentation.Dispatcher.emit
    (Events.Func_enter { id = id.it; at = id.at; inputs = values_input });
  (* Clause matching *)
  let match_clause ctx_caller ctx_callee clause values_input =
    let { args = args_input; body = exp_output; prems } = clause.it in
    checkf
      (List.length args_input = List.length values_input)
      clause.at "arity mismatch while matching clause";
    let ctx = assign_args ctx_caller ctx_callee args_input values_input in
    (ctx, args_input, prems, exp_output)
  in
  (* Builtin function invocation *)
  let invoke_func_builtin () =
    (* Invoke builtin function *)
    let invoke_func_builtin' () =
      Instrumentation.Dispatcher.emit
        (Events.Clause_enter { id = id.it; clause_idx = 0; at = id.at });
      let value_output =
        ctx.builtins.invoke id targs values_input |> unwrap_builtin
      in
      Instrumentation.Dispatcher.emit
        (Events.Clause_exit
           { id = id.it; clause_idx = 0; at = id.at; success = true });
      Ok (ctx, value_output)
    in
    invoke_func_builtin' ()
  in
  (* User-defined function invocation *)
  let invoke_func_def tparams clauses =
    (* Evaluate type arguments *)
    let targs =
      match targs with
      | [] -> []
      | targs ->
          let theta =
            Ctx.TDEnv.fold
              (fun tid typdef theta ->
                let tparams, deftyp = typdef in
                match (tparams, deftyp.it) with
                | [], PlainT typ -> TIdMap.add tid typ theta
                | _ -> theta)
              ctx.local.tdenv TIdMap.empty
          in
          List.map (Typ.subst_typ theta) targs
    in
    (* Apply the first matching clause *)
    let attempt_clauses () =
      let attempt_clauses' =
        List.mapi
          (fun idx_clause clause ->
            let attempt_clause' (ctx_local : Ctx.t) (prems : prem list)
                (exp_output : exp) : (Ctx.t * value) attempt =
              let* ctx_local = eval_prems ctx_local prems in
              let _, value_output = eval_exp ctx_local exp_output in
              Ok (ctx, value_output)
            in
            let attempt_clause () : (Ctx.t * value) attempt =
              Instrumentation.Dispatcher.emit
                (Events.Clause_enter
                   { id = id.it; clause_idx = idx_clause; at = id.at });
              (* Create a subtrace for the clause *)
              let ctx_local = Ctx.localize ctx in
              (* Add type arguments to the context *)
              checkf
                (List.length targs = List.length tparams)
                id.at "arity mismatch in type arguments";
              let typdef_bindings =
                List.map2
                  (fun tparam targ -> (tparam, ([], PlainT targ $ targ.at)))
                  tparams targs
              in
              let ctx_local = Ctx.add_typdefs ctx_local typdef_bindings in
              (* Try to match the clause *)
              let ctx_local, args_input, prems, exp_output =
                match_clause ctx ctx_local clause values_input
              in
              (* Try evaluating the clause *)
              let result =
                attempt_clause' ctx_local prems exp_output
                |> nest id.at
                     (F.asprintf "application of clause %s%s failed" id.it
                        (Print.string_of_args args_input))
              in
              Instrumentation.Dispatcher.emit
                (Events.Clause_exit
                   {
                     id = id.it;
                     clause_idx = idx_clause;
                     at = id.at;
                     success = Result.is_ok result;
                   });
              result
            in
            attempt_clause)
          clauses
      in
      choice attempt_clauses'
    in
    let* _, value_output = attempt_clauses () in
    Ok (ctx, value_output)
  in
  (* Main dispatch *)
  let result =
    let invoke_func' () =
      let* _, value_output =
        match Ctx.find_func_opt ctx id with
        | Some (_, Ctx.Func.Builtin) ->
            checkf
              (ctx.builtins.is_builtin id)
              id.at "builtin $%s is declared in the spec but not implemented"
              id.it;
            invoke_func_builtin ()
        | Some (_, Ctx.Func.Defined (tparams, clauses)) ->
            check_warnf (clauses <> []) id.at "function has no clauses";
            invoke_func_def tparams clauses
        | None ->
            if ctx.builtins.is_builtin id then (
              warn id.at
                (F.asprintf "builtin $%s is invoked without a spec declaration"
                   id.it);
              invoke_func_builtin ())
            else fail id.at (F.asprintf "unknown function %s" id.it)
      in
      Ok value_output
    in
    let is_anonymous fid =
      match Ctx.find_func_opt ctx fid with
      | Some (cursor, _) -> cursor = Ctx.Local
      | None -> false
    in
    let is_high_order values =
      List.exists
        (fun value ->
          match value.it with Lang.Il.FuncV _ -> true | _ -> false)
        values
    in
    let* value_output =
      (* Skip caching for generics and HOFs *)
      if targs <> [] || is_anonymous id || is_high_order values_input then
        invoke_func' ()
      else invoke_func' |> Cache.with_func_cache ctx.cache (id.it, values_input)
    in
    Ok (ctx, value_output)
  in
  let output = match result with Ok (_, v) -> Some v | Error _ -> None in
  Instrumentation.Dispatcher.emit
    (Events.Func_exit { id = id.it; at = id.at; output });
  result
  |> nest id.at
       (F.asprintf "invocation of function %s%s%s failed"
          (Print.string_of_defid id)
          (Print.string_of_targs targs)
          (Print.string_of_args args))

(* Load definitions into the context *)

let load_def (l : Ctx.global_loader) (def : def) : unit =
  match def.it with
  | TypD { synid = id; tparams; deftyp } ->
      let typdef = (tparams, deftyp) in
      Ctx.load_typdef l id typdef
  | RelD { relid = id; reltyp; rules } ->
      let rel = (reltyp, rules) in
      Ctx.load_rel l id rel
  | BuiltinDecD { defid = id; _ } -> Ctx.load_func l id Ctx.Func.Builtin
  | DecD { defid = id; tparams; clauses; _ } ->
      let func = Ctx.Func.Defined (tparams, clauses) in
      Ctx.load_func l id func

let load_spec (filename : string) (builtins : Builtins.t) (cache : Cache.t)
    (spec : spec) : Ctx.t =
  let l = Ctx.create_loader () in
  List.iter (load_def l) spec;
  let global_layer = Ctx.freeze l in
  Ctx.create ~filename builtins cache global_layer
