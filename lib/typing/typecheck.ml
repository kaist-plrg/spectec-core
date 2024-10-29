open Runtime.Domain
module Ctk = Runtime.Ctk
module Value = Runtime.Value
module Types = Runtime.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module FuncType = Types.FuncType
module FuncDef = Types.FuncDef
module ConsType = Types.ConsType
module ConsDef = Types.ConsDef
module Envs = Runtime.Envs
module WF = Wellformed
module F = Format
open Util.Source

(* Coercion rules *)

let rec insert_cast (expr_il : Il.Ast.expr) (typ : Type.t) : Il.Ast.expr =
  let ctk = Static.ctk_cast_expr (typ $ no_info) expr_il in
  Il.Ast.(
    CastE { typ = typ $ no_info; expr = expr_il } $$ expr_il.at % { typ; ctk })

(* (TODO) Coercion for unary when serializable enum is involved *)

(* Coercion for binary *)

and coerce_types_binary (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr) :
    Type.t * Il.Ast.expr * Il.Ast.expr =
  let typ_l = expr_l_il.note.typ in
  let typ_r = expr_r_il.note.typ in
  if Eq.eq_typ_alpha typ_l typ_r then (typ_l, expr_l_il, expr_r_il)
  else if Subtyp.implicit typ_l typ_r then
    (typ_r, insert_cast expr_l_il typ_r, expr_r_il)
  else if Subtyp.implicit typ_r typ_l then
    (typ_l, expr_l_il, insert_cast expr_r_il typ_l)
  else (
    Format.printf "(coerce_types_binary) Cannot coerce types %a and %a\n"
      Type.pp expr_l_il.note.typ Type.pp expr_r_il.note.typ;
    assert false)

and coerce_types_binary_numeric (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr * Il.Ast.expr =
  let rec reduce_senum (typ : Type.t) (expr_l_il : Il.Ast.expr)
      (expr_r_il : Il.Ast.expr) =
    match typ with
    | SEnumT (_, typ_inner) ->
        let expr_l_il = insert_cast expr_l_il typ_inner in
        let expr_r_il = insert_cast expr_r_il typ_inner in
        reduce_senum typ_inner expr_l_il expr_r_il
    | _ -> (typ, expr_l_il, expr_r_il)
  in
  let typ, expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  reduce_senum typ expr_l_il expr_r_il

(* Coercion for assignment (including assignment by call and return) *)

and coerce_type_assign (expr_from_il : Il.Ast.expr) (typ_to : Type.t) :
    Il.Ast.expr =
  let typ_from = expr_from_il.note.typ in
  if Eq.eq_typ_alpha typ_from typ_to then expr_from_il
  else if Subtyp.implicit typ_from typ_to then insert_cast expr_from_il typ_to
  else (
    Format.printf "(coerce_type) Cannot coerce type %a to %a\n" Type.pp
      expr_from_il.note.typ Type.pp typ_to;
    assert false)

(* Type inference *)

(* (7.2.11) Type Specialization

   A generic type may be specialized by specifying arguments for its type variables.
   In cases where the compiler can infer type arguments
   type specialization is not necessary.

   (17.2) Example architecture program

   The type substitution can be expressed directly, using type specialization,
   or can be inferred by a compiler, using a unification algorithm like Hindley-Milner. *)

type cstr_t = Type.t option TIdMap.t

let empty_cstr (tids_fresh : TId.t list) : cstr_t =
  List.map (fun tid_fresh -> (tid_fresh, None)) tids_fresh |> TIdMap.of_list

let rec gen_cstr (cstr : cstr_t) (typ_param : Type.t) (typ_arg : Type.t) :
    cstr_t =
  match (typ_param, typ_arg) with
  | VarT tid, typ_arg when TIdMap.mem tid cstr ->
      TIdMap.add tid (Some typ_arg) cstr
  | NewT (id_param, typ_inner_param), NewT (id_arg, typ_inner_arg)
    when id_param = id_arg ->
      gen_cstr cstr typ_inner_param typ_inner_arg
  | TupleT typs_param, TupleT typs_arg -> gen_cstrs cstr typs_param typs_arg
  | StackT (typ_inner_param, size_param), StackT (typ_inner_arg, size_arg)
    when Bigint.(size_param = size_arg) ->
      gen_cstr cstr typ_inner_param typ_inner_arg
  | StructT (id_param, fields_param), StructT (id_arg, fields_arg)
    when id_param = id_arg ->
      let typs_inner_param = List.map snd fields_param in
      let typs_inner_arg = List.map snd fields_arg in
      gen_cstrs cstr typs_inner_param typs_inner_arg
  | HeaderT (id_param, fields_param), HeaderT (id_arg, fields_arg)
    when id_param = id_arg ->
      let typs_inner_param = List.map snd fields_param in
      let typs_inner_arg = List.map snd fields_arg in
      gen_cstrs cstr typs_inner_param typs_inner_arg
  | UnionT (id_param, fields_param), UnionT (id_arg, fields_arg)
    when id_param = id_arg ->
      let typs_inner_param = List.map snd fields_param in
      let typs_inner_arg = List.map snd fields_arg in
      gen_cstrs cstr typs_inner_param typs_inner_arg
  | ExternT (id_param, fdenv_param), ExternT (id_arg, fdenv_arg)
    when id_param = id_arg ->
      let keys_param = FIdMap.keys fdenv_param |> FIdSet.of_list in
      let keys_arg = FIdMap.keys fdenv_arg |> FIdSet.of_list in
      assert (FIdSet.eq keys_param keys_arg);
      let keys = keys_param in
      FIdSet.fold
        (fun key cstr ->
          let fd_param = FIdMap.find key fdenv_param in
          let fd_arg = FIdMap.find key fdenv_arg in
          gen_cstr_fd cstr fd_param fd_arg)
        keys cstr
  | ParserT params_param, ParserT params_arg
  | ControlT params_param, ControlT params_arg ->
      let typs_inner_param =
        List.map (fun (_, _, typ, _) -> typ) params_param
      in
      let typs_inner_arg = List.map (fun (_, _, typ, _) -> typ) params_arg in
      gen_cstrs cstr typs_inner_param typs_inner_arg
  | _ -> cstr

and gen_cstr_fd (cstr : cstr_t) (fd_param : FuncDef.t) (fd_arg : FuncDef.t) :
    cstr_t =
  let params_param = FuncDef.get_params fd_param in
  let params_arg = FuncDef.get_params fd_arg in
  let typs_param = List.map (fun (_, _, typ, _) -> typ) params_param in
  let typs_arg = List.map (fun (_, _, typ, _) -> typ) params_arg in
  let cstr_params = gen_cstrs cstr typs_param typs_arg in
  gen_cstr cstr_params
    (FuncDef.get_typ_ret fd_param)
    (FuncDef.get_typ_ret fd_arg)

and merge_cstr (cstr_old : cstr_t) (cstr_new : cstr_t) : cstr_t =
  let keys_old = TIdMap.keys cstr_old |> TIdSet.of_list in
  let keys_new = TIdMap.keys cstr_new |> TIdSet.of_list in
  assert (
    TIdSet.cardinal keys_old = TIdSet.cardinal keys_new
    && TIdSet.eq keys_old keys_new);
  let keys = keys_old in
  TIdSet.fold
    (fun key cstr ->
      let typ_old = TIdMap.find key cstr_old in
      let typ_new = TIdMap.find key cstr_new in
      match (typ_old, typ_new) with
      | None, _ -> TIdMap.add key typ_new cstr
      | _, None -> TIdMap.add key typ_old cstr
      | Some typ_old, Some typ_new ->
          if Eq.eq_typ_alpha typ_old typ_new then
            TIdMap.add key (Some typ_old) cstr
          else (
            Format.printf "(merge_cstr) Type %a and %a do not match\n" Type.pp
              typ_old Type.pp typ_new;
            assert false))
    keys TIdMap.empty

and gen_cstrs (cstr : cstr_t) (typ_params : Type.t list)
    (typ_args : Type.t list) : cstr_t =
  let cstrs =
    List.map2
      (fun typ_param typ_arg -> gen_cstr cstr typ_param typ_arg)
      typ_params typ_args
  in
  List.fold_left merge_cstr cstr cstrs

let infer_targs (tids_fresh : TId.t list) (params : Types.param list)
    (args_il_typed : (Il.Ast.arg * Type.t) list) : Subst.Theta.t =
  let cstr = empty_cstr tids_fresh in
  let typ_params = List.map (fun (_, _, typ, _) -> typ) params in
  let typ_args = List.map snd args_il_typed in
  let cstr = gen_cstrs cstr typ_params typ_args in
  TIdMap.fold
    (fun tid typ_opt theta ->
      match typ_opt with
      | Some typ -> Subst.Theta.add tid typ theta
      | None -> Subst.Theta.add tid Types.TopT theta)
    cstr Subst.Theta.empty

(* (6.7) L-values

   L-values are expressions that may appear on the left side of an assignment operation
   or as arguments corresponding to out and inout function parameters.
   An l-value represents a storage reference. The following expressions are legal l-values:

   - Identifiers of a base or derived type.
   - Structure, header, and header union field member access operations (using the dot notation).
   - References to elements within header stacks (see Section 8.18): indexing, and references to last and next.
   - The result of a bit-slice operator [m:l]. *)

let rec check_lvalue (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_il : Il.Ast.expr)
    : unit =
  if not (check_lvalue' cursor ctx expr_il) then (
    Format.printf "(check_lvalue) %a is not an l-value\n"
      (Il.Pp.pp_expr ~level:0) expr_il;
    assert false)

and check_lvalue' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_il : Il.Ast.expr) :
    bool =
  match expr_il.it with
  | VarE { var } -> (
      let rtype = Ctx.find_opt Ctx.find_rtype_opt cursor var ctx in
      if Option.is_none rtype then (
        Format.printf "(check_lvalue) %a is a free identifier\n" Il.Pp.pp_var
          var;
        assert false);
      let typ, dir, _ = Option.get rtype in
      match dir with In | No -> false | _ -> check_lvalue_type typ)
  | ArrAccE { expr_base; _ }
  | BitAccE { expr_base; _ }
  | ExprAccE { expr_base; _ } ->
      check_lvalue' cursor ctx expr_base
  | _ -> false

and check_lvalue_type (typ : Type.t) : bool =
  match typ with
  | NewT (_, typ_inner) -> check_lvalue_type typ_inner
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT -> false
  | _ -> true

(* Type evaluation *)

module TIdMap = MakeTIdEnv (Type)

let rec eval_type (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ) :
    Il.Ast.typ =
  eval_type' cursor ctx typ.it $ typ.at

and eval_type_with_check (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ)
    : Il.Ast.typ =
  let typ = eval_type cursor ctx typ in
  WF.check_valid_type cursor ctx typ.it;
  typ

and eval_type' (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ') :
    Il.Ast.typ' =
  match typ with
  | VoidT -> Types.VoidT
  | ErrT -> Types.ErrT
  | MatchKindT -> Types.MatchKindT
  | StrT -> Types.StrT
  | BoolT -> Types.BoolT
  | IntT -> Types.IntT
  | FIntT expr_width ->
      let expr_width_il = type_expr cursor ctx expr_width in
      let width =
        expr_width_il |> Static.eval_expr cursor ctx |> it |> Value.get_num
      in
      Types.FIntT width
  | FBitT expr_width ->
      let expr_width_il = type_expr cursor ctx expr_width in
      let width =
        expr_width_il |> Static.eval_expr cursor ctx |> it |> Value.get_num
      in
      Types.FBitT width
  | VBitT expr_width ->
      let expr_width_il = type_expr cursor ctx expr_width in
      let width =
        expr_width_il |> Static.eval_expr cursor ctx |> it |> Value.get_num
      in
      Types.VBitT width
  | StackT (typ_inner, expr_size) ->
      let typ_inner = eval_type cursor ctx typ_inner in
      let expr_size_il = type_expr cursor ctx expr_size in
      let size =
        expr_size_il |> Static.eval_expr cursor ctx |> it |> Value.get_num
      in
      Types.StackT (typ_inner.it, size)
  | TupleT typs_inner ->
      let typs_inner =
        List.map (eval_type cursor ctx) typs_inner |> List.map it
      in
      Types.TupleT typs_inner
  (* When a type variable is bound by the type parameters (top-scoped variable should be unallowed) *)
  (* (TODO) Why not shove in the type parameters into the typedef environment? *)
  | NameT { it = Current id; _ }
    when Ctx.find_tparam_opt cursor id.it ctx |> Option.is_some ->
      Types.VarT id.it
  | NameT var ->
      let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
      if Option.is_none td then (
        Format.printf "(eval_type') Type definition %a does not exist\n"
          El.Pp.pp_var var;
        assert false);
      let td = Option.get td in
      specialize_typedef td []
  | SpecT (var, typs) ->
      let td = Ctx.find_opt Ctx.find_typedef_opt cursor var ctx in
      if Option.is_none td then (
        Format.printf "(eval_type') Type definition %a does not exist\n"
          El.Pp.pp_var var;
        assert false);
      let td = Option.get td in
      let typs = List.map (eval_type cursor ctx) typs |> List.map it in
      specialize_typedef td typs
  | AnyT -> Types.TopT

and specialize_typedef (td : TypeDef.t) (targs : Type.t list) : Type.t =
  let check_arity tparams =
    if List.length targs <> List.length tparams then (
      Format.printf
        "(specialize_typedef) Type definition %a expects %d type arguments but \
         %d were given\n"
        TypeDef.pp td (List.length tparams) (List.length targs);
      assert false)
  in
  match td with
  (* Aliased types are not generic *)
  | DefD typ_inner ->
      check_arity [];
      typ_inner
  | NewD (id, typ_inner) ->
      check_arity [];
      Types.NewT (id, typ_inner)
  (* Constant types are not generic *)
  | EnumD (id, _members) ->
      check_arity [];
      Types.EnumT id
  | SEnumD (id, typ_inner, _fields) ->
      check_arity [];
      Types.SEnumT (id, typ_inner)
  (* Aggregate types are not generic (yet, to be added in v1.2.2) *)
  | StructD (id, fields) ->
      check_arity [];
      Types.StructT (id, fields)
  | HeaderD (id, fields) ->
      check_arity [];
      Types.HeaderT (id, fields)
  | UnionD (id, fields) ->
      check_arity [];
      Types.UnionT (id, fields)
  (* Object types are generic *)
  | ExternD (id, tparams, fdenv) ->
      check_arity tparams;
      let theta = List.combine tparams targs |> Subst.Theta.of_list in
      let fdenv = Envs.FDEnv.map (Subst.subst_funcdef theta) fdenv in
      Types.ExternT (id, fdenv)
  | ParserD (tparams, params) ->
      check_arity tparams;
      let theta = List.combine tparams targs |> Subst.Theta.of_list in
      let params = List.map (Subst.subst_param theta) params in
      Types.ParserT params
  | ControlD (tparams, params) ->
      check_arity tparams;
      let theta = List.combine tparams targs |> Subst.Theta.of_list in
      let params = List.map (Subst.subst_param theta) params in
      Types.ControlT params
  | PackageD tparams ->
      check_arity tparams;
      Types.PackageT

and specialize_funcdef (fd : FuncDef.t) (targs : Type.t list) :
    FuncType.t * TId.t list option =
  let check_arity tparams targs =
    if List.length targs <> List.length tparams then (
      Format.printf
        "(specialize_funcdef) Function %a expects %d type arguments but %d \
         were given\n"
        FuncDef.pp fd (List.length tparams) (List.length targs);
      assert false)
  in
  let fresh_tid () = "__WILD_" ^ string_of_int (Ctx.fresh ()) in
  let fresh_targ tid = Types.VarT tid in
  let specialize_funcdef' funcDef tparams targs params typ_ret =
    let targs, tids_fresh =
      (* Insert fresh type variables if omitted *)
      if List.length tparams > 0 && List.length targs = 0 then
        let tids_fresh =
          List.init (List.length tparams) (fun _ -> fresh_tid ())
        in
        let targs = List.map fresh_targ tids_fresh in
        (targs, Some tids_fresh)
        (* Otherwise, check the arity and insert fresh type variables for top types *)
      else (
        check_arity tparams targs;
        let targs, tids_fresh =
          List.fold_left
            (fun (targs, tids_fresh) targ ->
              match (targ : Type.t) with
              | TopT ->
                  let tid_fresh = fresh_tid () in
                  let targ_fresh = fresh_targ tid_fresh in
                  (targs @ [ targ_fresh ], tids_fresh @ [ tid_fresh ])
              | _ -> (targs @ [ targ ], tids_fresh))
            ([], []) targs
        in
        let tids_fresh = if tids_fresh = [] then None else Some tids_fresh in
        (targs, tids_fresh))
    in
    let theta = List.combine tparams targs |> Subst.Theta.of_list in
    let params = List.map (Subst.subst_param theta) params in
    let typ_ret = Subst.subst_typ theta typ_ret in
    (funcDef params typ_ret, tids_fresh)
  in
  match fd with
  (* action is not generic *)
  | ActionD params ->
      check_arity [] targs;
      (ActionT params, None)
  (* extern function, function, and extern (abstract) method are generic *)
  | ExternFunctionD (tparams, params, typ_ret) ->
      let funcDef params typ_ret = Types.ExternFunctionT (params, typ_ret) in
      specialize_funcdef' funcDef tparams targs params typ_ret
  | FunctionD (tparams, params, typ_ret) ->
      let funcDef params typ_ret = Types.FunctionT (params, typ_ret) in
      specialize_funcdef' funcDef tparams targs params typ_ret
  | ExternMethodD (tparams, params, typ_ret) ->
      let funcDef params typ_ret = Types.ExternMethodT (params, typ_ret) in
      specialize_funcdef' funcDef tparams targs params typ_ret
  | ExternAbstractMethodD (tparams, params, typ_ret) ->
      let funcDef params typ_ret =
        Types.ExternAbstractMethodT (params, typ_ret)
      in
      specialize_funcdef' funcDef tparams targs params typ_ret

and specialize_consdef (cd : ConsDef.t) (targs : Type.t list) :
    ConsType.t * TId.t list option =
  let check_arity tparams targs =
    if List.length targs <> List.length tparams then (
      Format.printf
        "(specialize_consdef) Constructor %a expects %d type arguments but %d \
         were given\n"
        ConsDef.pp cd (List.length tparams) (List.length targs);
      assert false)
  in
  let fresh_tid () = "__WILD_" ^ string_of_int (Ctx.fresh ()) in
  let fresh_targ tid = Types.VarT tid in
  let tparams, cparams, typ = cd in
  (* Insert fresh type variables if omitted
     Otherwise, check the arity and insert fresh type variables for top types *)
  let targs, tids_fresh =
    if List.length tparams > 0 && List.length targs = 0 then
      let tids_fresh =
        List.init (List.length tparams) (fun _ -> fresh_tid ())
      in
      let targs = List.map fresh_targ tids_fresh in
      (targs, Some tids_fresh)
    else (
      check_arity tparams targs;
      let targs, tids_fresh =
        List.fold_left
          (fun (targs, tids_fresh) targ ->
            match (targ : Type.t) with
            | TopT ->
                let tid_fresh = fresh_tid () in
                let targ_fresh = fresh_targ tid_fresh in
                (targs @ [ targ_fresh ], tids_fresh @ [ tid_fresh ])
            | _ -> (targs @ [ targ ], tids_fresh))
          ([], []) targs
      in
      let tids_fresh = if tids_fresh = [] then None else Some tids_fresh in
      (targs, tids_fresh))
  in
  let theta = List.combine tparams targs |> Subst.Theta.of_list in
  let cparams = List.map (Subst.subst_cparam theta) cparams in
  let typ = Subst.subst_typ theta typ in
  let ct = (cparams, typ) in
  (ct, tids_fresh)

(* Annotation typing *)

and type_anno (cursor : Ctx.cursor) (ctx : Ctx.t) (anno : El.Ast.anno) :
    Il.Ast.anno =
  type_anno' cursor ctx anno.it $ anno.at

and type_anno' (cursor : Ctx.cursor) (ctx : Ctx.t) (anno : El.Ast.anno') :
    Il.Ast.anno' =
  match anno with
  | EmptyN text -> Lang.Ast.EmptyN text
  | TextN (text, texts) -> Lang.Ast.TextN (text, texts)
  | ExprN (text, exprs) ->
      let exprs = List.map (type_expr cursor ctx) exprs in
      Lang.Ast.ExprN (text, exprs)
  | RecordN (text, fields) ->
      let members, exprs = List.split fields in
      let exprs = List.map (type_expr cursor ctx) exprs in
      Lang.Ast.RecordN (text, List.combine members exprs)

(* Parameter typing *)

and type_param (cursor : Ctx.cursor) (ctx : Ctx.t) (param : El.Ast.param) :
    Il.Ast.param =
  type_param' cursor ctx param.it $ param.at

and type_param' (cursor : Ctx.cursor) (ctx : Ctx.t) (param : El.Ast.param') :
    Il.Ast.param' =
  let id, dir, typ, expr_default, annos = param in
  let typ = eval_type_with_check cursor ctx typ in
  let value_default_il =
    Option.map
      (fun expr_default ->
        let expr_default_il = type_expr cursor ctx expr_default in
        Static.eval_expr cursor ctx expr_default_il)
      expr_default
  in
  let annos_il = List.map (type_anno cursor ctx) annos in
  (* (TODO) evaluate annotations *)
  (id, dir, typ, value_default_il, annos_il)

and type_cparam (cursor : Ctx.cursor) (ctx : Ctx.t) (cparam : El.Ast.cparam) :
    Il.Ast.cparam =
  type_param cursor ctx cparam

(* Calling convention *)

and type_call_convention (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : Types.param list) (args_il_typed : (Il.Ast.arg * Type.t) list) :
    Il.Ast.arg list =
  List.map2 (type_call_convention' cursor ctx) params args_il_typed

and type_call_convention' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (param : Types.param) (arg_il_typed : Il.Ast.arg * Type.t) : Il.Ast.arg =
  let arg_il, typ_arg = arg_il_typed in
  let _, dir_param, typ_param, _ = param in
  let type_expr_arg (expr_il : Il.Ast.expr) =
    match dir_param with
    | Lang.Ast.In -> coerce_type_assign expr_il typ_param
    | Lang.Ast.Out | Lang.Ast.InOut ->
        Eq.check_eq_typ_alpha typ_arg typ_param;
        check_lvalue cursor ctx expr_il;
        expr_il
    | Lang.Ast.No ->
        Eq.check_eq_typ_alpha typ_arg typ_param;
        Static.check_ctk expr_il;
        expr_il
  in
  match (arg_il.it : Il.Ast.arg') with
  | ExprA expr_il ->
      let expr_il = type_expr_arg expr_il in
      Lang.Ast.ExprA expr_il $ arg_il.at
  | NameA (id, Some expr_il) ->
      let expr_il = type_expr_arg expr_il in
      Lang.Ast.NameA (id, Some expr_il) $ arg_il.at
  | NameA (_, None) | AnyA ->
      if dir_param <> Lang.Ast.Out then (
        Format.printf
          "(type_call) Don't care argument can only be used for an out \
           function/method argument\n";
        assert false);
      Lang.Ast.AnyA $ arg_il.at

(* Expression typing *)

and type_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : El.Ast.expr) :
    Il.Ast.expr =
  let typ, ctk, expr_il = type_expr' cursor ctx expr.it in
  Il.Ast.(expr_il $$ expr.at % { typ; ctk })

and type_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : El.Ast.expr') :
    Type.t * Ctk.t * Il.Ast.expr' =
  match expr with
  | BoolE { boolean } -> type_bool_expr boolean
  | StrE { text } -> type_str_expr text
  | NumE { num } -> type_num_expr num
  | VarE { var } -> type_var_expr cursor ctx var
  | SeqE { exprs } -> type_seq_expr cursor ctx exprs
  | RecordE { fields } -> type_record_expr cursor ctx fields
  | UnE { unop; expr } -> type_unop_expr cursor ctx unop expr
  | BinE { binop; expr_l; expr_r } ->
      type_binop_expr cursor ctx binop expr_l expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      type_ternop_expr cursor ctx expr_cond expr_then expr_else
  | CastE { typ; expr } -> type_cast_expr cursor ctx typ expr
  | MaskE { expr_base; expr_mask } ->
      type_mask_expr cursor ctx expr_base expr_mask
  | RangeE { expr_lb; expr_ub } -> type_range_expr cursor ctx expr_lb expr_ub
  | SelectE { exprs_select; cases } ->
      type_select_expr cursor ctx exprs_select cases
  | ArrAccE { expr_base; expr_idx } ->
      type_array_acc_expr cursor ctx expr_base expr_idx
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      type_bitstring_acc_expr cursor ctx expr_base expr_lo expr_hi
  | TypeAccE { var_base; member } ->
      type_type_acc_expr cursor ctx var_base member
  | ErrAccE { member } -> type_error_acc_expr cursor ctx member
  | ExprAccE { expr_base; member } ->
      type_expr_acc_expr cursor ctx expr_base member
  | CallFuncE { var_func; targs; args } ->
      type_call_func_expr cursor ctx var_func targs args
  | CallMethodE { expr_base; member; targs; args } ->
      type_call_method_expr cursor ctx expr_base member targs args
  | InstE { var_inst; targs; args } ->
      type_instantiation_expr cursor ctx var_inst targs args

and type_bool_expr (boolean : bool) : Type.t * Ctk.t * Il.Ast.expr' =
  let value = Value.BoolV boolean in
  (Types.BoolT, Ctk.LCTK, Il.Ast.ValueE { value = value $ no_info })

and type_str_expr (text : El.Ast.text) : Type.t * Ctk.t * Il.Ast.expr' =
  let value = Value.StrV text.it in
  (Types.StrT, Ctk.LCTK, Il.Ast.ValueE { value = value $ text.at })

and type_num_expr (num : El.Ast.num) : Type.t * Ctk.t * Il.Ast.expr' =
  let value, typ =
    match num.it with
    | value, Some (width, signed) ->
        if signed then (Value.FIntV (width, value), Types.FIntT width)
        else (Value.FBitV (width, value), Types.FBitT width)
    | value, None -> (Value.IntV value, Types.IntT)
  in
  let ctk = Ctk.LCTK in
  let expr_il = Il.Ast.ValueE { value = value $ num.at } in
  (typ, ctk, expr_il)

and type_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : El.Ast.var) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let rtype = Ctx.find_opt Ctx.find_rtype_opt cursor var ctx in
  if Option.is_none rtype then (
    Format.printf "(type_var_expr) %a is a free identifier\n" El.Pp.pp_var var;
    assert false);
  let typ, _, _ = Option.get rtype in
  let expr_il = Il.Ast.VarE { var } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.12) Operations on tuple expressions

   The empty tuple expression has type tuple<> - a tuple with no components. *)

and type_seq_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : El.Ast.expr list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  let exprs_il = List.map (type_expr cursor ctx) exprs in
  let typs =
    List.map note exprs_il |> List.map (fun Il.Ast.{ typ; _ } -> typ)
  in
  let typ = Types.SeqT typs in
  WF.check_valid_type cursor ctx typ;
  let expr_il = Il.Ast.SeqE { exprs = exprs_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.13) Operations on structure-valued expressions

   One can write expressions that evaluate to a structure or header.

   For a structure-valued expression typeRef is the name of a struct or header type.
   The typeRef can be omitted if it can be inferred from context, e.g., when initializing a variable with a struct type.
   Structure-valued expressions that evaluate to a value of some header type are always valid.

   Structure-valued expressions can be used in the right-hand side of assignments, in comparisons,
   in field selection expressions, and as arguments to functions, method or actions.
   Structure-valued expressions are not left values.

   Structure-valued expressions that do not have ... as their last element must provide a value
   for every member of the struct or header type to which it evaluates, by mentioning each field name exactly once.

   Structure-valued expressions that have ... as their last element are allowed to give values to only
   a subset of the fields of the struct or header type to which it evaluates.
   Any field names not given a value explicitly will be given their default value (see Section 8.26).

   The order of the fields of the struct or header type does not need to
   match the order of the values of the structure-valued expression.

   It is a compile-time error if a field name appears more than once in the same structure-valued expression. *)

and type_record_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (El.Ast.member * El.Ast.expr) list) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let members, exprs = List.split fields in
  let exprs_il = List.map (type_expr cursor ctx) exprs in
  let typs =
    List.map note exprs_il |> List.map (fun Il.Ast.{ typ; _ } -> typ)
  in
  let typ =
    let members = List.map it members in
    Types.RecordT (List.combine members typs)
  in
  WF.check_valid_type cursor ctx typ;
  let expr_il = Il.Ast.RecordE { fields = List.combine members exprs_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.6) Operations on fixed-width bit types (unsigned integers)

   This section discusses all operations that can be performed on expressions of
   type bit<W> for some width W, also known as bit-strings.

   Each of the following operations produces a bit-string result
   when applied to bit-strings of the same width:

    - Negation, denoted by unary -.
        The result is computed by subtracting the value from 2W.
        The result is unsigned and has the same width as the input.
        The semantics is the same as the C negation of unsigned numbers.
    - Unary plus, denoted by +. This operation behaves like a no-op.
    - Bitwise “complement” of a single bit-string, denoted by ~.

   (8.7) Operations on fixed-width signed integers

   This section discusses all operations that can be performed on expressions of type int<W> for some W.
   Recall that the int<W> denotes signed W-bit integers, represented using two's complement.

   The int<W> datatype supports the following operations;
   all binary operations require both operands to have the exact same type.
   The result always has the same width as the left operand.

    - Negation, denoted by unary -.
    - Unary plus, denoted by +. This operation behaves like a no-op.
    - Bitwise “complement” of a single bit-string, denoted by ~.

   (8.8) Operations on arbitrary-precision integers

   The type int denotes arbitrary-precision integers.
   In P4, all expressions of type int must be compile-time known values.
   The type int supports the following operations:

    - Negation, denoted by unary -
    - Unary plus, denoted by +. This operation behaves like a no-op.

   Note: bitwise-operations (|,&,^,~) are not defined on expressions of type int.
   In addition, it is illegal to apply division and modulo to negative values. *)

and type_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : El.Ast.unop)
    (expr : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  let typ =
    match unop.it with
    | BNotOp ->
        if not (match typ with FIntT _ | FBitT _ -> true | _ -> false) then (
          Format.printf
            "(type_unop_expr) ~ expects a fixed-size integer but %a was given\n"
            (El.Pp.pp_expr ~level:0) expr;
          assert false);
        typ
    | LNotOp ->
        if not (match typ with BoolT -> true | _ -> false) then (
          Format.printf
            "(type_unop_expr) ! expects a boolean but %a was given\n"
            (El.Pp.pp_expr ~level:0) expr;
          assert false);
        typ
    | UMinusOp ->
        if not (match typ with IntT | FIntT _ | FBitT _ -> true | _ -> false)
        then (
          Format.printf
            "(type_unop_expr) - expects an integer but %a was given\n"
            (El.Pp.pp_expr ~level:0) expr;
          assert false);
        typ
  in
  let expr_il = Il.Ast.UnE { unop; expr = expr_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.2) Operaions on error types

    The error type only supports equality (==) and inequality (!=) comparisons.
    The result of such a comparison is a Boolean value.

    (8.3) Operations on enum types

    Similar to errors, enum expressions without a specified underlying type only support
    equality (==) and inequality (!=) comparisons.

    (8.4) Operations on match_kind types

    They support only assignment and comparisons for equality and inequality.

    (8.5) Expressions on Booleans

    The following operations are provided on Boolean expressions:

     - “And”, denoted by &&
     - “Or”, denoted by ||
     - Negation, denoted by !
     - Equality and inequality tests, denoted by == and != respectively.

    (8.6) Operations on fixed-width bit types (unsigned integers)

    All binary operations except shifts and concatenation require both operands to have the same exact type and width;
    supplying operands with different widths produces an error at compile time.
    No implicit casts are inserted by the compiler to equalize the widths.
    There are no other binary operations that accept signed and unsigned values simultaneously besides shifts and concatenation.
    The following operations are provided on bit-string expressions:

     - Test for equality between bit-strings of the same width, designated by ==. The result is a Boolean value.
     - Test for inequality between bit-strings of the same width, designated by !=. The result is a Boolean value.
     - Unsigned comparisons <,>,<=,>=. Both operands must have the same width and the result is a Boolean value.

   Each of the following operations produces a bit-string result when applied to bit-strings of the same width:

     - Addition, denoted by +.
     - Subtraction, denoted by -.
         The result is unsigned, and has the same type as the operands.
     - Multiplication, denoted by *.
         The result has the same width as the operands and is computed by truncating the result to the output's width.
         P4 architectures may impose additional restrictions
         — e.g., they may only allow multiplication by a non-negative integer power of two.
     - Bitwise “and” between two bit-strings of the same width, denoted by &.
     - Bitwise “or” between two bit-strings of the same width, denoted by |.
     - Bitwise “complement” of a single bit-string, denoted by ~.
     - Bitwise “xor” of two bit-strings of the same width, denoted by ^.
     - Saturating addition, denoted by |+|.
     - Saturating subtraction, denoted by |-|.

    Bit-strings also support the following operations:

     - Logical shift left and right by a (not-necessarily-known-at-compile-time) non-negative integer value,
       denoted by << and >> respectively. In a shift, the left operand is unsigned, and right operand must be either
       an expression of type bit<S> or a non-negative integer value that is known at compile time.
       The result has the same type as the left operand.

    (8.7) Operations on fixed-width signed integers

    All binary operations except shifts and concatenation require both operands to have the same exact type (signedness)
    and width and supplying operands with different widths or signedness produces a compile-time error.
    No implicit casts are inserted by the compiler to equalize the types. Except for shifts and concatenation,
    P4 does not have any binary operations that operate simultaneously on signed and unsigned values.

    Note that bitwise operations on signed integers are well-defined, since the representation is mandated to be two's complement.

    The int<W> datatype supports the following operations; all binary operations require both operands to have the exact same type.
    The result always has the same width as the left operand.

     - Addition, denoted by +.
     - Subtraction, denoted by -.
     - Comparison for equality and inequality, denoted == and != respectively.
         These operations produce a Boolean result.
     - Numeric comparisons, denoted by <,<=,>, and >=.
         These operations produce a Boolean result.
     - Multiplication, denoted by *.
         Result has the same width as the operands.
         P4 architectures may impose additional restrictions
         —e.g., they may only allow multiplication by a power of two.
     - Bitwise “and” between two bit-strings of the same width, denoted by &.
     - Bitwise “or” between two bit-strings of the same width, denoted by |.
     - Bitwise “complement” of a single bit-string, denoted by ~.
     - Bitwise “xor” of two bit-strings of the same width, denoted by ^.
     - Saturating addition, denoted by |+|.
     - Saturating subtraction, denoted by |-|.

    The int<W> datatype also support the following operations:

     - Arithmetic shift left and right denoted by << and >>.
       The left operand is signed and the right operand must be either an unsigned number of type bit<S>
       or a non-negative integer compile-time known value. The result has the same type as the left operand.
     - Concatenation of bit-strings and/or fixed-width signed integers, denoted by ++.
       The two operands must be either bit<W> or int<W>, and they can be of different signedness and width.
       The result has the same signedness as the left operand and the width equal to the sum of the two operands' width.

    (8.8) Operations on arbitrary-precsion integers

    The type int denotes arbitrary-precision integers. In P4, all expressions of type int must be compile-time known values. The type int supports the following operations:

     - Addition, denoted by +.
     - Subtraction, denoted by -.
     - Comparison for equality and inequality, denoted by == and != respectively.
         These operations produce a Boolean result.
     - Numeric comparisons <,<=,>, and >=.
         These operations produce a Boolean result.
     - Multiplication, denoted by *.
     - Truncating integer division between positive values, denoted by /.
     - Modulo between positive values, denoted by %.
     - Arithmetic shift left and right denoted by << and >>.
         These operations produce an int result.
         The right operand must be either an unsigned constant of type bit<S> or a non-negative integer compile-time known value.

    Each operand that participates in any of these operation must have type int (except shifts).
    Binary operations cannot be used to combine values of type int with values of a fixed-width type (except shifts).
    However, the compiler automatically inserts casts from int to fixed-width types in certain situations—see Section 8.11.

    Note: bitwise-operations (|,&,^,~) are not defined on expressions of type int.
          In addition, it is illegal to apply division and modulo to negative values.
    Note: saturating arithmetic is not supported for arbitrary-precision integers.

    (8.9) Concatentation and shifts

    (8.9.1) Concatenation

    Concatenation is applied to two bit-strings (signed or unsigned). It is denoted by the infix operator ++.
    The result is a bit-string whose length is the sum of the lengths of the inputs
    where the most significant bits are taken from the left operand; the sign of the result is taken from the left operand.

    (8.9.2) A note about shifts

    The left operand of shifts can be any one out of unsigned bit-strings, signed bit-strings,
    and arbitrary-precision integers, and the right operand of shifts must be either an expression of type bit<S>
    or a non-negative integer compile-time known value. The result has the same type as the left operand.

    (8.10) Operations on variable-size bit types

    To support parsing headers with variable-length fields, P4 offers a type varbit.
    Each occurrence of the type varbit has a statically-declared maximum width, as well as a dynamic width,
    which must not exceed the static bound. Prior to initialization a variable-size bit-string has an unknown dynamic width.

    Variable-length bit-strings support a limited set of operations:

     - Assignment to another variable-sized bit-string.
         The target of the assignment must have the same static width as the source.
         When executed, the assignment sets the dynamic width of the target to the dynamic width of the source.
     - Comparison for equality or inequality with another varbit field.
         Two varbit fields can be compared only if they have the same type.
         Two varbits are equal if they have the same dynamic width and all the bits up to the dynamic width are the same.

    (8.12) Operations on tuple expressions

    Tuples can be compared for equality using == and !=; two tuples are equal if and only if all their fields are respectively equal.

    (8.16) Operations on struct types

    Two structs can be compared for equality (==) or inequality (!=) only if they
    have the same type and all of their fields can be recursively compared for equality.
    Two structures are equal if and only if all their corresponding fields are equal.

    (8.17) Operations on header types

    Two headers can be compared for equality (==) or inequality (!=) only if they
    have the same type. Two headers are equal if and only if they are both invalid,
    or they are both valid and all their corresponding fields are equal.

    (8.18) Operations on header stacks

    Two header stacks can be compared for equality (==) or inequality (!=) only if they
    have the same element type and the same length. Two stacks are equal if and only if
    all their corresponding elements are equal. Note that the nextIndex value is
    not used in the equality comparison.

    (8.19) Operations on header unions

    Two header unions can be compared for equality (==) or inequality (!=) if they
    have the same type. The unions are equal if and only if all their corresponding fields are equal
    (i.e., either all fields are invalid in both unions, or in both unions the same field is valid,
    and the values of the valid fields are equal as headers).

    (8.23) Operations on types introduced by type

    Values with a type introduced by the type keyword provide only a few operations:

     - comparisons for equality and inequality if the original type supported such comparisons *)

and type_binop_plus_minus_mult (binop : Lang.Ast.binop)
    (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr'
    =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let typ, expr_l_il, expr_r_il =
    coerce_types_binary_numeric expr_l_il expr_r_il
  in
  match typ with
  | IntT | FIntT _ | FBitT _ ->
      let expr_il =
        Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il }
      in
      (typ, expr_il)
  | _ ->
      Format.printf
        "(type_binop_plus_minus_mult) Addition, subtraction, and \
         multiplication operator expects either two arbitrary precision \
         integers or \n\
        \        two integers of the same signedness and width but %a and %a \
         were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_saturating_plus_minus (binop : Lang.Ast.binop)
    (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr'
    =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let typ, expr_l_il, expr_r_il =
    coerce_types_binary_numeric expr_l_il expr_r_il
  in
  match typ with
  | FIntT _ | FBitT _ ->
      let expr_il =
        Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il }
      in
      (typ, expr_il)
  | _ ->
      Format.printf
        "(type_binop_saturating_plus_minus) Saturating addition and \
         subtraction operator expects two integers of the same signedness and \
         width but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_div_mod (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  match (typ_l, typ_r) with
  (* (TODO) Non-negativity can only be checked dynamically *)
  | IntT, IntT ->
      let typ = typ_l in
      let expr_il =
        Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il }
      in
      (typ, expr_il)
  | _ ->
      Format.printf
        "(type_binop_div) Division and modulo operator expects two arbitrary \
         precision integers but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_shift (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let ctk_r = expr_r_il.note.ctk in
  let typ, expr_l_il, expr_r_il =
    match (typ_l, typ_r) with
    | FBitT _, FBitT _ ->
        let typ = typ_l in
        (typ, expr_l_il, expr_r_il)
    | FBitT _, IntT when Ctk.is_ctk ctk_r ->
        let typ = typ_l in
        (typ, expr_l_il, expr_r_il)
    | FIntT _, FBitT _ ->
        let typ = typ_l in
        (typ, expr_l_il, expr_r_il)
    | FIntT _, IntT when Ctk.is_ctk ctk_r ->
        let typ = typ_l in
        (typ, expr_l_il, expr_r_il)
    | IntT, FBitT _ ->
        let typ = typ_l in
        (typ, expr_l_il, expr_r_il)
    | IntT, IntT when Ctk.is_ctk ctk_r ->
        let typ = typ_l in
        (typ, expr_l_il, expr_r_il)
    | _ ->
        Format.printf
          "(type_binop_shift) Shift operator expects an unsigned bitstring, \
           signed bitstring, or an integer as the left operand and an unsigned \
           bitstring or a compile-time known integer as the right operand but \
           %a and %a were given\n"
          Type.pp typ_l Type.pp typ_r;
        assert false
  in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and type_binop_compare (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let typ, expr_l_il, expr_r_il =
    coerce_types_binary_numeric expr_l_il expr_r_il
  in
  match typ with
  | IntT | FIntT _ | FBitT _ ->
      let typ = Types.BoolT in
      let expr_il =
        Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il }
      in
      (typ, expr_il)
  | _ ->
      Format.printf
        "(type_binop_compare) Comparison expects either two arbitrary \
         precision integers or two integers of the same signedness and width \
         but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_compare_equal (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let _, expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let typ = Types.BoolT in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and type_binop_bitwise (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let typ, expr_l_il, expr_r_il =
    coerce_types_binary_numeric expr_l_il expr_r_il
  in
  match typ with
  | FIntT _ | FBitT _ ->
      let expr_il =
        Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il }
      in
      (typ, expr_il)
  | _ ->
      Format.printf
        "(type_binop_bitwise) Bitwise operator expects two bit-strings of the \
         same width but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

(* (TODO) What is the rationale for disallowing implicit cast here? *)
and type_binop_concat (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let typ =
    match (typ_l, typ_r) with
    | FIntT width_l, FIntT width_r -> Types.FIntT Bigint.(width_l + width_r)
    | FIntT width_l, FBitT width_r -> Types.FIntT Bigint.(width_l + width_r)
    | FBitT width_l, FIntT width_r -> Types.FBitT Bigint.(width_l + width_r)
    | FBitT width_l, FBitT width_r -> Types.FBitT Bigint.(width_l + width_r)
    | _ ->
        Format.printf
          "(type_binop_concat) Concatenation expects two bit-strings but %a \
           and %a were given\n"
          Type.pp typ_l Type.pp typ_r;
        assert false
  in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and type_binop_logical (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  match (typ_l, typ_r) with
  | BoolT, BoolT ->
      let typ = Types.BoolT in
      let expr_il =
        Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il }
      in
      (typ, expr_il)
  | _ ->
      Format.printf
        "(type_binop_logical_expr) Logical operator expects two boolean \
         operands but %a and %a were given\n"
        Type.pp typ_l Type.pp typ_r;
      assert false

and type_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : El.Ast.binop)
    (expr_l : El.Ast.expr) (expr_r : El.Ast.expr) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_l_il = type_expr cursor ctx expr_l in
  let expr_r_il = type_expr cursor ctx expr_r in
  let typ, expr_il =
    match binop.it with
    | PlusOp | MinusOp | MulOp ->
        type_binop_plus_minus_mult binop expr_l_il expr_r_il
    | SPlusOp | SMinusOp ->
        type_binop_saturating_plus_minus binop expr_l_il expr_r_il
    | DivOp | ModOp -> type_binop_div_mod binop expr_l_il expr_r_il
    | ShlOp | ShrOp -> type_binop_shift binop expr_l_il expr_r_il
    | LeOp | GeOp | LtOp | GtOp -> type_binop_compare binop expr_l_il expr_r_il
    | EqOp | NeOp -> type_binop_compare_equal binop expr_l_il expr_r_il
    | BAndOp | BXorOp | BOrOp -> type_binop_bitwise binop expr_l_il expr_r_il
    | ConcatOp -> type_binop_concat binop expr_l_il expr_r_il
    | LAndOp | LOrOp -> type_binop_logical binop expr_l_il expr_r_il
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.5.1) Conditional operator

   A conditional expression of the form e1 ? e2 : e3 behaves the same as in languages like C.
   As described above, the expression e1 is evaluated first, and second either e2 or e3 is evaluated depending
   on the result.

   The first sub-expression e1 must have Boolean type and the second and third sub-expressions must have the same type,
   which cannot both be arbitrary-precision integers unless the condition itself can be evaluated at compilation time.
   This restriction is designed to ensure that the width of the result of the conditional expression can be inferred
   statically at compile time. *)

and type_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_cond : El.Ast.expr) (expr_then : El.Ast.expr)
    (expr_else : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_cond_il = type_expr cursor ctx expr_cond in
  let typ_cond = expr_cond_il.note.typ in
  let ctk_cond = expr_cond_il.note.ctk in
  if typ_cond <> Types.BoolT then (
    Format.printf "(type_ternop_expr) Condition %a must be a boolean\n"
      (El.Pp.pp_expr ~level:0) expr_cond;
    assert false);
  let expr_then_il = type_expr cursor ctx expr_then in
  let expr_else_il = type_expr cursor ctx expr_else in
  let typ, expr_then_il, expr_else_il =
    coerce_types_binary expr_then_il expr_else_il
  in
  if (not (Ctk.is_ctk ctk_cond)) && typ = Types.IntT then (
    Format.printf
      "(type_ternop_expr) Branches %a and %a cannot both be \
       arbitrary-precision integers when the condition %a is not compile-time \
       known\n"
      (El.Pp.pp_expr ~level:0) expr_then (El.Pp.pp_expr ~level:0) expr_else
      (El.Pp.pp_expr ~level:0) expr_cond;
    assert false);
  let expr_il =
    Il.Ast.TernE
      {
        expr_cond = expr_cond_il;
        expr_then = expr_then_il;
        expr_else = expr_else_il;
      }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.11) Casts

   P4 provides a limited set of casts between types. A cast is written (t) e,
   where t is a type and e is an expression. Casts are only permitted on base types and derived types
   introduced by typedef, type, and enum. *)

and type_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ)
    (expr : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let typ_target = eval_type_with_check cursor ctx typ in
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  if not (Subtyp.explicit typ typ_target.it) then (
    Format.printf "(type_cast_expr) Invalid cast from %a to %a\n" Type.pp typ
      Type.pp typ_target.it;
    assert false);
  let typ = typ_target.it in
  let expr_il = Il.Ast.CastE { typ = typ_target; expr = expr_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.15.3) Masks

   The infix operator &&& takes two arguments of the same numeric type (Section 7.4),
   and creates a value of the same type. The right value is used as a “mask”,
   where each bit set to 0 in the mask indicates a “don't care” bit.

   Similar to other binary operations, the mask operator allows the compiler to
   automatically insert casts to unify the argument types in certain situations (section 8.11.2).

   P4 architectures may impose additional restrictions on the expressions on the left and
   right-hand side of a mask operator: for example, they may require that
   either or both sub-expressions be compile-time known values. *)

and type_mask_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : El.Ast.expr)
    (expr_mask : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let expr_mask_il = type_expr cursor ctx expr_mask in
  let typ, expr_base_il, expr_mask_il =
    coerce_types_binary_numeric expr_base_il expr_mask_il
  in
  if not (Type.is_numeric typ) then (
    Format.printf "(type_mask_expr) Incompatible type %a for range operation\n"
      Type.pp typ;
    assert false);
  let typ = Types.SetT typ in
  WF.check_valid_type cursor ctx typ;
  let expr_il =
    Il.Ast.MaskE { expr_base = expr_base_il; expr_mask = expr_mask_il }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.15.4) Ranges

   The infix operator .. takes two arguments of the same numeric type T (Section 7.4),
   and creates a value of the type set<T>. The set contains all values numerically between
   the first and the second, inclusively.

   Similar to other binary operations, the range operator allows the compiler to
   automatically insert casts to unify the argument types in certain situations (section 8.11.2). *)

and type_range_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_lb : El.Ast.expr)
    (expr_ub : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_lb_il = type_expr cursor ctx expr_lb in
  let expr_ub_il = type_expr cursor ctx expr_ub in
  let typ, expr_lb_il, expr_ub_il =
    coerce_types_binary_numeric expr_lb_il expr_ub_il
  in
  if not (Type.is_numeric typ) then (
    Format.printf "(type_range_expr) Incompatible type %a for range operation\n"
      Type.pp typ;
    assert false);
  let typ = Types.SetT typ in
  WF.check_valid_type cursor ctx typ;
  let expr_il = Il.Ast.RangeE { expr_lb = expr_lb_il; expr_ub = expr_ub_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (13.6) Select expressions

   A select expression evaluates to a state.

   Each expression in the expressionList must have a type of
   bit<W>, int<W>, bool, enum, serializable enum, or a tuple type with fields of one of the above types.

   In a select expression, if the expressionList has type tuple<T>,
   then each keysetExpression must have type set<tuple<T>>.
   In particular, if a set is specified as a range or mask expression, the endpoints of the range
   and mask expression are implicitly cast to type T using the standard rules for casts. *)

(* (8.15.1) Singleton sets

   In a set context, expressions denote singleton sets.

   (8.15.2) The universal set

   In a set context, the expressions default and _ denote the universal set,
   which contains all possible values of a given type. *)

and type_keyset (cursor : Ctx.cursor) (ctx : Ctx.t) (typ_key : Type.t)
    (keyset : El.Ast.keyset) : Il.Ast.keyset =
  type_keyset' cursor ctx typ_key keyset.it $ keyset.at

and type_keyset' (cursor : Ctx.cursor) (ctx : Ctx.t) (typ_key : Type.t)
    (keyset : El.Ast.keyset') : Il.Ast.keyset' =
  assert (cursor = Ctx.Local);
  match keyset with
  | ExprK expr ->
      let expr_il =
        match expr.it with
        | MaskE _ | RangeE _ -> type_expr cursor ctx expr
        | _ ->
            let expr_il = type_expr cursor ctx expr in
            let typ = Types.SetT expr_il.note.typ in
            WF.check_valid_type cursor ctx typ;
            Il.Ast.(expr_il.it $$ expr_il.at % { typ; ctk = expr_il.note.ctk })
      in
      let typ = expr_il.note.typ in
      let expr_il =
        match (typ_key, typ) with
        | SetT typ_key_inner, SetT typ_inner ->
            let expr_il =
              { expr_il with note = { expr_il.note with typ = typ_inner } }
            in
            coerce_type_assign expr_il typ_key_inner
        | _ ->
            Format.printf
              "(type_keyset) Key type %a and the type %a of the keyset \
               expression %a must be set types\n"
              Type.pp typ_key Type.pp typ (Il.Pp.pp_expr ~level:0) expr_il;
            assert false
      in
      Lang.Ast.ExprK expr_il
  | DefaultK -> Lang.Ast.DefaultK
  | AnyK -> Lang.Ast.AnyK

and type_keysets (cursor : Ctx.cursor) (ctx : Ctx.t) (typs_key : Type.t list)
    (keysets : El.Ast.keyset list) : Il.Ast.keyset list =
  match (typs_key, keysets) with
  | _, [ { it = DefaultK; at; _ } ] -> [ Lang.Ast.DefaultK $ at ]
  | _, [ { it = AnyK; at; _ } ] -> [ Lang.Ast.AnyK $ at ]
  | typs_key, keysets ->
      if List.length typs_key <> List.length keysets then (
        Format.printf
          "(type_keysets) Number of select keys must match the number of cases\n";
        assert false);
      List.map2 (type_keyset cursor ctx) typs_key keysets

and type_select_case (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typs_key : Type.t list) (case : El.Ast.select_case) : Il.Ast.select_case =
  type_select_case' cursor ctx typs_key case.it $ case.at

and type_select_case' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typs_key : Type.t list) (case : El.Ast.select_case') : Il.Ast.select_case'
    =
  assert (cursor = Ctx.Local);
  let keysets, state_label = case in
  let keysets_il = type_keysets cursor ctx typs_key keysets in
  let typdir_label = Ctx.find_rtype_opt cursor state_label.it ctx in
  if
    not
      (match typdir_label with Some (Types.StateT, _, _) -> true | _ -> false)
  then (
    Format.printf "(type_transition_stmt) Label %s is not a valid label\n"
      state_label.it;
    assert false);
  (keysets_il, state_label)

and type_select_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs_select : El.Ast.expr list) (cases : El.Ast.select_case list) :
    Type.t * Ctk.t * Il.Ast.expr' =
  if
    not
      (cursor = Ctx.Local
      && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
  then (
    Format.printf
      "(type_select_expr) Select expression must be in a parser state (more \
       strictly, only nested inside a transition statement)\n";
    assert false);
  let exprs_select_il =
    List.map
      (fun expr_select ->
        let expr_select_il = type_expr cursor ctx expr_select in
        let typ_select = Types.SetT expr_select_il.note.typ in
        WF.check_valid_type cursor ctx typ_select;
        Il.Ast.(
          expr_select_il.it
          $$ expr_select_il.at
             % { typ = typ_select; ctk = expr_select_il.note.ctk }))
      exprs_select
  in
  let typs_select =
    List.map (fun expr -> Il.Ast.(expr.note.typ)) exprs_select_il
  in
  List.iter (WF.check_valid_type cursor ctx) typs_select;
  let cases_il = List.map (type_select_case cursor ctx typs_select) cases in
  let typ = Types.StateT in
  let expr_il =
    Il.Ast.SelectE { exprs_select = exprs_select_il; cases = cases_il }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.12) Operations on tuple expressions

   The fields of a tuple can be accessed using array index syntax x[0], x[1].
   The array indexes must be compile-time constants,
   to enable the type-checker to identify the field types statically.

   (8.18) Operations on header stacks

   Given a header stack value hs of size n, the following expressions are legal:

    - hs[index]: produces a reference to the header at the specified position within the stack;
      if hs is an l-value, the result is also an l-value. The header may be invalid.
      Some implementations may impose the constraint that the index expression evaluates to a value
      that is known at compile time. A P4 compiler must give an error if an index value that
      is a compile-time constant is out of range.
      Accessing a header stack hs with an index less than 0 or greater than or equal to hs.size
      results in an undefined value. See Section 8.25 for more details.
      The index is an expression that must be of numeric types (Section 7.4). *)

(* (DESIGN CHOICE)

   p4cherry type checker does not restrict that index expr for a header stack access
   should be a compile-time known value *)

and type_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_idx : El.Ast.expr) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let expr_idx_il = type_expr cursor ctx expr_idx in
  let typ_base, typ_idx = (expr_base_il.note.typ, expr_idx_il.note.typ) in
  if not (Type.is_numeric typ_idx) then (
    Format.printf "(type_array_acc_expr) Index %a must be of numeric type\n"
      (El.Pp.pp_expr ~level:0) expr_idx;
    assert false);
  let typ, expr_il =
    match typ_base with
    | TupleT typs_base_inner ->
        let value_idx = Static.eval_expr cursor ctx expr_idx_il in
        let idx =
          value_idx.it |> Value.get_num |> Bigint.to_int |> Option.get
        in
        if idx < 0 || idx >= List.length typs_base_inner then (
          Format.printf "(type_array_acc_expr) Index %d out of range for %a\n"
            idx (El.Pp.pp_expr ~level:0) expr_base;
          assert false);
        let typ = List.nth typs_base_inner idx in
        let expr_idx_il =
          Il.Ast.(
            ValueE { value = value_idx } $$ expr_idx_il.at % expr_idx_il.note)
        in
        let expr_il =
          Il.Ast.ArrAccE { expr_base = expr_base_il; expr_idx = expr_idx_il }
        in
        (typ, expr_il)
    | StackT (typ_base_inner, _) ->
        let typ = typ_base_inner in
        let expr_il =
          Il.Ast.ArrAccE { expr_base = expr_base_il; expr_idx = expr_idx_il }
        in
        (typ, expr_il)
    | _ ->
        Format.printf "(type_array_acc_expr) %a cannot be indexed\n"
          (El.Pp.pp_expr ~level:0) expr_base;
        assert false
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.6) Operations on fixed-width bit types (unsigned integers)

   Bit-strings also support the following operations:

    - Extraction of a set of contiguous bits, also known as a slice, denoted by [H:L],
      where H and L must be expressions that evaluate to non-negative compile-time known values, and H >= L.
      The types of H and L (which do not need to be identical) must be numeric (Section 7.4).
      The result is a bit-string of width H - L + 1, including the bits numbered from L
      (which becomes the least significant bit of the result) to H (the most significant bit of the result)
      from the source operand. The conditions 0 <= L <= H < W are checked statically
      (where W is the length of the source bit-string). Note that both endpoints of the extraction are inclusive.
      The bounds are required to be known-at-compile-time values so that the result width can be computed at
      compile time. Slices are also l-values, which means that P4 supports assigning to a slice:  e[H:L] = x .
      The effect of this statement is to set bits H through L (inclusive of both) of e to the
      bit-pattern represented by x, and leaves all other bits of e unchanged.
      A slice of an unsigned integer is an unsigned integer.

   (8.7) Operations on fixed-width signed integers

    - Extraction of a set of contiguous bits, also known as a slice, denoted by [H:L],
      where H and L must be expressions that evaluate to non-negative compile-time known values,
      and H >= L must be true.
      The result is an unsigned bit-string of width H - L + 1, including the bits numbered from L
      (which becomes the least significant bit of the result) to H (the most significant bit of the result)
      from the source operand.

   (8.8) Operations on arbitrary-precision integers

   Bit slices, denoted by [H:L], where H and L must be expressions that evaluate to
   non-negative compile-time known values, and H >= L must be true. The types of H and L
   (which do not need to be identical) must be one of the following:

    - int - an arbitrary-precision integer (section 7.1.6.5)
    - bit<W> - a W-bit unsigned integer where W >= 0 (section 7.1.6.2)
    - int<W> - a W-bit signed integer where W >= 1 (section 7.1.6.3)
    - a serializable enum with an underlying type that is bit<W> or int<W> (section 7.2.1). *)

and check_bitstring_base' (typ : Type.t) : bool =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT -> false
  | IntT -> true
  | FIntT width -> Bigint.(width > zero)
  | FBitT width -> Bigint.(width >= zero)
  | VBitT _ | VarT _ -> false
  | NewT _ | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
  | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ | TopT
  | SeqT _ | RecordT _ | SetT _ | StateT ->
      false

and check_bitstring_base (typ : Type.t) : unit =
  if not (check_bitstring_base' typ) then (
    Format.printf "(check_bitstring_base) %a is not a valid base type\n" Type.pp
      typ;
    assert false)

and check_bitstring_index' (typ : Type.t) : bool =
  match typ with
  | VoidT | ErrT | MatchKindT | StrT | BoolT -> false
  | IntT | FIntT _ | FBitT _ -> true
  | VBitT _ | VarT _ -> false
  | NewT _ | EnumT _ -> false
  | SEnumT (_, typ_inner) -> check_bitstring_index' typ_inner
  | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _ -> false
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ | TopT | SeqT _
  | RecordT _ | SetT _ | StateT ->
      false

and check_bitstring_index (typ : Type.t) : unit =
  if not (check_bitstring_index' typ) then (
    Format.printf "(check_bitstring_index) %a is not a valid index type\n"
      Type.pp typ;
    assert false)

and check_bitstring_slice_range' (typ_base : Type.t) (width_slice : Bigint.t) :
    bool =
  match typ_base with
  | VoidT | ErrT | MatchKindT | StrT | BoolT -> false
  | IntT -> true
  | FIntT width_base | FBitT width_base -> Bigint.(width_slice <= width_base)
  | VBitT _ | VarT _ -> false
  | NewT _ | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _
  | UnionT _ | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ | TopT
  | SeqT _ | RecordT _ | SetT _ | StateT ->
      false

and check_bitstring_slice_range (typ_base : Type.t) (idx_lo : Bigint.t)
    (idx_hi : Bigint.t) : unit =
  let width_slice = Bigint.(idx_hi - idx_lo + one) in
  if
    Bigint.(idx_lo < zero)
    || Bigint.(idx_hi < zero)
    || Bigint.(idx_lo > idx_hi)
    || not (check_bitstring_slice_range' typ_base width_slice)
  then (
    Format.printf "(type_bitstring_acc_expr) Invalid slice [%a:%a] for %a\n"
      Bigint.pp idx_lo Bigint.pp idx_hi Type.pp typ_base;
    assert false)

and type_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_lo : El.Ast.expr) (expr_hi : El.Ast.expr) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let typ_base = expr_base_il.note.typ in
  check_bitstring_base typ_base;
  let expr_lo_il = type_expr cursor ctx expr_lo in
  let typ_lo = expr_lo_il.note.typ in
  check_bitstring_index typ_lo;
  let value_lo = Static.eval_expr cursor ctx expr_lo_il in
  let idx_lo = value_lo.it |> Value.get_num in
  let expr_hi_il = type_expr cursor ctx expr_hi in
  let typ_hi = expr_hi_il.note.typ in
  check_bitstring_index typ_hi;
  let value_hi = Static.eval_expr cursor ctx expr_hi_il in
  let idx_hi = value_hi.it |> Value.get_num in
  check_bitstring_slice_range typ_base idx_lo idx_hi;
  let width_slice = Bigint.(idx_hi - idx_lo + one) in
  let typ = Types.FBitT width_slice in
  let expr_il =
    Il.Ast.BitAccE { expr_base = expr_base_il; value_lo; value_hi }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.2) Operations on error types

   Symbolic names declared by an error declaration belong to the error namespace. *)

and type_error_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (member : El.Ast.member) : Type.t * Ctk.t * Il.Ast.expr' =
  let value_error = Ctx.find_value_opt cursor ("error." ^ member.it) ctx in
  if Option.is_none value_error then (
    Format.printf "(type_error_acc_expr) Member %s does not exist in error\n"
      member.it;
    assert false);
  let value_error = Option.get value_error in
  let typ = Types.ErrT in
  let expr_il = Il.Ast.ValueE { value = value_error $ member.at } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.3) Operations on enum types

   Symbolic names declared by an enum belong to the namespace introduced by the enum declaration
   rather than the top-level namespace. *)

and type_type_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_base : El.Ast.var) (member : El.Ast.member) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let td_base = Ctx.find_opt Ctx.find_typedef_opt cursor var_base ctx in
  if Option.is_none td_base then (
    Format.printf "(type_type_acc_expr) %a is a free identifier\n" El.Pp.pp_var
      var_base;
    assert false);
  let td_base = Option.get td_base in
  let typ, value =
    match td_base with
    | EnumD (id, members) ->
        if not (List.mem member.it members) then (
          Format.printf "(type_type_acc_expr) Member %s does not exist in %a\n"
            member.it TypeDef.pp td_base;
          assert false);
        let typ = Types.EnumT id in
        let value = Value.EnumFieldV (id, member.it) in
        (typ, value)
    | SEnumD (id, typ_inner, fields) ->
        if not (List.mem_assoc member.it fields) then (
          Format.printf "(type_type_acc_expr) Member %s does not exist in %a\n"
            member.it TypeDef.pp td_base;
          assert false);
        let typ = Types.SEnumT (id, typ_inner) in
        let value_inner = List.assoc member.it fields in
        let value = Value.SEnumFieldV (id, member.it, value_inner) in
        (typ, value)
    | _ ->
        Format.printf "(type_type_acc_expr) %a cannot be accessed\n" TypeDef.pp
          td_base;
        assert false
  in
  let expr_il = Il.Ast.ValueE { value = value $ member.at } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.16) Operations on struct types

   The only operation defined on expressions whose type is a struct is field access,
   written using dot (“.”) notation—e.g., s.field.
   If s is an l-value, then s.field is also an l-value.

   (8.17) Operations on headers

   Headers provide the same operations as structs.

   (8.18) Operatins on header stacks

   Given a header stack value hs of size n, the following expressions are legal:

   - hs.size: produces a 32-bit unsigned integer that returns the size
     of the header stack (a compile-time constant).
   - hs.next: produces a reference to the element with index hs.nextIndex in the stack.
     May only be used in a parser. If the stack's nextIndex counter is greater than or equal to size,
     then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
     If hs is an l-value, then hs.next is also an l-value.
   - hs.last: produces a reference to the element with index hs.nextIndex - 1 in the stack, if such an element exists.
     May only be used in a parser. If the nextIndex counter is less than 1, or greater than size,
     then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
     Unlike hs.next, the resulting reference is never an l-value.
   - hs.lastIndex: produces a 32-bit unsigned integer that encodes the index hs.nextIndex - 1.
     May only be used in a parser. If the nextIndex counter is 0, then evaluating this expression produces an undefined value.

   (8.19) Operations on header unions *)

and type_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (member : El.Ast.member) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let typ_base = expr_base_il.note.typ in
  let typ =
    match typ_base with
    | StackT (typ_inner, _) -> (
        match member.it with
        | "size" | "lastIndex" -> Types.FBitT (Bigint.of_int 32)
        | "next" | "last" -> typ_inner
        | _ ->
            Format.printf
              "(type_expr_acc_expr) Invalid member %s for header stack\n"
              member.it;
            assert false)
    | StructT (_id, fields) | HeaderT (_id, fields) | UnionT (_id, fields) ->
        let typ_inner = List.assoc_opt member.it fields in
        if Option.is_none typ_inner then (
          Format.printf "(type_expr_acc_expr) Member %s does not exist in %a\n"
            member.it (El.Pp.pp_expr ~level:0) expr_base;
          assert false);
        Option.get typ_inner
    | _ ->
        Format.printf "(type_expr_acc_expr) %a cannot be accessed\n"
          (El.Pp.pp_expr ~level:0) expr_base;
        assert false
  in
  let expr_il = Il.Ast.ExprAccE { expr_base = expr_base_il; member } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.20) Method invocations and function calls

   A function call or method invocation can optionally specify for each argument the corresponding parameter name.
   It is illegal to use names only for some arguments: either all or no arguments must specify the parameter name.
   Function arguments are evaluated in the order they appear, left to right, before the function invocation takes place.

   The calling convention is copy-in/copy-out (Section 6.8).
   For generic functions the type arguments can be explicitly specified in the function call.
   The compiler only inserts implicit casts for direction in arguments to methods or functions as described in Section 8.11.
   The types for all other arguments must match the parameter types exactly.

   The result returned by a function call is discarded when the function call is used as a statement.

   The “don't care” identifier (_) can only be used for an out function/method argument,
   when the value of returned in that argument is ignored by subsequent computations.
   When used in generic functions or methods, the compiler may reject the program if it is
   unable to infer a type for the don't care argument. *)

(* (6.8) Calling convention: call by copy in/copy out

   Invocations are executed using copy-in/copy-out semantics.

   Each parameter may be labeled with a direction:

   - in parameters are read-only. It is an error to use an in parameter on the left-hand side of an assignment
     or to pass it to a callee as a non-in argument.
   - out parameters are, with a few exceptions listed below, uninitialized and are treated as l-values (See Section 6.7)
     within the body of the method or function. An argument passed as an out parameter must be an l-value;
   - inout parameters behave like a combination of in and out parameters simultaneously:
     In consequence, an argument passed as an inout parameter must be an l-value.
   - The meaning of parameters with no direction depends upon the kind of entity the parameter is for:
      - For anything other than an action, e.g. a control, parser, or function, a directionless parameter means that
        the value supplied as an argument in a call must be a compile-time known value (see Section 18.1).
      - For an action, a directionless parameter indicates that it is “action data”.
        See Section 14.1 for the meaning of action data. *)

(* (Appendix F) Restrictions on compile time and run time calls

   The next table lists restrictions on what kinds of calls can be made from which places in a P4 program.
   Calling a parser, control, or table means invoking its apply() method.
   Calling a value-set means using it in a select expression.
   The row for extern describes where extern method calls can be made from.

   One way that an extern can be called from the top level of a parser or control is in an initializer expression
   for a declared variable, e.g. bit<32> x = rand.get();.

                | can be called at run time from this place in a P4 program
   This type    | parser state | control apply	block | parser/control top level | action | extern | function
   package	    | N/A          | N/A                  | N/A                      | N/A    | N/A    | N/A
   parser       | yes          | no                   | no                       | no     | no     | no
   control      | no           | yes                  | no                       | no     | no     | no
   extern       | yes          | yes                  | yes                      | yes    | no     | no
   table        | no           | yes                  | no                       | no     | no     | no
   value-set    | yes          | no                   | no                       | no     | no     | no
   action       | no           | yes                  | no                       | yes    | no     | no
   function     | yes          | yes                  | no                       | yes    | no     | yes
   value types	| N/A          | N/A                  | N/A                      | N/A    | N/A    | N/A

   There may not be any recursion in calls, neither by a thing calling itself directly, nor mutual recursion.
   An extern can never cause any other type of P4 program object to be called. See Section 6.8.1.
   Actions may be called directly from a control apply block.

   Note that while the extern row shows that extern methods can be called from many places,
   particular externs may have additional restrictions not listed in this table.
   Any such restrictions should be documented in the description for each extern,
   as part of the documentation for the architecture that defines the extern. *)

(* (8.17) Operations on headers

   In addition, headers support the following methods:

    - The method isValid() returns the value of the “validity” bit of the header.
    - The method setValid() sets the header's validity bit to “true”. It can only be applied to an l-value.
    - The method setInvalid() sets the header's validity bit to “false”. It can only be applied to an l-value.

   (8.18) Operations on header stacks

   Finally, P4 offers the following computations that can be used to manipulate
   the elements at the front and back of the stack:

    - hs.push_front(int count): shifts hs “right” by count. The first count elements become invalid.
      The last count elements in the stack are discarded. The hs.nextIndex counter is incremented by count.
      The count argument must be a positive integer that is a compile-time known value. The return type is void.

    - hs.pop_front(int count): shifts hs “left” by count (i.e., element with index count is copied in stack at index 0).
      The last count elements become invalid. The hs.nextIndex counter is decremented by count.
      The count argument must be a positive integer that is a compile-time known value. The return type is void.

   (8.19) Operations on header unions

   u.isValid() returns true if any member of the header union u is valid, otherwise it returns false.
   setValid() and setInvalid() methods are not defined for header unions.

   (9) Compile-time size determination

   The method calls minSizeInBits, minSizeInBytes, maxSizeInBits, and maxSizeInBytes can be applied to
   certain expressions. These method calls return the minimum (or maximum) size in bits (or bytes)
   required to store the expression. Thus, the result type of these methods has type int.
   Except in certain situations involving type variables, discussed below, these method calls produce
   local compile-time known values; otherwise they produce compile-time known values. None of these methods evaluate
   the expression that is the receiver of the method call, so it may be invalid (e.g., an out-of-bounds header stack access).

   The definition of e.minSizeInBits() and e.maxSizeInBits() is
   given recursively on the type of e as described in the following table:

   Type         |	minSizeInBits                                          | maxSizeInBits
   bit<N>       |	N	                                                     | N
   int<N>	      | N	                                                     | N
   bool	        | 1	                                                     | 1
   enum bit<N>  | N                                                      | N
   enum int<N>  | N                                                      | N
   tuple	      | foreach field(tuple) sum of	field.minSizeInBits()      | foreach field(tuple) sum of field.maxSizeInBits()
   varbit<N>    |	0                                                      | N
   struct       | foreach field(struct) sum of field.minSizeInBits()     | foreach field(struct) sum of field.maxSizeInBits()
   header       | foreach field(header) sum of field.minSizeInBits()     | foreach field(header) sum of field.maxSizeInBits()
   H[N]	        | N * H.minSizeInBits()                                  | N * H.maxSizeInBits()
   header_union	| max(foreach field(header_union)	field.minSizeInBits()) | max(foreach field(header_union) field.maxSizeInBits())

   The methods can also be applied to type name expressions e:

    - if the type of e is a type introduced by type, the result is the application of the method to the underlying type
    - if e is the name of a type (e.g., introduced by a typedef declaration), where the type given a name is one of the above,
      then the result is obtained by applying the method to the underlying type.

   These methods are defined for:

    - all serializable types
    - for a type that does not contain varbit fields, both methods return the same result
    - for a type that does contain varbit fields, maxSizeInBits is the worst-case size
        of the serialized representation of the data and minSizeInBits is the “best” case.
    - Every other case is undefined and will produce a compile-time error. *)

and check_call_arity (ft : FuncType.t) (params : Types.param list)
    (args : Il.Ast.arg list) : unit =
  if List.length params <> List.length args then (
    Format.printf
      "(check_call_arity) Function %a expects %d arguments but %d were given\n"
      FuncType.pp ft (List.length params) (List.length args);
    assert false)

and check_named_args (args : Il.Ast.arg list) : unit =
  let args = List.map it args in
  let is_named arg =
    match (arg : Il.Ast.arg') with NameA _ -> true | _ -> false
  in
  if
    not
      (List.for_all is_named args
      || List.for_all (fun arg -> not (is_named arg)) args)
  then (
    Format.printf
      "(check_named_args) Either all or no arguments must specify the \
       parameter name\n";
    assert false)

(* Invariant: parameters and arguments are checked of arity and all-or-nothing named *)
and align_params_with_args (params : Types.param list) (typ_args : Type.t list)
    (args_il : Il.Ast.arg list) =
  let module PMap = Map.Make (String) in
  let params_map =
    List.fold_left
      (fun params_map param ->
        let id, _, _, _ = param in
        PMap.add id param params_map)
      PMap.empty params
  in
  let args = List.combine args_il typ_args in
  List.fold_left2
    (fun (params, typ_args, args_il) param (arg_il, typ_arg) ->
      match (arg_il.it : Il.Ast.arg') with
      | ExprA _ | AnyA ->
          (params @ [ param ], typ_args @ [ typ_arg ], args_il @ [ arg_il ])
      | NameA (id, _) ->
          let param = PMap.find id.it params_map in
          (params @ [ param ], typ_args @ [ typ_arg ], args_il @ [ arg_il ]))
    ([], [], []) params args

and type_func (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : El.Ast.var)
    (targs_il : Il.Ast.targ list) (args : El.Ast.arg list) :
    FuncType.t * TId.t list option =
  let targs = List.map it targs_il in
  let fd =
    Ctx.find_overloaded_opt Ctx.find_funcdef_overloaded_opt cursor var_func args
      ctx
  in
  if Option.is_none fd then (
    Format.printf "(type_func) Function %a not found\n" El.Pp.pp_var var_func;
    assert false);
  let fd = Option.get fd in
  specialize_funcdef fd targs

and type_method (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : El.Ast.expr)
    (member : El.Ast.member) (targs_il : Il.Ast.targ list)
    (args : El.Ast.arg list) : FuncType.t * Il.Ast.expr * TId.t list option =
  let error_not_found () =
    Format.printf "(type_method) Method %s not found for %a\n" member.it
      (El.Pp.pp_expr ~level:0) expr_base;
    assert false
  in
  let targs = List.map it targs_il in
  let expr_base_il = type_expr cursor ctx expr_base in
  let typ_base = expr_base_il.note.typ in
  let ft, tids_fresh =
    match typ_base with
    | StackT _ -> (
        match member.it with
        | "push_front" | "pop_front" ->
            let params = [ ("count", Lang.Ast.No, Types.IntT, None) ] in
            let typ_ret = Types.VoidT in
            (Types.BuiltinMethodT (params, typ_ret), None)
        | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits"
        | "maxSizeInBytes" ->
            (Types.BuiltinMethodT ([], Types.IntT), None)
        | _ -> error_not_found ())
    | StructT _ -> (
        match member.it with
        | "minSizeInBits" | "minSizeInBytes" ->
            (Types.BuiltinMethodT ([], Types.IntT), None)
        | _ -> error_not_found ())
    | HeaderT _ -> (
        match member.it with
        | "isValid" -> (Types.BuiltinMethodT ([], Types.BoolT), None)
        | "setValid" | "setInvalid" ->
            (Types.BuiltinMethodT ([], Types.VoidT), None)
        | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits"
        | "maxSizeInBytes" ->
            (Types.BuiltinMethodT ([], Types.IntT), None)
        | _ -> error_not_found ())
    | UnionT _ -> (
        match member.it with
        | "isValid" -> (Types.BuiltinMethodT ([], Types.BoolT), None)
        | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits"
        | "maxSizeInBytes" ->
            (Types.BuiltinMethodT ([], Types.IntT), None)
        | _ -> error_not_found ())
    | ExternT (_id, fdenv) -> (
        let fd = Envs.FDEnv.find_overloaded_opt (member.it, args) fdenv in
        match fd with
        | Some fd -> specialize_funcdef fd targs
        | None -> error_not_found ())
    | ParserT params -> (
        match member.it with
        | "apply" -> (Types.ParserApplyMethodT params, None)
        | _ -> error_not_found ())
    | ControlT params -> (
        match member.it with
        | "apply" -> (Types.ControlApplyMethodT params, None)
        | _ -> error_not_found ())
    | TableT typ -> (
        match member.it with
        | "apply" -> (Types.TableApplyMethodT typ, None)
        | _ -> error_not_found ())
    | _ -> error_not_found ()
  in
  (ft, expr_base_il, tids_fresh)

and type_call (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tids_fresh : TId.t list option) (ft : FuncType.t)
    (targs_il : Il.Ast.targ list) (args : El.Ast.arg list) :
    Il.Ast.targ list * Il.Ast.arg list * Type.t =
  (* (TODO) Consider default parameters, in such case arity can appear to mismatch *)
  let params = FuncType.get_params ft in
  let args_il, typ_args = List.map (type_arg cursor ctx) args |> List.split in
  check_call_arity ft params args_il;
  check_named_args args_il;
  let params, typ_args, args_il =
    align_params_with_args params typ_args args_il
  in
  let args_il_typed = List.combine args_il typ_args in
  let typ_ret = FuncType.get_typ_ret ft in
  let targs_il, params, typ_ret =
    match tids_fresh with
    | Some tids_fresh ->
        let theta = infer_targs tids_fresh params args_il_typed in
        let targs_il =
          List.map
            (fun tid_fresh -> Subst.Theta.find tid_fresh theta)
            tids_fresh
          |> List.map (fun typ -> typ $ no_info)
        in
        let params = List.map (Subst.subst_param theta) params in
        let typ_ret = Subst.subst_typ theta typ_ret in
        (targs_il, params, typ_ret)
    | None -> (targs_il, params, typ_ret)
  in
  let args_il = type_call_convention cursor ctx params args_il_typed in
  (targs_il, args_il, typ_ret)

and type_call_func_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_func : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  (* Find the function definition and specialize it if generic *)
  let targs_il = List.map (eval_type_with_check cursor ctx) targs in
  (* (TODO) Need to check its well-formedness after specialization *)
  let ft, tids_fresh = type_func cursor ctx var_func targs_il args in
  (* (TODO) Implement restrictions on compile time and run time calls (Appendix F) *)
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, typ =
    type_call cursor ctx tids_fresh ft targs_il args
  in
  if typ = Types.VoidT then (
    Format.printf
      "(type_call_expr) Function call as an expression must return a value\n";
    assert false);
  let expr_il =
    Il.Ast.CallFuncE { var_func; targs = targs_il; args = args_il }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

and type_call_method_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (member : El.Ast.member)
    (targs : El.Ast.targ list) (args : El.Ast.arg list) :
    Type.t * Ctk.t * Il.Ast.expr' =
  (* Find the function definition and specialize it if generic *)
  let targs_il = List.map (eval_type_with_check cursor ctx) targs in
  (* (TODO) Need to check its well-formedness after specialization *)
  let ft, expr_base_il, tids_fresh =
    type_method cursor ctx expr_base member targs_il args
  in
  (* (TODO) Implement restrictions on compile time and run time calls (Appendix F) *)
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, typ =
    type_call cursor ctx tids_fresh ft targs_il args
  in
  if typ = Types.VoidT then (
    Format.printf
      "(type_call_expr) Function call as an expression must return a value\n";
    assert false);
  let expr_il =
    Il.Ast.CallMethodE
      { expr_base = expr_base_il; member; targs = targs_il; args = args_il }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.21) Constructor invocations

   The syntax for a constructor invocation is similar to a function call;
   constructors can also be called using named arguments. Constructors are evaluated entirely
   at compilation time (see Section 18). In consequence, all constructor arguments must also
   be expressions that can be evaluated at compilation time. When performing type inference
   and overload resolution, constructor invocations are treated similar to methods or functions. *)

(* (11.3) Instantiations

   Instantiations are similar to variable declarations, but are reserved for
   the types with constructors (extern objects, control blocks, parsers, and packages).

   An instantiation is written as a constructor invocation followed by a name.
   Instantiations are always executed at compilation time (Section 18.1).
   The effect is to allocate an object with the specified name, and to bind it to the
   result of the constructor invocation. Note that instantiation arguments can be specified by name. *)

(* (Appendix F) Restrictions on compile time and run time calls

   The next table lists restrictions on where one may perform instantiations (see Section 11.3)
   of different types. The answer for package is always “no” because there is no “inside a package”
   where instantiations can be written in P4_16. One can definitely make constructor calls and
   use instances of stateful types as parameters when instantiating a package,
   and restrictions on those types are in the table above.

   For externs, one can only specify their interface in P4_16, not their implementation.
   Thus there is no place to instantiate objects within an extern.

   You may declare variables and constants of any of the value types within a parser, control, or function
   (see Section 11.2 for more details). Declaring a variable or constant is not the same as instantiation,
   hence the answer “N/A” (for not applicable) in those table entries.
   Variables may not be declared at the top level of your program, but constants may.

               | can be instantiated in this place
   This type   | top level | package | parser | control | extern | function
   package     | yes       | no      | no     | no      | no     | no
   parser      | no        | no      | yes    | no      | no     | no
   control     | no        | no      | no     | yes     | no     | no
   extern      | yes       | no      | yes    | yes     | no     | no
   function    | yes       | no      | no     | no      | no     | no
   table       | no        | no      | no     | yes     | no     | no
   value-set   | yes       | no      | yes    | no      | no     | no
   value types | N/A       | N/A     | N/A    | N/A     | N/A    | N/A *)

and check_instantiation_arity (var_inst : El.Ast.var)
    (cparams : Types.cparam list) (args : Il.Ast.arg list) : unit =
  if List.length cparams <> List.length args then (
    Format.printf
      "(check_call_arity) Instance %a expects %d arguments but %d were given\n"
      El.Pp.pp_var var_inst (List.length cparams) (List.length args);
    assert false)

and align_cparams_with_args (cparams : Types.cparam list)
    (typ_args : Type.t list) (args_il : Il.Ast.arg list) =
  align_params_with_args cparams typ_args args_il

and type_instantiation (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  (* Find the constructor definition and specialize it if necessary *)
  (* (TODO) Implement type inference for missing type arguments *)
  let targs_il = List.map (eval_type_with_check cursor ctx) targs in
  let cd =
    Ctx.find_overloaded_opt Ctx.find_consdef_opt cursor var_inst args ctx
  in
  if Option.is_none cd then (
    Format.printf "(type_instantiation) %a is not an instance type\n"
      El.Pp.pp_var var_inst;
    assert false);
  let cd = Option.get cd in
  (* (TODO) Need to check its well-formedness after specialization *)
  let ct, tids_fresh =
    let targs = List.map it targs_il in
    specialize_consdef cd targs
  in
  (* (TODO) Implement restrictions on compile time and run time calls (Appendix F) *)
  (* Check if the arguments match the parameters *)
  let cparams, typ_inst = ct in
  let args_il, typ_args = List.map (type_arg cursor ctx) args |> List.split in
  check_instantiation_arity var_inst cparams args_il;
  check_named_args args_il;
  let cparams, typ_args, args_il =
    align_cparams_with_args cparams typ_args args_il
  in
  let args_il_typed = List.combine args_il typ_args in
  let targs_il, cparams, typ_inst =
    match tids_fresh with
    | Some tids_fresh ->
        let theta = infer_targs tids_fresh cparams args_il_typed in
        let targs_il =
          List.map
            (fun tid_fresh -> Subst.Theta.find tid_fresh theta)
            tids_fresh
          |> List.map (fun typ -> typ $ no_info)
        in
        let cparams = List.map (Subst.subst_cparam theta) cparams in
        let typ_inst = Subst.subst_typ theta typ_inst in
        (targs_il, cparams, typ_inst)
    | None -> (targs_il, cparams, typ_inst)
  in
  let args_il = type_call_convention cursor ctx cparams args_il_typed in
  let typ = typ_inst in
  let expr_il = Il.Ast.InstE { var_inst; targs = targs_il; args = args_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

and type_instantiation_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  type_instantiation cursor ctx var_inst targs args

(* Argument typing *)

and type_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : El.Ast.arg) :
    Il.Ast.arg * Type.t =
  let arg_il, typ = type_arg' cursor ctx arg.it in
  (arg_il $ arg.at, typ)

and type_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : El.Ast.arg') :
    Il.Ast.arg' * Type.t =
  match arg with
  | ExprA expr ->
      let expr_il = type_expr cursor ctx expr in
      let typ = expr_il.note.typ in
      let arg_il = Lang.Ast.ExprA expr_il in
      (arg_il, typ)
  | NameA (id, Some expr) ->
      let expr_il = type_expr cursor ctx expr in
      let typ = expr_il.note.typ in
      let arg_il = Lang.Ast.NameA (id, Some expr_il) in
      (arg_il, typ)
  | NameA (id, None) -> (Lang.Ast.NameA (id, None), TopT)
  | AnyA -> (Lang.Ast.AnyA, TopT)

(* Statement typing *)

let rec type_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmt : El.Ast.stmt) : Ctx.t * Flow.t * Il.Ast.stmt =
  let ctx, flow, stmt_il = type_stmt' cursor ctx flow stmt.it in
  (ctx, flow, stmt_il $ stmt.at)

and type_stmt' (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmt : El.Ast.stmt') : Ctx.t * Flow.t * Il.Ast.stmt' =
  if cursor <> Ctx.Local then (
    Format.printf "(type_stmt) Statements must be local\n";
    assert false);
  match stmt with
  | EmptyS -> (ctx, flow, Lang.Ast.EmptyS)
  | AssignS { expr_l; expr_r } -> type_assign_stmt cursor ctx flow expr_l expr_r
  | SwitchS { expr_switch; cases } ->
      type_switch_stmt cursor ctx flow expr_switch cases
  | IfS { expr_cond; stmt_then; stmt_else } ->
      type_if_stmt cursor ctx flow expr_cond stmt_then stmt_else
  | BlockS { block } -> type_block_stmt cursor ctx flow block
  | ExitS -> (ctx, flow, Lang.Ast.ExitS)
  | RetS { expr_ret } -> type_return_stmt cursor ctx flow expr_ret
  | CallFuncS { var_func; targs; args } ->
      type_call_func_stmt cursor ctx flow var_func targs args
  | CallMethodS { expr_base; member; targs; args } ->
      type_call_method_stmt cursor ctx flow expr_base member targs args
  | CallInstS { var_inst; targs; args } ->
      type_call_inst_stmt cursor ctx flow var_inst targs args
  | TransS { expr_label } -> type_transition_stmt cursor ctx flow expr_label
  | DeclS { decl } -> type_decl_stmt cursor ctx flow decl

and type_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmts : El.Ast.stmt list) : Ctx.t * Flow.t * Il.Ast.stmt list =
  List.fold_left
    (fun (ctx, flow, stmts_il) stmt ->
      let ctx, flow, stmt_il = type_stmt cursor ctx flow stmt in
      (ctx, flow, stmts_il @ [ stmt_il ]))
    (ctx, flow, []) stmts

(* (12.1) Assignment statement

   An assignment, written with the = sign, first evaluates its left sub-expression to an l-value,
   then evaluates its right sub-expression to a value, and finally copies the value into the l-value.
   Derived types (e.g. structs) are copied recursively, and all components of headers are copied,
   including “validity” bits. Assignment is not defined for extern values. *)

and type_assign_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_l : El.Ast.expr) (expr_r : El.Ast.expr) :
    Ctx.t * Flow.t * Il.Ast.stmt' =
  let expr_l_il = type_expr cursor ctx expr_l in
  let typ_l = expr_l_il.note.typ in
  check_lvalue cursor ctx expr_l_il;
  let expr_r_il = type_expr cursor ctx expr_r in
  let expr_r_il = coerce_type_assign expr_r_il typ_l in
  let stmt_il = Lang.Ast.AssignS { expr_l = expr_l_il; expr_r = expr_r_il } in
  (ctx, flow, stmt_il)

(* (12.7) Switch statement

   The switch statement can only be used within control blocks.

   There are two kinds of switch expressions allowed,
   described separately in the following two subsections.

   (12.7.3) Notes common to all switch statements

   It is a compile-time error if two labels of a switch statement equal each other.
   The switch label values need not include all possible values of the switch expression.
   It is optional to have a switch case with the default label, but if one is present,
   it must be the last one in the switch statement.

   If a switch label is not followed by a block statement, it falls through to the next label.
   However, if a block statement is present, it does not fall through. Note that this is different
   from C-style switch statements, where a break is needed to prevent fall-through.
   If the last switch label is not followed by a block statement, the behavior is the same
   as if the last switch label were followed by an empty block statement { }.

   When a switch statement is executed, first the switch expression is evaluated,
   and any side effects from evaluating this expression are visible to any switch case that is executed.
   Among switch labels that are not default, at most one of them can equal
   the value of the switch expression. If one is equal, that switch case is executed.

   If no labels are equal to the switch expression, then:

    - if there is a default label,
        the case with the default label is executed.
    - if there is no default label,
        then no switch case is executed, and execution continues after the end of the switch statement,
        with no side effects (except any that were caused by evaluating the switch expression). *)

and type_switch_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_switch : El.Ast.expr) (cases : El.Ast.switch_case list) :
    Ctx.t * Flow.t * Il.Ast.stmt' =
  let expr_switch_il = type_expr cursor ctx expr_switch in
  match expr_switch_il.it with
  | ExprAccE
      {
        expr_base =
          {
            it =
              CallMethodE
                {
                  expr_base =
                    { it = VarE { var = { it = Current id_table; _ } }; _ };
                  member = member_method;
                  _;
                };
            _;
          };
        member = member_acc;
      }
    when member_method.it = "apply" && member_acc.it = "action_run" ->
      type_switch_table_stmt cursor ctx flow expr_switch_il id_table cases
  | _ ->
      Format.printf
        "(type_switch_stmt) Switch statement with integer or enumerated type \
         expression not supported\n";
      assert false

(* (12.7.1) Switch statement with action_run expression

   For this variant of switch statement, the expression must be of the form
   t.apply().action_run, where t is the name of a table (see Section 14.2.2).
   All switch labels must be names of actions of the table t, or default.

   Note that the default label of the switch statement is used to match
   on the kind of action executed, no matter whether there was a table hit or miss.
   The default label does not indicate that the table missed and
   the default_action was executed. *)

and type_switch_table_label (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id_table : Il.Ast.id) (labels_seen : Il.Ast.switch_label' list)
    (label : El.Ast.switch_label) : unit =
  if List.mem label.it labels_seen then (
    Format.printf "(type_switch_table_label) Label %a was used multiple times\n"
      Il.Pp.pp_switch_label label;
    assert false);
  match label.it with
  | NameL id_action ->
      let id_enum = "action_list(" ^ id_table.it ^ ")" in
      let member = id_action.it in
      let id_field = id_enum ^ "." ^ member in
      let value_enum = Ctx.find_value_opt cursor id_field ctx in
      if
        not
          (match value_enum with
          | Some (Value.EnumFieldV (id_enum', member'))
            when id_enum = id_enum' && member = member' ->
              true
          | _ -> false)
      then (
        Format.printf
          "(type_switch_table_label) Action %a was not declared in table %a\n"
          Il.Pp.pp_id id_action Il.Pp.pp_id id_table;
        assert false)
  | DefaultL -> ()

and type_switch_table_case (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (id_table : Il.Ast.id) (labels_seen : Il.Ast.switch_label' list)
    (case : El.Ast.switch_case) :
    Flow.t * Il.Ast.switch_label * Il.Ast.switch_case =
  match case.it with
  | MatchC (label, block) ->
      type_switch_table_label cursor ctx id_table labels_seen label;
      let _ctx, flow, block_il = type_block cursor ctx flow block in
      let case_il = Lang.Ast.MatchC (label, block_il) $ case.at in
      (flow, label, case_il)
  | FallC label ->
      type_switch_table_label cursor ctx id_table labels_seen label;
      let case_il = Lang.Ast.FallC label $ case.at in
      (flow, label, case_il)

and type_switch_table_cases' (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (id_table : Il.Ast.id) (flows_case : Flow.t list)
    (labels_seen : Il.Ast.switch_label' list)
    (cases_il : Il.Ast.switch_case list) (cases : El.Ast.switch_case list) :
    Flow.t list * Il.Ast.switch_label' list * Il.Ast.switch_case list =
  match cases with
  | [] -> (flows_case, labels_seen, cases_il)
  | case :: cases ->
      let flow_case, label, case_il =
        type_switch_table_case cursor ctx flow id_table labels_seen case
      in
      if
        (match (label.it : Il.Ast.switch_label') with
        | DefaultL -> true
        | _ -> false)
        && List.length cases > 0
      then (
        Format.printf
          "(type_switch_table_cases') Default label must be the last switch \
           label\n";
        assert false);
      let flows_case = flows_case @ [ flow_case ] in
      let labels_seen = labels_seen @ [ label.it ] in
      let cases_il = cases_il @ [ case_il ] in
      type_switch_table_cases' cursor ctx flow id_table flows_case labels_seen
        cases_il cases

and type_switch_table_cases (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (id_table : Il.Ast.id) (cases : El.Ast.switch_case list) :
    Flow.t * Il.Ast.switch_case list =
  let flows_case, _, cases_il =
    type_switch_table_cases' cursor ctx flow id_table [] [] [] cases
  in
  let flow =
    if List.for_all (fun flow_case -> flow_case = Flow.Ret) flows_case then
      Flow.Ret
    else Flow.Cont
  in
  (flow, cases_il)

and type_switch_table_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_switch_il : Il.Ast.expr) (id_table : Il.Ast.id)
    (cases : El.Ast.switch_case list) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let flow, cases_il = type_switch_table_cases cursor ctx flow id_table cases in
  let stmt_il =
    Lang.Ast.SwitchS { expr_switch = expr_switch_il; cases = cases_il }
  in
  (ctx, flow, stmt_il)

(* (12.7.2) Switch statement with integer or enumerated type expression

   For this variant of switch statement, the expression must evaluate
   to a result with one of these types:

    - bit<W>
    - int<W>
    - enum, either with or without an underlying representation specified
    - error

   All switch labels must be expressions with compile-time known values,
   and must have a type that can be implicitly cast to the type of the
   switch expression (see Section 8.11.2). Switch labels must not begin with
   a left brace character {, to avoid ambiguity with a block statement. *)

(* (12.6) Conditional statement

   However, the condition expression in P4 is required to be a Boolean
   (and not an integer). *)

and type_if_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_cond : El.Ast.expr) (stmt_then : El.Ast.stmt)
    (stmt_else : El.Ast.stmt) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let expr_cond_il = type_expr cursor ctx expr_cond in
  let typ_cond = expr_cond_il.note.typ in
  if typ_cond <> Types.BoolT then (
    Format.printf "(type_if_stmt) Condition %a must be a boolean\n"
      (El.Pp.pp_expr ~level:0) expr_cond;
    assert false);
  let _ctx', flow_then, stmt_then_il = type_stmt cursor ctx flow stmt_then in
  let _ctx', flow_else, stmt_else_il = type_stmt cursor ctx flow stmt_else in
  let stmt_il =
    Lang.Ast.IfS
      {
        expr_cond = expr_cond_il;
        stmt_then = stmt_then_il;
        stmt_else = stmt_else_il;
      }
  in
  let flow = Flow.merge flow_then flow_else in
  (ctx, flow, stmt_il)

(* (12.3) Block statement

   It contains a sequence of statements and declarations, which are executed sequentially.
   The variables and constants within a block statement are only visible within the block. *)

(* (CHECK) Can context ever change after type checking a block? *)

and type_block (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (block : El.Ast.block) : Ctx.t * Flow.t * Il.Ast.block =
  let stmts, annos = block.it in
  let ctx = Ctx.enter_frame ctx in
  let ctx, flow, block_il = type_block' cursor ctx flow stmts annos in
  let ctx = Ctx.exit_frame ctx in
  (ctx, flow, block_il $ block.at)

and type_block' (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmts : El.Ast.stmt list) (annos : El.Ast.anno list) :
    Ctx.t * Flow.t * Il.Ast.block' =
  let annos_il = List.map (type_anno cursor ctx) annos in
  let ctx, flow, stmts_il = type_stmts cursor ctx flow stmts in
  let block_il = (stmts_il, annos_il) in
  (ctx, flow, block_il)

and type_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (block : El.Ast.block) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let ctx, flow, block_il = type_block cursor ctx flow block in
  let stmt_il = Lang.Ast.BlockS { block = block_il } in
  (ctx, flow, stmt_il)

(* (12.4) Return statement

   The return statement immediately terminates the execution of the action, function or control containing it.
   return statements are not allowed within parsers. return statements followed by an expression are only
   allowed within functions that return values; in this case the type of the expression must match the return type
   of the function. Any copy-out behavior due to direction out or inout parameters of the enclosing action, function,
   or control are still performed after the execution of the return statement.
   See Section 6.8 for details on copy-out behavior. *)

and type_return_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (_flow : Flow.t)
    (expr_ret : El.Ast.expr option) : Ctx.t * Flow.t * Il.Ast.stmt' =
  if
    not
      (match ctx.local.kind with
      | Function _ | Action | ExternAbstractMethod _ | ControlApplyMethod ->
          true
      | _ -> false)
  then (
    Format.printf
      "(type_return_stmt) Return statement must be in a function, action, \
       abstract extern method, and control method\n";
    assert false);
  let typ_ret_func =
    match ctx.local.kind with
    | Function typ_ret_func -> typ_ret_func
    | Action -> Types.VoidT
    | ExternAbstractMethod typ_ret_func -> typ_ret_func
    | ControlApplyMethod -> Types.VoidT
    | _ -> assert false
  in
  let expr_ret_il =
    match expr_ret with
    | Some expr_ret ->
        let expr_ret_il = type_expr cursor ctx expr_ret in
        let expr_ret_il = coerce_type_assign expr_ret_il typ_ret_func in
        Some expr_ret_il
    | None ->
        if typ_ret_func <> Types.VoidT then (
          Format.printf
            "(type_return_stmt) Function must return a value of type %a\n"
            Type.pp typ_ret_func;
          assert false);
        None
  in
  let stmt_il = Lang.Ast.RetS { expr_ret = expr_ret_il } in
  (ctx, Flow.Ret, stmt_il)

(* (8.20) Method invocations and function calls *)

and type_call_func_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (var_func : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Ctx.t * Flow.t * Il.Ast.stmt' =
  (* Find the function definition and specialize it is generic *)
  (* (TODO) Implement type inference for missing type arguments *)
  let targs_il = List.map (eval_type_with_check cursor ctx) targs in
  (* (TODO) Need to check its well-formedness after specialization *)
  let ft, tids_fresh = type_func cursor ctx var_func targs_il args in
  (* (TODO) Implement restrictions on compile time and run time calls (Appendix F) *)
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, _typ =
    type_call cursor ctx tids_fresh ft targs_il args
  in
  let stmt_il =
    Lang.Ast.CallFuncS { var_func; targs = targs_il; args = args_il }
  in
  (ctx, flow, stmt_il)

and type_call_method_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_base : El.Ast.expr) (member : El.Ast.member)
    (targs : El.Ast.targ list) (args : El.Ast.arg list) :
    Ctx.t * Flow.t * Il.Ast.stmt' =
  (* Find the function definition and specialize it is generic *)
  (* (TODO) Implement type inference for missing type arguments *)
  let targs_il = List.map (eval_type_with_check cursor ctx) targs in
  (* (TODO) Need to check its well-formedness after specialization *)
  let ft, expr_base_il, tids_fresh =
    type_method cursor ctx expr_base member targs_il args
  in
  (* (TODO) Implement restrictions on compile time and run time calls (Appendix F) *)
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, _typ =
    type_call cursor ctx tids_fresh ft targs_il args
  in
  let stmt_il =
    Lang.Ast.CallMethodS
      { expr_base = expr_base_il; member; targs = targs_il; args = args_il }
  in
  (ctx, flow, stmt_il)

(* (15.1) Direct type invocation

   Controls and parsers are often instantiated exactly once.
   As a light syntactic sugar, control and parser declarations with no
   constructor parameters may be applied directly, as if they were an instance.
   This has the effect of creating and applying a local instance of that type.

   For completeness, the behavior of directly invoking the same type
   more than once is defined as follows.

    - Direct type invocation in different scopes will result in different local
      instances with different fully-qualified control names.
    - In the same scope, direct type invocation will result in a different local
      instance per invocation—however, instances of the same type will share the
      same global name, via the @name annotation. If the type contains controllable
      entities, then invoking it directly more than once in the same scope is illegal,
      because it will produce multiple controllable entities with the
      same fully-qualified control name.

   No direct invocation is possible for controls or parsers that require
   constructor arguments. These need to be instantiated before they are invoked. *)

and type_call_inst_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Ctx.t * Flow.t * Il.Ast.stmt' =
  let typ, _, expr_il = type_instantiation cursor ctx var_inst targs [] in
  if not (match typ with ParserT _ | ControlT _ -> true | _ -> false) then (
    Format.printf
      "(type_call_inst_stmt) Direct type invocation is only defined for a \
       control or parser\n";
    assert false);
  let var_inst, targs_il =
    match expr_il with
    | Il.Ast.InstE { var_inst; targs = targs_il; _ } -> (var_inst, targs_il)
    | _ -> assert false
  in
  (* (TODO) Should handle the case where the same name is used in the same scope *)
  let id = Format.asprintf "%a" Il.Pp.pp_var var_inst $ no_info in
  let ctx = Ctx.add_rtype cursor id.it typ Lang.Ast.No Ctk.CTK ctx in
  let expr_base =
    El.Ast.VarE { var = Lang.Ast.Current id $ no_info } $ no_info
  in
  let member = "apply" $ no_info in
  let ctx, flow, stmt_il =
    type_call_method_stmt cursor ctx flow expr_base member [] args
  in
  let args_il =
    match stmt_il with
    | Lang.Ast.CallMethodS { args = args_il; _ } -> args_il
    | _ -> assert false
  in
  let stmt_il =
    Lang.Ast.CallInstS { var_inst; targs = targs_il; args = args_il }
  in
  (ctx, flow, stmt_il)

(* (13.5) Transition statements

   The last statement in a parser state is an optional transition statement,
   which transfers control to another state, possibly accept or reject. *)

and type_transition_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_label : El.Ast.expr) : Ctx.t * Flow.t * Il.Ast.stmt' =
  if not (match ctx.local.kind with Ctx.ParserState -> true | _ -> false) then (
    Format.printf
      "(type_transition_stmt) Transition statement must be in a parser state\n";
    assert false);
  let expr_label_il = type_expr cursor ctx expr_label in
  let typ_label = expr_label_il.note.typ in
  if not (match typ_label with StateT -> true | _ -> false) then (
    Format.printf "(type_transition_stmt) Label %a is not a valid label\n"
      (El.Pp.pp_expr ~level:0) expr_label;
    assert false);
  let stmt_il = Lang.Ast.TransS { expr_label = expr_label_il } in
  (ctx, flow, stmt_il)

and type_decl_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (decl : El.Ast.decl) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let ctx, decl_il = type_decl cursor ctx decl in
  let stmt_il =
    match decl_il with
    | Some decl_il -> Lang.Ast.DeclS { decl = decl_il }
    | None -> Lang.Ast.EmptyS
  in
  (ctx, flow, stmt_il)

(* Declaration typing *)

and type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : El.Ast.decl) :
    Ctx.t * Il.Ast.decl option =
  let at = decl.at in
  let ctx, decl = type_decl' cursor ctx decl.it in
  let decl = Option.map (fun decl -> decl $ at) decl in
  (ctx, decl)

and type_decl' (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : El.Ast.decl') :
    Ctx.t * Il.Ast.decl' option =
  let wrap_none ctx = (ctx, None) in
  let wrap_some (ctx, decl_il) = (ctx, Some decl_il) in
  match decl with
  (* Constant, variable, and object declarations *)
  | ConstD { id; typ; value; annos } ->
      type_const_decl cursor ctx id typ value annos |> wrap_some
  | VarD { id; typ; init; annos } ->
      type_var_decl cursor ctx id typ init annos |> wrap_some
  | InstD { id; var_inst; targs; args; init; annos } ->
      type_instantiation_decl cursor ctx id var_inst targs args init annos
      |> wrap_some
  (* Derived type declarations *)
  | ErrD { members } -> type_error_decl cursor ctx members |> wrap_none
  | MatchKindD { members } ->
      type_match_kind_decl cursor ctx members |> wrap_none
  | StructD { id; fields; annos } ->
      type_struct_decl cursor ctx id fields annos |> wrap_none
  | HeaderD { id; fields; annos } ->
      type_header_decl cursor ctx id fields annos |> wrap_none
  | UnionD { id; fields; annos } ->
      type_union_decl cursor ctx id fields annos |> wrap_none
  | EnumD { id; members; annos } ->
      type_enum_decl cursor ctx id members annos |> wrap_none
  | SEnumD { id; typ; fields; annos } ->
      type_senum_decl cursor ctx id typ fields annos |> wrap_none
  | NewTypeD { id; typdef; annos } ->
      type_newtype_decl cursor ctx id typdef annos |> wrap_none
  | TypeDefD { id; typdef; annos } ->
      type_typedef_decl cursor ctx id typdef annos |> wrap_none
  (* Function declarations *)
  | ActionD { id; params; body; annos } ->
      type_action_decl cursor ctx id params body annos |> wrap_some
  | FuncD { id; typ_ret; tparams; params; body } ->
      type_function_decl cursor ctx id tparams params typ_ret body |> wrap_some
  | ExternFuncD { id; typ_ret; tparams; params; annos } ->
      type_extern_function_decl cursor ctx id tparams params typ_ret annos
      |> wrap_some
  (* Object declarations *)
  (* Extern *)
  | ExternConstructorD { id; cparams; annos } ->
      type_extern_constructor_decl cursor ctx id cparams annos |> wrap_some
  | ExternAbstractMethodD { id; typ_ret; tparams; params; annos } ->
      type_extern_abstract_method_decl cursor ctx id tparams params typ_ret
        annos
      |> wrap_some
  | ExternMethodD { id; typ_ret; tparams; params; annos } ->
      type_extern_method_decl cursor ctx id tparams params typ_ret annos
      |> wrap_some
  | ExternObjectD { id; tparams; mthds; annos } ->
      type_extern_object_decl cursor ctx id tparams mthds annos |> wrap_some
  (* Parser *)
  | ValueSetD _ ->
      Format.printf "(type_decl) %a\n" (El.Pp.pp_decl' ~level:0) decl;
      assert false
  | ParserTypeD { id; tparams; params; annos } ->
      type_parser_type_decl cursor ctx id tparams params annos |> wrap_none
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      type_parser_decl cursor ctx id tparams params cparams locals states annos
      |> wrap_some
  (* Control *)
  | TableD { id; table; annos } ->
      type_table_decl cursor ctx id table annos |> wrap_some
  | ControlTypeD { id; tparams; params; annos } ->
      type_control_type_decl cursor ctx id tparams params annos |> wrap_none
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      type_control_decl cursor ctx id tparams params cparams locals body annos
      |> wrap_some
  (* Package *)
  | PackageTypeD { id; tparams; cparams; annos } ->
      type_package_type_decl cursor ctx id tparams cparams annos |> wrap_some

and type_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : El.Ast.decl list) :
    Ctx.t * Il.Ast.decl list =
  List.fold_left
    (fun (ctx, decls_il) decl ->
      let ctx, decl_il = type_decl cursor ctx decl in
      let decls_il =
        match decl_il with
        | Some decl_il -> decls_il @ [ decl_il ]
        | None -> decls_il
      in
      (ctx, decls_il))
    (ctx, []) decls

(* (11.1) Constants

   The initializer expression must be a compile-time known value. *)

and type_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr : El.Ast.expr) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  let annos_il = List.map (type_anno cursor ctx) annos in
  let typ_target = eval_type_with_check cursor ctx typ in
  let expr_il = type_expr cursor ctx expr in
  let expr_il = coerce_type_assign expr_il typ_target.it in
  let value = Static.eval_expr cursor ctx expr_il in
  let ctx =
    Ctx.add_value cursor id.it value.it ctx
    |> Ctx.add_rtype cursor id.it typ_target.it Lang.Ast.No Ctk.LCTK
  in
  let decl_il =
    Il.Ast.ConstD { id; typ = typ_target; value; annos = annos_il }
  in
  (ctx, decl_il)

(* (11.2) Variables

   Local variables are declared with a type, a name, and an optional initializer
   (as well as an optional annotation).

   Variable declarations without an initializer are uninitialized (except for headers and other header-related types,
   which are initialized to invalid in the same way as described for direction out parameters in Section 6.8).
   The language places few restrictions on the types of the variables: most P4 types that can be written explicitly can be used
   (e.g., base types, struct, header, header stack, tuple). However, it is impossible to declare variables with type int,
   or with types that are only synthesized by the compiler (e.g., set) In addition, variables of
   type parser, control, package, or extern types must be declared using instantiations (see Section 11.3).

   Reading the value of a variable that has not been initialized yields an undefined result.
   The compiler should attempt to detect and emit a warning in such situations.

   Variables declarations can appear in the following locations within a P4 program:

    - In a block statement,
    - In a parser state,
    - In an action body,
    - In a control block's apply sub-block,
    - In the list of local declarations in a parser, and
    - In the list of local declarations in a control.

   Variables have local scope, and behave like stack-allocated variables in languages such as C.
   The value of a variable is never preserved from one invocation of its enclosing block to the next.
   In particular, variables cannot be used to maintain state between different network packets. *)

(* (7.1.5) Strings

   There are no operations on string values; one cannot declare variables with a string type. *)

and check_valid_var_type (typ : Type.t) : unit =
  if not (check_valid_var_type' typ) then (
    Format.printf
      "(check_valid_var_type) Type %a is not a valid variable type\n" Type.pp
      typ;
    assert false)

and check_valid_var_type' (typ : Type.t) : bool =
  match typ with
  | VoidT -> false
  | ErrT | MatchKindT -> true
  | StrT -> false
  | BoolT -> true
  | IntT -> false
  | FIntT _ | FBitT _ | VBitT _ | VarT _ -> true
  | NewT (_, typ_inner) -> check_valid_var_type' typ_inner
  | EnumT _ | SEnumT _ | TupleT _ | StackT _ | StructT _ | HeaderT _ | UnionT _
    ->
      true
  | ExternT _ | ParserT _ | ControlT _ | PackageT | TableT _ | TopT | SeqT _
  | RecordT _ | SetT _ | StateT ->
      false

and type_var_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr_init : El.Ast.expr option)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if
    (not
       (cursor = Ctx.Block
       && match ctx.block.kind with Parser | Control -> true | _ -> false))
    && not
         (cursor = Ctx.Local
         &&
         match ctx.local.kind with
         | Function _ | Action | ExternAbstractMethod _ | ParserState
         | ControlApplyMethod ->
             true
         | _ -> false)
  then (
    Format.printf
      "(type_var_decl) Variables must be declared in a block statement, a \
       parser state, an action body, a control block's apply sub-block, the \
       list of local declarations in a parser or a control\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let typ_target = eval_type_with_check cursor ctx typ in
  check_valid_var_type typ_target.it;
  let expr_init_il =
    Option.map
      (fun expr_init ->
        let expr_init_il = type_expr cursor ctx expr_init in
        let expr_init_il = coerce_type_assign expr_init_il typ_target.it in
        expr_init_il)
      expr_init
  in
  let ctx =
    Ctx.add_rtype cursor id.it typ_target.it Lang.Ast.InOut Ctk.DYN ctx
  in
  let decl_il =
    Il.Ast.VarD { id; typ = typ_target; init = expr_init_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (8.21) Constructor invocations *)

(* (10.3.1) Instantiating objects with abstract methods

   When instantiating an extern type that has abstract methods users have to supply
   implementations for all such methods. This is done using object initializers.

   The abstract methods can only use the supplied arguments or refer to values that are
   in the top-level scope. When calling another method of the same instance the this
   keyword is used to indicate the current object instance. *)

(* (TODO) Consider the "this" keyword *)

and type_instantiation_init_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fdenv_abstract : Envs.FDEnv.t) (init : El.Ast.decl) :
    Envs.FDEnv.t * Il.Ast.decl =
  let fdenv, init_il =
    type_instantiation_init_decl' cursor ctx fdenv_abstract init.it
  in
  (fdenv, init_il $ init.at)

and type_instantiation_init_decl' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fdenv_abstract : Envs.FDEnv.t) (init : El.Ast.decl') :
    Envs.FDEnv.t * Il.Ast.decl' =
  match init with
  | FuncD { id; typ_ret; tparams; params; body } ->
      let fid = FId.to_fid id params in
      let ctx, decl_il =
        type_function_decl cursor ctx id tparams params typ_ret body
      in
      let fd = Ctx.find_funcdef_overloaded cursor fid ctx in
      let fdenv_abstract = Envs.FDEnv.add fid fd fdenv_abstract in
      (fdenv_abstract, decl_il)
  | _ ->
      Format.printf
        "(type_instantiation_init_decl) Instantiation initializer should be a \
         function declaration\n";
      assert false

and type_instantiation_init_decls (ctx : Ctx.t) (inits : El.Ast.decl list) :
    Envs.FDEnv.t * Il.Ast.decl list =
  List.fold_left
    (fun (fdenv_abstract, inits_il) init ->
      let fdenv_abstract, init_il =
        type_instantiation_init_decl Ctx.Global ctx fdenv_abstract init
      in
      (fdenv_abstract, inits_il @ [ init_il ]))
    (Envs.FDEnv.empty, []) inits

and type_instantiation_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    (init : El.Ast.decl list) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl'
    =
  let annos_il = List.map (type_anno cursor ctx) annos in
  let typ, _, expr_il = type_instantiation cursor ctx var_inst targs args in
  let targs_il, args_il =
    match expr_il with
    | Il.Ast.InstE { targs = targs_il; args = args_il; _ } -> (targs_il, args_il)
    | _ -> assert false
  in
  (* Typecheck abstract methods defined by object initializers (for externs only) *)
  let typ, init_il =
    match typ with
    | ExternT (id, fdenv_extern) ->
        let ctx =
          { Ctx.empty with global = ctx.global }
          |> Ctx.add_rtype Ctx.Local "this" typ Lang.Ast.No Ctk.CTK
        in
        let fdenv_abstract, init_il = type_instantiation_init_decls ctx init in
        let fdenv_extern =
          Envs.FDEnv.fold
            (fun fid fd fdenv_extern ->
              let typ_ret = FuncDef.get_typ_ret fd in
              let fd_extern =
                Envs.FDEnv.find_overloaded_opt fid fdenv_abstract
              in
              (* (TODO) What if extern abstract method decl has type parameters? *)
              if
                match (fd_extern : FuncDef.t option) with
                | Some
                    (ExternAbstractMethodD
                      (_tparams_extern, _params_extern, typ_ret_extern)) ->
                    typ_ret = typ_ret_extern
                | _ -> false
              then (
                Format.printf
                  "(type_instantiation_decl) Abstract method %a is not declared\n"
                  FId.pp fid;
                assert false);
              Envs.FDEnv.add fid fd fdenv_extern)
            fdenv_abstract fdenv_extern
        in
        Envs.FDEnv.iter
          (fun fid fd_extern ->
            match (fd_extern : FuncDef.t) with
            | ExternAbstractMethodD _ ->
                Format.printf
                  "(type_instantiation_decl) Abstract method %a is not defined\n"
                  FId.pp fid;
                assert false
            | _ -> ())
          fdenv_extern;
        (Types.ExternT (id, fdenv_extern), init_il)
    | _ when init <> [] ->
        Format.printf
          "(type_instantiation_decl) Initializers are only allowed for extern \
           objects\n";
        assert false
    | _ -> (typ, [])
  in
  let ctx = Ctx.add_rtype cursor id.it typ Lang.Ast.No Ctk.CTK ctx in
  let decl_il =
    Il.Ast.InstD
      {
        id;
        var_inst;
        targs = targs_il;
        args = args_il;
        init = init_il;
        annos = annos_il;
      }
  in
  (ctx, decl_il)

(* (7.1.2) The error type

   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)

and type_error_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : El.Ast.member list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_error_decl) Error declarations must be global\n";
    assert false);
  let type_error_decl' (ctx : Ctx.t) (member : El.Ast.member) : Ctx.t =
    let id = "error." ^ member.it in
    if Ctx.find_value_opt cursor id ctx |> Option.is_some then (
      Format.printf "(type_error_decl_glob) Error %s was already defined\n" id;
      assert false);
    let value = Value.ErrV member.it in
    let typ = Types.ErrT in
    Ctx.add_value cursor id value ctx
    |> Ctx.add_rtype cursor id typ Lang.Ast.No Ctk.LCTK
  in
  let ctx = List.fold_left type_error_decl' ctx members in
  ctx

(* (7.1.3) The match kind type

   The match_kind type is very similar to the error type and is used to declare a set of distinct names
   that may be used in a table's key property (described in Section 14.2.1).
   All identifiers are inserted into the top-level namespace.
   It is an error to declare the same match_kind identifier multiple times.

   (TODO) Can the type system enforce the following constraint?

   The declaration of new match_kinds can only occur within model description files;
   P4 programmers cannot declare new match kinds. *)

and type_match_kind_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : El.Ast.member list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_match_kind_decl) Match kind declarations must be global\n";
    assert false);
  let type_match_kind_decl' (ctx : Ctx.t) (member : El.Ast.member) : Ctx.t =
    let id = member.it in
    if Ctx.find_value_opt cursor id ctx |> Option.is_some then (
      Format.printf "(type_match_kind_decl) Match kind %s was already defined\n"
        id;
      assert false);
    let value = Value.MatchKindV member.it in
    let typ = Types.MatchKindT in
    Ctx.add_value cursor id value ctx
    |> Ctx.add_rtype cursor id typ Lang.Ast.No Ctk.LCTK
  in
  let ctx = List.fold_left type_match_kind_decl' ctx members in
  ctx

(* (7.2.5) Struct types

   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)

and type_struct_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list)
    (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_struct_decl) Struct declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  let members, typs, annoss =
    List.fold_left
      (fun (members, typs, annoss) (member, typ, annos) ->
        (members @ [ member ], typs @ [ typ ], annoss @ [ annos ]))
      ([], [], []) fields
  in
  let _annoss_il = List.map (List.map (type_anno cursor ctx)) annoss in
  let typs = List.map (eval_type_with_check cursor ctx) typs in
  let td =
    let members = List.map it members in
    let typs = List.map it typs in
    Types.StructD (id.it, List.combine members typs)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (7.2.2) Header types *)

and type_header_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list)
    (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_header_decl) Header declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  let members, typs, annoss =
    List.fold_left
      (fun (members, typs, annoss) (member, typ, annos) ->
        (members @ [ member ], typs @ [ typ ], annoss @ [ annos ]))
      ([], [], []) fields
  in
  let _annoss_il = List.map (List.map (type_anno cursor ctx)) annoss in
  let typs = List.map (eval_type_with_check cursor ctx) typs in
  let td =
    let members = List.map it members in
    let typs = List.map it typs in
    Types.HeaderD (id.it, List.combine members typs)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (7.2.4) Header unions *)

and type_union_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list)
    (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_union_decl) Union declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  let members, typs, annoss =
    List.fold_left
      (fun (members, typs, annoss) (member, typ, annos) ->
        (members @ [ member ], typs @ [ typ ], annoss @ [ annos ]))
      ([], [], []) fields
  in
  let _annoss_il = List.map (List.map (type_anno cursor ctx)) annoss in
  let typs = List.map (eval_type_with_check cursor ctx) typs in
  let td =
    let members = List.map it members in
    let typs = List.map it typs in
    Types.UnionD (id.it, List.combine members typs)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (7.2.1) Enumeration types

   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)

and type_enum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (members : El.Ast.member list) (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_enum_decl) Enum declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  let type_enum_decl' (ctx : Ctx.t) (member : El.Ast.member) : Ctx.t =
    let id_field = id.it ^ "." ^ member.it in
    if Ctx.find_value_opt cursor id_field ctx |> Option.is_some then (
      Format.printf "(type_enum_decl_glob) Enum %s was already defined\n"
        id_field;
      assert false);
    let value = Value.EnumFieldV (id.it, member.it) in
    let typ = Types.EnumT id.it in
    Ctx.add_value cursor id_field value ctx
    |> Ctx.add_rtype cursor id_field typ Lang.Ast.No Ctk.LCTK
  in
  let ctx = List.fold_left type_enum_decl' ctx members in
  let td =
    let members = List.map it members in
    Types.EnumD (id.it, members)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (7.2.1) Enumeration types

   It is also possible to specify an enum with an underlying representation.
   These are sometimes called serializable enums, because headers are allowed to have fields with such enum types.
   This requires the programmer provide both the fixed-width unsigned (or signed) integer type and an associated integer value
   for each symbolic entry in the enumeration. The symbol typeRef in the grammar above must be one of the following types:

    - an unsigned integer, i.e. bit<W> for some compile-time known W.
    - a signed integer, i.e. int<W> for some compile-time known W.
    - a type name declared via typedef, where the base type of that type is either one of the types listed above,
      or another typedef name that meets these conditions.

   Compiler implementations are expected to raise an error if the fixed-width integer representation for an enumeration entry
   falls outside the representation range of the underlying type. *)

and type_senum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (fields : (El.Ast.member * El.Ast.expr) list)
    (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_senum_decl) Serializable enum declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  let typ = eval_type_with_check cursor ctx typ in
  let type_senum_decl' (ctx : Ctx.t) (member, value) : Ctx.t =
    let id_field = id.it ^ "." ^ member.it in
    if Ctx.find_value_opt cursor id_field ctx |> Option.is_some then (
      Format.printf
        "(type_senum_decl_glob) Serializable enum %s was already defined\n"
        id_field;
      assert false);
    let value = Value.SEnumFieldV (id.it, member.it, value.it) in
    let typ = Types.SEnumT (id.it, typ.it) in
    Ctx.add_value cursor id_field value ctx
    |> Ctx.add_rtype cursor id_field typ Lang.Ast.No Ctk.LCTK
  in
  let members, exprs_field = List.split fields in
  let values_field =
    List.map
      (fun expr_field ->
        let expr_field_il = type_expr cursor ctx expr_field in
        let expr_field_il = coerce_type_assign expr_field_il typ.it in
        Static.eval_expr cursor ctx expr_field_il)
      exprs_field
  in
  let ctx =
    List.fold_left type_senum_decl' ctx (List.combine members values_field)
  in
  let td =
    let members = List.map it members in
    let values_field = List.map it values_field in
    Types.SEnumD (id.it, typ.it, List.combine members values_field)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (7.6) Introducing new types

   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword are restricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)

and type_newtype_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typdef : (El.Ast.typ, El.Ast.decl) El.Ast.alt) (annos : El.Ast.anno list) :
    Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_newtype_decl) New type declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  match typdef with
  | Left typ ->
      let typ = eval_type_with_check cursor ctx typ in
      let td = Types.NewD (id.it, typ.it) in
      WF.check_valid_typedef cursor ctx td;
      let ctx = Ctx.add_typedef cursor id.it td ctx in
      ctx
  | Right _ -> failwith "(TODO: type_newtype_decl) Handle newtype with decl"

(* (7.5) typedef

   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)

and type_typedef_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typdef : (El.Ast.typ, El.Ast.decl) El.Ast.alt) (annos : El.Ast.anno list) :
    Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf "(type_typedef_decl) Typedef declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  match typdef with
  | Left typ ->
      let typ = eval_type_with_check cursor ctx typ in
      let td = Types.DefD typ.it in
      WF.check_valid_typedef cursor ctx td;
      let ctx = Ctx.add_typedef cursor id.it td ctx in
      ctx
  | Right _ -> failwith "(TODO: type_typedef_decl) Handle typedef with decl"

(* (14.1) Actions

   Actions are code fragments that can read and write the data being processed.
   Actions may contain data values taht can be written by the control plane and read by the data plane.

   Syntactically actions resemble functions with no return value.
   Actions may be declared within a control block: in this case they can only be used within
   instances of that control block.

   Action parameters may not have extern types. Action parameters that have no direction
   (e.g., port in the previous example) indicate "action data." All such parameters must appear
   at the end of the parameter list. When used in a match-action table (see Section 14.2.1.2), these
   parameters will be provided by the table entries (e.g., as specified by the control plane, the
   default_action table property, or the entries table property).

   The body of an action consists of a sequence of statements and declarations. No table, control, or parser
   applications can appear within actions.

   Some targets may impose additional restrictions on action bodies-e.g., only allowing straight-line
   code, with no conditional statements or expressions. *)

and type_action_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (params : El.Ast.param list) (body : El.Ast.block)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if
    (not (cursor = Ctx.Global))
    && not (cursor = Ctx.Block && ctx.block.kind = Ctx.Control)
  then (
    Format.printf
      "(type_action_decl) Action declarations must be global or in a control \
       block\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let fid = FId.to_fid id params in
  (* Construct action layer context *)
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.set_localkind Ctx.Action ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il = List.map (type_param Ctx.Local ctx') params in
  let ctx' = Ctx.add_params Ctx.Local params_il ctx' in
  (* Typecheck body *)
  let _ctx', _flow, block_il = type_block Ctx.Local ctx' Cont body in
  (* Create an action definition *)
  let fd =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ActionD params
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef cursor fid fd ctx in
  let decl_il =
    Il.Ast.ActionD { id; params = params_il; body = block_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (10) Function declarations

   Functions can only be declared at the top level and all parameters must have a direction.
   P4 functions are modeled after functions as found in most other programming languages,
   but the language does not permit recursive functions.

   A function returns a value using the return statement.
   A function with a return type of void can simply use the return statement with no arguments.
   A function with a non-void return type must return a value of the suitable type
   on all possible execution paths. *)

(* (TODO) Aren't functions also allowed in object initializers? *)

and type_function_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (body : El.Ast.block) : Ctx.t * Il.Ast.decl' =
  if cursor <> Ctx.Global then (
    Format.printf "(type_function_decl) Function declarations must be global\n";
    assert false);
  let fid = FId.to_fid id params in
  (* Construct function layer context *)
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let typ_ret = eval_type_with_check Ctx.Local ctx' typ_ret in
  let ctx' = Ctx.set_localkind (Ctx.Function typ_ret.it) ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il = List.map (type_param Ctx.Local ctx') params in
  let ctx' = Ctx.add_params Ctx.Local params_il ctx' in
  (* Typecheck body *)
  let _ctx', flow, block_il = type_block Ctx.Local ctx' Cont body in
  if flow <> Flow.Ret && typ_ret.it <> Types.VoidT then (
    Format.printf
      "(type_function_decl) A function must return a value on all possible \
       execution paths\n";
    assert false);
  (* Create a function definition *)
  let fd =
    let tparams = List.map it tparams in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.FunctionD (tparams, params, typ_ret.it)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef cursor fid fd ctx in
  let decl_il =
    Il.Ast.FuncD { id; typ_ret; tparams; params = params_il; body = block_il }
  in
  (ctx, decl_il)

(* (7.2.10.1) Extern functions

   An extern function declaration describes the name and type signature
   of the function, but not its implementation. *)

and type_extern_function_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_extern_function_decl) Extern function declarations must be global\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let fid = FId.to_fid id params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let ctx' = Ctx.set_localkind Ctx.ExternFunction ctx' in
  let params_il = List.map (type_param Ctx.Local ctx') params in
  let typ_ret = eval_type_with_check Ctx.Local ctx' typ_ret in
  let fd =
    let tparams = List.map it tparams in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ExternFunctionD (tparams, params, typ_ret.it)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef cursor fid fd ctx in
  let decl_il =
    Il.Ast.ExternFuncD
      { id; typ_ret; tparams; params = params_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.2.10.2) Extern objects

   An extern object declaration declares an object and all methods that can be invoked to
   perform computations and to alter the state of the object.
   Extern object declarations can also optionally declare constructor methods;
   these must have the same name as the enclosing extern type, no type parameters, and no return type.
   Extern declarations may only appear as allowed by the architecture model and may be specific to a target. *)

and type_extern_constructor_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (cparams : El.Ast.cparam list) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.printf
      "(type_extern_constructor_decl) Extern constructor declarations must be \
       in an extern block\n";
    assert false);
  if id.it <> ctx.block.id then (
    Format.printf
      "(type_extern_constructor_decl) Extern constructor must have the same \
       name as the object\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let tparams = Ctx.get_tparams cursor ctx in
  let cid = FId.to_fid id cparams in
  let cparams_il = List.map (type_cparam cursor ctx) cparams in
  let td = Ctx.find_typedef Ctx.Global id.it ctx in
  let typ_args = List.map (fun tparam -> Types.VarT tparam) tparams in
  let typ = specialize_typedef td typ_args in
  let cd =
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    (tparams, cparams, typ)
  in
  WF.check_valid_consdef cursor ctx cd;
  let ctx = Ctx.add_consdef cid cd ctx in
  let decl_il =
    Il.Ast.ExternConstructorD { id; cparams = cparams_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.2.10.2) Extern objects - Abstract methods

   However, some types of extern objects may provide methods that can be implemented by the P4 programmers.
   Such methods are described with the abstract keyword prior to the method definition.
   When such an object is instantiated the user has to supply an implementation of all the abstract methods. *)

and type_extern_abstract_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.printf
      "(type_extern_abstract_method_decl) Extern method declarations must be \
       in an extern block\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let fid = FId.to_fid id params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let typ_ret = eval_type_with_check Ctx.Local ctx' typ_ret in
  let ctx' = Ctx.set_localkind (Ctx.ExternAbstractMethod typ_ret.it) ctx' in
  let params_il = List.map (type_param Ctx.Local ctx') params in
  let fd =
    let tparams = List.map it tparams in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ExternAbstractMethodD (tparams, params, typ_ret.it)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef cursor fid fd ctx in
  let decl_il =
    Il.Ast.ExternAbstractMethodD
      { id; tparams; params = params_il; typ_ret; annos = annos_il }
  in
  (ctx, decl_il)

and type_extern_method_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern) then (
    Format.printf
      "(type_extern_method_decl) Extern method declarations must be in an \
       extern block\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let fid = FId.to_fid id params in
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx' in
  let ctx' = Ctx.set_localkind Ctx.ExternMethod ctx' in
  let params_il = List.map (type_param Ctx.Local ctx') params in
  let typ_ret = eval_type_with_check Ctx.Local ctx' typ_ret in
  let fd =
    let tparams = List.map it tparams in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ExternMethodD (tparams, params, typ_ret.it)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef cursor fid fd ctx in
  let decl_il =
    Il.Ast.ExternMethodD
      { id; tparams; params = params_il; typ_ret; annos = annos_il }
  in
  (ctx, decl_il)

and type_extern_object_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (mthds : El.Ast.decl list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_extern_object_decl) Extern object declarations must be global\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  (* Separate constructors and methods *)
  let cons, mthds =
    List.partition
      (fun (mthd : El.Ast.decl) ->
        match mthd.it with ExternConstructorD _ -> true | _ -> false)
      mthds
  in
  (* Typecheck methods and abstract methods
     to construct function definition environment *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Extern ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let ctx', mthds_il = type_decls Ctx.Block ctx' mthds in
  (* Create an extern object type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    Types.ExternD (id.it, tparams, ctx'.block.fdenv)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  (* Typecheck constructors
     to update constructor definition environment
     This comes after method typing to prevent recursive instantiation *)
  let ctx'' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx'' = Ctx.set_blockkind Ctx.Extern ctx'' in
  let ctx'' = Ctx.add_tparams Ctx.Block tparams ctx'' in
  let ctx'', cons_il = type_decls Ctx.Block ctx'' cons in
  (* Update the context with the constructor definition environment *)
  let ctx =
    { ctx with global = { ctx.global with cdenv = ctx''.global.cdenv } }
  in
  let decl_il =
    Il.Ast.ExternObjectD
      { id; tparams; mthds = mthds_il @ cons_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.2.12) Parser and control blocks types

   Parsers and control blocks types are similar to function types: they describe the signature of parsers and control blocks.
   Such functions have no return values. Declarations of parsers and control block types in architectures may be generic
   (i.e., have type parameters).

   (7.2.12.1) Parser type declarations

   A parser should have at least one argument of type packet_in, representing the received packet that is processed. *)

and type_parser_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_parser_type_decl) Parser type declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  (* Typecheck implicit "apply" method
     to construct function definition environment *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let params_il = List.map (type_param Ctx.Block ctx') params in
  (* Create a parser type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ParserD (tparams, params)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (13.2) Parser declarations

   A parser declaration comprises a name, a list of parameters, an optional list of constructor parameters,
   local elements, and parser states (as well as optional annotations).
   Unlike parser type declarations, parser declarations may not be generic.

   At least one state, named start, must be present in any parser. A parser may not define
   two states with the same name. It is also illegal for a parser to give explicit definitions
   for the accept and reject states—those states are logically distinct from the states defined by the programmer.

   State declarations are described below. Preceding the parser states, a parser may also contain
   a list of local elements. These can be constants, variables, or instantiations of objects that
   may be used within the parser. Such objects may be instantiations of extern objects, or other parsers
   that may be invoked as subroutines. However, it is illegal to instantiate a control block within a parser.

   The states and local elements are all in the same namespace. *)

and type_parser_state (cursor : Ctx.cursor) (ctx : Ctx.t)
    (state : El.Ast.parser_state) : Ctx.t * Il.Ast.parser_state =
  let label, block, annos = state.it in
  let ctx, state_il = type_parser_state' cursor ctx label block annos in
  (ctx, state_il $ state.at)

and type_parser_state' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (label : El.Ast.state_label) (block : El.Ast.block)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.parser_state' =
  if
    not
      (cursor = Ctx.Local
      && ctx.block.kind = Ctx.Parser
      && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
  then (
    Format.printf "(type_parser_state) Parser state must be local\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let ctx, _flow, block_il = type_block Ctx.Local ctx Cont block in
  let state_il = (label, block_il, annos_il) in
  (ctx, state_il)

and type_parser_states (_cursor : Ctx.cursor) (ctx : Ctx.t)
    (states : El.Ast.parser_state list) : Ctx.t * Il.Ast.parser_state list =
  let labels = List.map it states |> List.map (fun (label, _, _) -> label.it) in
  if not (List.mem "start" labels) then (
    Format.printf "(type_parser_states) A \"start\" state must exist\n";
    assert false);
  if List.mem "accept" labels || List.mem "reject" labels then (
    Format.printf
      "(type_parser_states) \"accpet\" and \"reject\" states are reserved\n";
    assert false);
  let labels = "accept" :: "reject" :: labels in
  WF.check_distinct_names labels;
  let ctx' = Ctx.set_localkind Ctx.ParserState ctx in
  let ctx' =
    List.fold_left
      (fun ctx' label ->
        Ctx.add_rtype Ctx.Local label Types.StateT Lang.Ast.No Ctk.DYN ctx')
      ctx' labels
  in
  let _ctx', states_il =
    List.fold_left
      (fun (ctx', states) state ->
        let ctx' =
          let label, _, _ = state.it in
          Ctx.set_id Ctx.Local label.it ctx'
        in
        let ctx', state_il = type_parser_state Ctx.Local ctx' state in
        (ctx', states @ [ state_il ]))
      (ctx', []) states
  in
  let decl_il = states_il in
  (ctx, decl_il)

and type_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (cparams : El.Ast.cparam list) (locals : El.Ast.decl list)
    (states : El.Ast.parser_state list) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  if cursor <> Ctx.Global then (
    Format.printf "(type_parser_decl) Parser declarations must be global\n";
    assert false);
  if tparams <> [] then (
    Format.printf "(type_parser_decl) Parser declarations cannot be generic\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let cid = FId.to_fid id cparams in
  (* Typecheck and add constructor parameters to the block context *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx' in
  let cparams_il = List.map (type_cparam Ctx.Block ctx') cparams in
  let ctx' = Ctx.add_cparams Ctx.Block cparams_il ctx' in
  (* Typecheck and add apply parameters to the block context *)
  let params_il = List.map (type_param Ctx.Block ctx') params in
  let ctx' = Ctx.add_params Ctx.Block params_il ctx' in
  (* Typecheck and add local declarations to the block context *)
  let ctx', locals_il = type_decls Ctx.Block ctx' locals in
  (* Typecheck parser states *)
  let _ctx', states_il = type_parser_states Ctx.Block ctx' states in
  (* Create a parser constructor definition *)
  let typ =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ParserT params
  in
  let cd =
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    ([], cparams, typ)
  in
  WF.check_valid_consdef Ctx.Block ctx cd;
  let ctx = Ctx.add_consdef cid cd ctx in
  let decl_il =
    Il.Ast.ParserD
      {
        id;
        tparams;
        cparams = cparams_il;
        params = params_il;
        locals = locals_il;
        states = states_il;
        annos = annos_il;
      }
  in
  (ctx, decl_il)

(* (14.2) Tables

   A table declaration introduces a table instance.
   To obtain multiple instances of a table, it must be declared within a control block
   that is itself instantiated multiple times.

   Syntactically a table is defined in terms of a set of key-value properties.
   Some of these properties are “standard” properties, but the set of properties can
   be extended by target-specific compilers as needed.
   Note duplicated properties are invalid and the compiler should reject them.

   In addition, the tables may optionally define the following properties,

    - default_action: an action to execute when the lookup in the lookup table
        fails to find a match for the key used.
    - size: an integer specifying the desired size of the table.
    - entries: entries that are initially added to a table when the P4 program is loaded,
        some or all of which may be unchangeable by the control plane software.
    - largest_priority_wins - Only useful for some tables with the entries property.
        See section 14.2.1.4 for details.
    - priority_delta - Only useful for some tables with the entries property.
        See section 14.2.1.4 for details.

   The compiler must set the default_action to NoAction (and also insert it into the list of actions)
   for tables that do not define the default_action property.  Hence, all tables can be thought of
   as having a default_action` property, either implicitly or explicitly.

   A property marked as const cannot be changed dynamically by the control plane.
   The key, actions, and size properties are always constant, so the const keyword is not needed for these.

   (14.2.2) Match-action unit invocation

   A table can be invoked by calling its apply method. Calling an apply method on a table instance
   returns a value with a struct type with three fields. This structure is synthesized by the compiler automatically.
   For each table T, the compiler synthesizes an enum and a struct, shown in pseudo-P4:

      enum action_list(T) {
         // one field for each action in the actions list of table T
      }
      struct apply_result(T) {
          bool hit;
          bool miss;
          action_list(T) action_run;
      } *)

and type_table_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (id : El.Ast.id) : Ctx.t =
  let id_enum = "action_list(" ^ id.it ^ ")" in
  let type_table_enum_decl' (ctx : Ctx.t) (member : Il.Ast.member') : Ctx.t =
    let id_field = id_enum ^ "." ^ member in
    if Ctx.find_value_opt cursor id_field ctx |> Option.is_some then (
      Format.printf "(type_enum_decl_glob) Enum %s was already defined\n"
        id_field;
      assert false);
    let value = Value.EnumFieldV (id_enum, member) in
    let typ = Types.EnumT id_enum in
    Ctx.add_value cursor id_field value ctx
    |> Ctx.add_rtype cursor id_field typ Lang.Ast.No Ctk.LCTK
  in
  let members =
    List.map
      (fun (var, _, _) -> Format.asprintf "%a" Il.Pp.pp_var' var)
      table_ctx.actions
  in
  let ctx = List.fold_left type_table_enum_decl' ctx members in
  let td_enum = Types.EnumD (id_enum, members) in
  let ctx = Ctx.add_typedef cursor id_enum td_enum ctx in
  let id_struct = "apply_result(" ^ id.it ^ ")" in
  let td_struct =
    let typ_enum = specialize_typedef td_enum [] in
    Types.StructD
      ( id_struct,
        [
          ("hit", Types.BoolT); ("miss", Types.BoolT); ("action_run", typ_enum);
        ] )
  in
  let ctx = Ctx.add_typedef cursor id_struct td_struct ctx in
  ctx

and type_table_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (table : El.Ast.table) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if not (cursor = Ctx.Block && ctx.block.kind = Ctx.Control) then (
    Format.printf
      "(type_table_decl) Table declarations must be in a control block\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let table_ctx = Tblctx.empty in
  let table_ctx, table_keys_il =
    type_table_keys cursor ctx table_ctx table.keys
  in
  let table_ctx, table_actions_il =
    type_table_actions cursor ctx table_ctx table.actions
  in
  let table_action_vars =
    List.map it table_actions_il |> List.map (fun (var, _, _) -> var)
  in
  WF.check_distinct_vars table_action_vars;
  let table_entries_il =
    List.map (type_table_entry cursor ctx table_ctx) table.entries
  in
  let table_default_il =
    type_table_default cursor ctx table_ctx table.default
  in
  let table_customs_il =
    List.map (type_table_custom cursor ctx) table.customs
  in
  (* (TODO) Should we add action_list(T), apply_result(T) type in env? And should we add decl to?
     Petr4 did, and also it puts id in TableT instead of typ *)
  let ctx = type_table_type_decl cursor ctx table_ctx id in
  let typ =
    let id_struct = "apply_result(" ^ id.it ^ ")" in
    let td_struct = Ctx.find_typedef cursor id_struct ctx in
    let typ_struct = specialize_typedef td_struct [] in
    Types.TableT typ_struct
  in
  let ctx = Ctx.add_rtype cursor id.it typ Lang.Ast.No Ctk.DYN ctx in
  let table_il =
    Lang.Ast.
      {
        keys = table_keys_il;
        actions = table_actions_il;
        entries = table_entries_il;
        default = table_default_il;
        customs = table_customs_il;
      }
  in
  let decl_il = Il.Ast.TableD { id; table = table_il; annos = annos_il } in
  (ctx, decl_il)

(* (14.2.1.1) Keys

   A key is a list of pairs of the form (e : m), where e is an expression that describes the data to be matched
   in the table, and m is a match_kind constant that describes
   the algorithm used to perform the lookup (see Section 7.1.3).

   If a table has no key property, or if the value of its key property is the empty tuple, i.e. key = {},
   then it contains no look-up table, just a default action—i.e., the associated lookup table is always the empty map.

   The expected meaning of these values is as follows:

    - an exact match kind on a key field means that the value of the field in the table specifies exactly the value
      the lookup key field must have in order to match.
      This is applicable for all legal key fields whose types support equality comparisons.
    - a ternary match kind on a key field means that the field in the table specifies a set of values
      for the key field using a value and a mask.
      The meaning of the (value, mask) pair is similar to the P4 mask expressions, as described in Section 8.15.3:
      a key field k matches the table entry when k & mask == value & mask.
    - a lpm (longest prefix match) match kind on a key field is a specific type of ternary match where
      the mask is required to have a form in binary that is a contiguous set of 1 bits followed by a contiguous set of 0 bits.
      Masks with more 1 bits have automatically higher priorities. A mask with all bits 0 is legal. *)

and check_table_key (match_kind : string) (typ : Type.t) : unit =
  if not (check_table_key' match_kind typ) then (
    Format.printf
      "(check_table_key) %a is not a valid table key type for match kind %s.\n"
      Type.pp typ match_kind;
    assert false)

and check_table_key' (match_kind : string) (typ : Type.t) : bool =
  match match_kind with
  | "exact" | "optional" -> (
      match typ with
      | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ | EnumT _ -> true
      | SEnumT (_id, typ_inner) -> check_table_key' match_kind typ_inner
      | NewT (_id, typs_inner) -> check_table_key' match_kind typs_inner
      (* Has eq op but not appropriate for table key *)
      | TupleT _ | ErrT | MatchKindT | StackT _ | StructT _ | HeaderT _
      | UnionT _ ->
          false
      (* No equality op *)
      | VoidT | StrT | VarT _ | ExternT _ | ParserT _ | ControlT _ | PackageT
      | TableT _ | TopT | SeqT _ | RecordT _ | SetT _ | StateT ->
          false)
  | "lpm" | "ternary" | "range" -> (
      match typ with
      | IntT | FIntT _ | FBitT _ -> true
      | SEnumT (_id, typ_inner) -> check_table_key' match_kind typ_inner
      | NewT (_id, typs_inner) -> check_table_key' match_kind typs_inner
      (* Has eq op but not appropriate for table key *)
      | BoolT | TupleT _ | EnumT _ | VBitT _ | ErrT | MatchKindT | StackT _
      | StructT _ | HeaderT _ | UnionT _ ->
          false
      (* No equality op *)
      | VoidT | StrT | VarT _ | ExternT _ | ParserT _ | ControlT _ | PackageT
      | TableT _ | TopT | SeqT _ | RecordT _ | SetT _ | StateT ->
          false)
  | _ ->
      Format.printf "(check_table_key) %s is not a valid match_kind\n"
        match_kind;
      assert false

and type_table_key (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_key : El.Ast.table_key) : Tblctx.t * Il.Ast.table_key =
  let table_ctx, table_key_il =
    type_table_key' cursor ctx table_ctx table_key.it
  in
  (table_ctx, table_key_il $ table_key.at)

and type_table_key' (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_key : El.Ast.table_key') : Tblctx.t * Il.Ast.table_key' =
  let expr, match_kind, annos = table_key in
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  let value_match_kind = Ctx.find_value_opt cursor match_kind.it ctx in
  let value_match_kind =
    match value_match_kind with
    | Some (Value.MatchKindV match_kind) -> match_kind
    | _ ->
        Format.printf "(type_table_key) %a is not a valid match_kind\n"
          El.Pp.pp_match_kind match_kind;
        assert false
  in
  check_table_key value_match_kind typ;
  let annos_il = List.map (type_anno cursor ctx) annos in
  let table_key_il = (expr_il, match_kind, annos_il) in
  let table_ctx = Tblctx.add_key (Types.SetT typ, value_match_kind) table_ctx in
  (table_ctx, table_key_il)

and type_table_keys (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_keys : El.Ast.table_key list) : Tblctx.t * Il.Ast.table_key list =
  List.fold_left
    (fun (table_ctx, table_keys_il) table_key ->
      let table_ctx, table_key_il =
        type_table_key cursor ctx table_ctx table_key
      in
      (table_ctx, table_keys_il @ [ table_key_il ]))
    (table_ctx, []) table_keys

(* (14.2.1.2) Actions

   Each action in the list of actions for a table must have a distinct name.

   Each action parameter that has a direction (in, inout, or out) must be bound in the actions list specification;
   conversely, no directionless parameters may be bound in the list.
   The expressions supplied as arguments to an action are not evaluated until the action is invoked.
   Applying tables, whether directly via an expression like table1.apply().hit, or indirectly,
   are forbidden in the expressions supplied as action arguments. *)

(* (TODO) : check action in entry | default, do we need to use find_overloaded_opt?
   What about just search name from table_action_notes and get info *)

and type_call_action_partial (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : Types.param list) (args_il : (Il.Ast.arg * Type.t) list) :
    Il.Ast.arg list =
  (* (TODO) : check including table funcitons *)
  let params =
    List.filter_map
      (fun param ->
        let _, dir, _, _ = param in
        match dir with Lang.Ast.No -> None | _ -> Some param)
      params
  in
  type_call_convention cursor ctx params args_il

and type_table_action (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_action : El.Ast.table_action) : Tblctx.t * Il.Ast.table_action =
  let table_ctx, table_action_il =
    type_table_action' cursor ctx table_ctx table_action.it
  in
  (table_ctx, table_action_il $ table_action.at)

and type_table_action' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_action : El.Ast.table_action') :
    Tblctx.t * Il.Ast.table_action' =
  let var, args, annos = table_action in
  let fd = Ctx.find_opt Ctx.find_funcdef_opt cursor var ctx in
  if Option.is_none fd then (
    Format.printf "(type_table_action) There is no action named %a\n"
      El.Pp.pp_var var;
    assert false);
  let fd = Option.get fd in
  if not (match fd with Types.ActionD _ -> true | _ -> false) then (
    Format.printf "(type_table_action) %a is not an action\n" El.Pp.pp_var var;
    assert false);
  let params = FuncDef.get_params fd in
  let args_il_typed = List.map (type_arg cursor ctx) args in
  let args_il_before = List.map fst args_il_typed in
  let args_il = type_call_action_partial cursor ctx params args_il_typed in
  let annos_il = List.map (type_anno cursor ctx) annos in
  let table_action_il = (var, args_il, annos_il) in
  let table_ctx =
    Tblctx.add_action (var.it, params, args_il_before) table_ctx
  in
  (table_ctx, table_action_il)

and type_table_actions (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_actions : El.Ast.table_action list) :
    Tblctx.t * Il.Ast.table_action list =
  List.fold_left
    (fun (table_ctx, table_actions_il) table_action ->
      let table_ctx, table_action_il =
        type_table_action cursor ctx table_ctx table_action
      in
      (table_ctx, table_actions_il @ [ table_action_il ]))
    (table_ctx, []) table_actions

(* (14.2.1.3) Default action

   If present, the default_action property must appear after the action property.
   The default action must be one of the actions that appear in the actions list.
   In particular, the expressions passed as in, out, or inout parameters must be
   syntactically identical to the expressions used in one of the elements of the actions list.

   Note that the specified default action must supply arguments for the control-plane-bound parameters
   (i.e., the directionless parameters), since the action is synthesized at compilation time.
   The expressions supplied as arguments for parameters with a direction (in, inout, or out)
   are evaluated when the action is invoked while the expressions supplied as
   arguments for directionless parameters are evaluated at compile time. *)

and type_call_default_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : Types.param list) (args_il_typed : (Il.Ast.arg * Type.t) list)
    (args_action : Il.Ast.arg list) : Il.Ast.arg list =
  let args_il_dyn =
    List.map2
      (fun param arg_il_typed ->
        let arg_il, _ = arg_il_typed in
        let _, dir, _, _ = param in
        match dir with Lang.Ast.No -> None | _ -> Some arg_il)
      params args_il_typed
    |> List.filter_map (fun x -> x)
  in
  if not (Il.Eq.eq_args args_action args_il_dyn) then (
    Format.printf
      "(type_call_default_action_partial) Arguments %a and %a are \
       syntactically different\n"
      Il.Pp.pp_args args_action Il.Pp.pp_args args_il_dyn;
    assert false);
  type_call_convention cursor ctx params args_il_typed

and type_table_default_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tblctx : Tblctx.t) (table_action : El.Ast.table_action) :
    Il.Ast.table_action =
  type_table_default_action' cursor ctx tblctx table_action.it $ table_action.at

and type_table_default_action' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tblctx : Tblctx.t) (table_action : El.Ast.table_action') :
    Il.Ast.table_action' =
  let var, args, annos = table_action in
  let table_action = Tblctx.find_action tblctx var in
  if Option.is_none table_action then (
    Format.printf
      "(type_table_action_default) There is no action named %a in actions list\n"
      El.Pp.pp_var var;
    assert false);
  let params, args_action = Option.get table_action in
  let args_il_typed = List.map (type_arg cursor ctx) args in
  let args_il =
    type_call_default_action cursor ctx params args_il_typed args_action
  in
  let annos_il = List.map (type_anno cursor ctx) annos in
  (var, args_il, annos_il)

and type_table_default (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_default : El.Ast.table_default option) :
    Il.Ast.table_default option =
  match table_default with
  | Some table_default ->
      type_table_default' cursor ctx table_ctx table_default.it
      $ table_default.at |> Option.some
  | None -> None

and type_table_default' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_default : El.Ast.table_default') :
    Il.Ast.table_default' =
  let action, default_const = table_default in
  let action_il = type_table_default_action cursor ctx table_ctx action in
  (action_il, default_const)

(* (14.2.1.4) Entries

   Entries cannot be specified for a table with no key (see Sec. 14.2.1.1).

   The keysetExpression component of an entry is a tuple that must provide
   a field for each key in the table keys (see Sec. 14.2.1). The table key type must match
   the type of the element of the set. The actionRef component must be an action which appears
   in the table actions list (and must not have the @defaultonly annotation), with all its arguments bound. *)

and type_table_entry_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx_key : Type.t * Il.Ast.match_kind') (keyset : El.Ast.keyset) :
    Il.Ast.keyset =
  type_table_entry_keyset' cursor ctx table_ctx_key keyset.it $ keyset.at

and type_table_entry_keyset' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx_key : Type.t * Il.Ast.match_kind') (keyset : El.Ast.keyset') :
    Il.Ast.keyset' =
  let typ_key, match_kind = table_ctx_key in
  match keyset with
  | ExprK expr ->
      let expr_il =
        match expr.it with
        | MaskE _ ->
            if match_kind = "lpm" || match_kind = "ternary" then
              type_expr cursor ctx expr
            else (
              Format.printf
                "(type_action_keyset) match_kind %s cannot use mask expression \n"
                match_kind;
              assert false)
        | RangeE _ ->
            if match_kind = "range" then type_expr cursor ctx expr
            else (
              Format.printf
                "(type_action_keyset) match_kind %s cannot use range expression \n"
                match_kind;
              assert false)
        | _ ->
            let expr_il = type_expr Ctx.Local ctx expr in
            let typ = Types.SetT expr_il.note.typ in
            Il.Ast.(expr_il.it $$ expr_il.at % { typ; ctk = expr_il.note.ctk })
      in
      let expr_il =
        let typ =
          match expr_il.note.typ with
          | Types.SetT typ -> typ
          | _ -> assert false
        in
        let typ_key =
          match typ_key with Types.SetT typ -> typ | _ -> assert false
        in
        let expr_il =
          Il.Ast.(expr_il.it $$ expr_il.at % { typ; ctk = expr_il.note.ctk })
        in
        let expr_il = coerce_type_assign expr_il typ_key in
        Il.Ast.(
          expr_il.it
          $$ expr_il.at % { typ = Types.SetT typ; ctk = expr_il.note.ctk })
      in
      Lang.Ast.ExprK expr_il
  | DefaultK ->
      if match_kind = "exact" then (
        Format.printf
          "(type_action_keyset) exact match does not allow default expression \n";
        assert false);
      Lang.Ast.DefaultK
  | AnyK ->
      if match_kind = "exact" then (
        Format.printf
          "(type_action_keyset) exact match does not allow wildcard expression \n";
        assert false);
      Lang.Ast.AnyK

and type_table_entry_keysets (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (keysets : El.Ast.keyset list) : Il.Ast.keyset list =
  match (table_ctx.keys, keysets) with
  | _, [ { it = DefaultK; at; note } ] -> [ Lang.Ast.DefaultK $$ at % note ]
  | _, [ { it = AnyK; at; note } ] -> [ Lang.Ast.AnyK $$ at % note ]
  | table_ctx_keys, keysets ->
      if List.length table_ctx_keys <> List.length keysets then (
        Format.printf
          "(type_action_keysets) Number of select keys must match the number \
           of cases\n";
        assert false);
      List.map2 (type_table_entry_keyset cursor ctx) table_ctx_keys keysets

and type_call_entry_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : Types.param list) (args_il_typed : (Il.Ast.arg * Type.t) list)
    (args_action : Il.Ast.arg list) : Il.Ast.arg list =
  type_call_default_action cursor ctx params args_il_typed args_action

and type_table_entry_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_action : El.Ast.table_action) :
    Il.Ast.table_action =
  type_table_entry_action' cursor ctx table_ctx table_action.it
  $ table_action.at

and type_table_entry_action' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_action : El.Ast.table_action') :
    Il.Ast.table_action' =
  let var, args, annos = table_action in
  let table_action = Tblctx.find_action table_ctx var in
  if Option.is_none table_action then (
    Format.printf
      "(type_table_action_entry) There is no action named %a in actions list\n"
      El.Pp.pp_var var;
    assert false);
  let params, args_action = Option.get table_action in
  let args_il_typed = List.map (type_arg cursor ctx) args in
  let args_il =
    type_call_entry_action cursor ctx params args_il_typed args_action
  in
  let annos_il = List.map (type_anno cursor ctx) annos in
  (var, args_il, annos_il)

and type_table_entry (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_entry : El.Ast.table_entry) : Il.Ast.table_entry =
  type_table_entry' cursor ctx table_ctx table_entry.it $ table_entry.at

and type_table_entry' (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_entry : El.Ast.table_entry') : Il.Ast.table_entry' =
  let keysets, action, annos = table_entry in
  let keysets_il = type_table_entry_keysets cursor ctx table_ctx keysets in
  let action_il = type_table_entry_action cursor ctx table_ctx action in
  let annos_il = List.map (type_anno cursor ctx) annos in
  (keysets_il, action_il, annos_il)

(* (14.2.1.5) Size

   The size is an optional property of a table. When present, its value must always be
   an integer compile-time known value. It is specified in units of number of table entries.

   (14.2.1.6) Additional properties

   A table declaration defines its essential control and data plane interfaces—i.e., keys and actions.
   However, the best way to implement a table may actually depend on the nature of the entries
   that will be installed at runtime (for example, tables could be dense or sparse, could be implemented as
   hash-tables, associative memories, tries, etc.)

   However, these architecture-specific properties may not change the semantics of table lookups,
   which always produce either a hit and an action or a miss—they can only change how those results are
   interpreted on the state of the data plane. This restriction is needed to ensure that it is possible to
   reason about the behavior of tables during compilation. *)

and type_table_custom (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_custom : El.Ast.table_custom) : Il.Ast.table_custom =
  type_table_custom' cursor ctx table_custom.it $ table_custom.at

and type_table_custom' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_custom : El.Ast.table_custom') : Il.Ast.table_custom' =
  let member, expr, custom_const, annos = table_custom in
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  let annos_il = List.map (type_anno cursor ctx) annos in
  (match member.it with
  | "size" ->
      if not (Type.is_numeric typ) then (
        Format.printf
          "(type_table_custom) size should be numeric type, not %a\n"
          Types.pp_typ typ;
        assert false)
  | "largest_priority_wins" ->
      if typ <> BoolT then (
        Format.printf
          "(type_table_custom) largest_priority_wins should be boolean type, \
           not %a\n"
          Types.pp_typ typ;
        assert false)
  | "priority_delta" ->
      if not (Type.is_numeric typ) then (
        Format.printf
          "(type_table_custom) priority_delta should be numeric type, not %a\n"
          Types.pp_typ typ;
        assert false)
  | _ ->
      Format.printf "(type_table_custom) Custom element %s is undefined\n"
        member.it;
      assert false);
  (member, expr_il, custom_const, annos_il)

(* (7.2.12.2) Control type declarations *)

and type_control_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (annos : El.Ast.anno list) : Ctx.t =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_control_type_decl) Control type declarations must be global\n";
    assert false);
  let _annos_il = List.map (type_anno cursor ctx) annos in
  (* Typecheck implicit "apply" method
     to construct function definition environment *)
  let ctx' = Ctx.set_id Ctx.Local id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Control ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let params_il = List.map (type_param Ctx.Block ctx') params in
  (* Create a control type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ControlD (tparams, params)
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  ctx

(* (14) Control blocks

   Syntactically, a control block is declared with a name, parameters, optional type parameters,
   and a sequence of declarations of constants, variables, actions, tables, and other instantiations.
   It is illegal to instantiate a parser within a control block.
   Unlike control type declarations, control declarations may not be generic.

   P4 does not support exceptional control-flow within a control block.
   The only statement which has a non-local effect on control flow is exit, which causes execution of
   the enclosing control block to immediately terminate. That is, there is no equivalent of the
   verify statement or the reject state from parsers.
   Hence, all error handling must be performed explicitly by the programmer. *)

and type_control_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (cparams : El.Ast.cparam list) (locals : El.Ast.decl list)
    (body : El.Ast.block) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if cursor <> Ctx.Global then (
    Format.printf "(type_control_decl) Control declarations must be global\n";
    assert false);
  if tparams <> [] then (
    Format.printf "(type_control_decl) Control declarations cannot be generic\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  let cid = FId.to_fid id cparams in
  (* Typecheck and add constructor parameters to the block context *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Control ctx' in
  let cparams_il = List.map (type_cparam Ctx.Block ctx') cparams in
  let ctx' = Ctx.add_cparams Ctx.Block cparams_il ctx' in
  (* Typecheck and add apply parameters to the block context *)
  let params_il = List.map (type_param Ctx.Block ctx') params in
  let ctx' = Ctx.add_params Ctx.Block params_il ctx' in
  (* Typecheck and add local declarations to the block context *)
  let ctx', locals_il = type_decls Ctx.Block ctx' locals in
  (* Typecheck implicit "apply" method *)
  let ctx' = Ctx.set_id Ctx.Local "apply" ctx' in
  let ctx' = Ctx.set_localkind Ctx.ControlApplyMethod ctx' in
  let _ctx', _flow, body_il = type_block Ctx.Local ctx' Cont body in
  (* Create a control constructor definition *)
  let typ =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    Types.ControlT params
  in
  let cd =
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    ([], cparams, typ)
  in
  WF.check_valid_consdef Ctx.Block ctx cd;
  let ctx = Ctx.add_consdef cid cd ctx in
  let decl_il =
    Il.Ast.ControlD
      {
        id;
        tparams;
        cparams = cparams_il;
        params = params_il;
        locals = locals_il;
        body = body_il;
        annos = annos_il;
      }
  in
  (ctx, decl_il)

(* (7.2.13) Package types

   All parameters of a package are evaluated at compilation time, and in consequence they must all be directionless
   (they cannot be in, out, or inout). Otherwise package types are very similar to parser type declarations. *)

and type_package_constructor_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (cparams : El.Ast.cparam list) : Ctx.t * Il.Ast.cparam list
    =
  if not (cursor = Ctx.Block && ctx.block.kind = Package) then (
    Format.printf
      "(type_package_constructor_decl) Package constructor declarations must \
       be in a package block\n";
    assert false);
  if id.it <> ctx.block.id then (
    Format.printf
      "(type_package_constructor_decl) Package constructor must have the same \
       name as the object\n";
    assert false);
  let tparams = Ctx.get_tparams cursor ctx in
  let cid = FId.to_fid id cparams in
  let cparams_il = List.map (type_cparam Ctx.Block ctx) cparams in
  let td = Ctx.find_typedef Ctx.Global id.it ctx in
  let typ_args = List.map (fun tparam -> Types.VarT tparam) tparams in
  let typ = specialize_typedef td typ_args in
  let cd =
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    (tparams, cparams, typ)
  in
  WF.check_valid_consdef Ctx.Block ctx cd;
  let ctx = Ctx.add_consdef cid cd ctx in
  (ctx, cparams_il)

and type_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (cparams : El.Ast.cparam list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  if cursor <> Ctx.Global then (
    Format.printf
      "(type_package_type_decl) Package type declarations must be global\n";
    assert false);
  let annos_il = List.map (type_anno cursor ctx) annos in
  (* Create a package type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    Types.PackageD tparams
  in
  WF.check_valid_typedef cursor ctx td;
  let ctx = Ctx.add_typedef cursor id.it td ctx in
  (* Package type declaration is implicitly a constructor declaration *)
  let ctx' = Ctx.set_id Ctx.Block id.it ctx in
  let ctx' = Ctx.set_blockkind Ctx.Package ctx' in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let ctx', cparams_il =
    type_package_constructor_decl Ctx.Block ctx' id cparams
  in
  (* Update the context with the constructor definition environment *)
  let ctx =
    { ctx with global = { ctx.global with cdenv = ctx'.global.cdenv } }
  in
  let decl_il =
    Il.Ast.PackageTypeD { id; tparams; cparams = cparams_il; annos = annos_il }
  in
  (ctx, decl_il)

(* Entry point : Program typing *)

let type_program (program : El.Ast.program) : Il.Ast.program =
  Ctx.refresh ();
  let _ctx, program = type_decls Ctx.Global Ctx.empty program in
  program
