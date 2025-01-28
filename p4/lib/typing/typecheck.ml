open Domain.Dom
module Ctk = Runtime_static.Ctk
module Num = Runtime_static.Vdomain.Num
module Value = Runtime_static.Vdomain.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module FuncType = Types.FuncType
module FuncDef = Types.FuncDef
module ConsType = Types.ConsType
module ConsDef = Types.ConsDef
module Numerics = Runtime_static.Numerics
module Envs = Runtime_static.Envs
module Frame = Envs.Frame
module TDEnv = Envs.TDEnv
module FDEnv = Envs.FDEnv
module CDEnv = Envs.CDEnv
module WF = Wellformed
module F = Format
open Util.Source
open Util.Error

let error_no_info = error_checker_no_info
let error_pass_info = error_checker_pass_info
let check = check_checker

(* Coercion rules *)

let insert_cast (expr_il : Il.Ast.expr) (typ : Type.t) : Il.Ast.expr =
  let ctk = Static.ctk_cast_expr (typ $ no_info) expr_il in
  let expr_il =
    Il.Ast.(
      CastE { typ = typ $ no_info; expr = expr_il } $$ expr_il.at % { typ; ctk })
  in
  expr_il

(* Coercion for unary *)

(* Precondition: checker should always result in false for serializable enum case *)
let rec coerce_type_unary_numeric (checker : Type.t -> bool)
    (expr_il : Il.Ast.expr) : Il.Ast.expr =
  let typ = expr_il.note.typ |> Type.canon in
  match typ with
  | _ when checker typ -> expr_il
  | SEnumT (_, typ_inner, _) ->
      let expr_il = insert_cast expr_il typ_inner in
      coerce_type_unary_numeric checker expr_il
  | _ ->
      F.asprintf "(coerce_type_unary) cannot coerce type %a" (Type.pp ~level:0)
        typ
      |> error_no_info

(* Coercion for binary *)

let rec coerce_types_binary (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr)
    : Il.Ast.expr * Il.Ast.expr =
  let typ_l = expr_l_il.note.typ in
  let typ_r = expr_r_il.note.typ in
  if Type.eq_alpha typ_l typ_r then (expr_l_il, expr_r_il)
  else if Subtyp.implicit typ_l typ_r then
    let expr_l_il = insert_cast expr_l_il typ_r in
    (expr_l_il, expr_r_il)
  else if Subtyp.implicit typ_r typ_l then
    let expr_r_il = insert_cast expr_r_il typ_l in
    (expr_l_il, expr_r_il)
  else
    F.asprintf "(coerce_types_binary) cannot coerce types %a and %a"
      (Type.pp ~level:0) expr_l_il.note.typ (Type.pp ~level:0)
      expr_r_il.note.typ
    |> error_no_info

(* Precondition: checker should alwaysult in false for serializable enum case *)
and coerce_types_binary_numeric (checker : Type.t -> Type.t -> bool)
    (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr) :
    Il.Ast.expr * Il.Ast.expr =
  let typ_l = expr_l_il.note.typ |> Type.canon in
  let typ_r = expr_r_il.note.typ |> Type.canon in
  match (typ_l, typ_r) with
  | _ when checker typ_l typ_r -> (expr_l_il, expr_r_il)
  | SEnumT (_, typ_l_inner, _), _ ->
      let expr_l_il = insert_cast expr_l_il typ_l_inner in
      coerce_types_binary_numeric checker expr_l_il expr_r_il
  | _, SEnumT (_, typ_r_inner, _) ->
      let expr_r_il = insert_cast expr_r_il typ_r_inner in
      coerce_types_binary_numeric checker expr_l_il expr_r_il
  | _ ->
      F.asprintf "(coerce_types_binary_numeric) cannot coerce types %a and %a"
        (Type.pp ~level:0) typ_l (Type.pp ~level:0) typ_r
      |> error_no_info

(* Coercion for assignment (including assignment by call and return) *)

and coerce_type_assign (expr_from_il : Il.Ast.expr) (typ_to : Type.t) :
    Il.Ast.expr =
  let typ_from = expr_from_il.note.typ in
  if Type.eq_alpha typ_from typ_to then expr_from_il
  else if Subtyp.implicit typ_from typ_to then insert_cast expr_from_il typ_to
  else
    F.asprintf "(coerce_type) cannot coerce type %a to %a" (Type.pp ~level:0)
      expr_from_il.note.typ (Type.pp ~level:0) typ_to
    |> error_no_info

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
      (* (TODO) Add occurs check? *)
      TIdMap.add tid (Some typ_arg) cstr
  | SpecT (tdp_param, typs_inner_param), SpecT (tdp_arg, typs_inner_arg) ->
      let typ_param_inner =
        TypeDef.specialize_poly tdp_param typs_inner_param
      in
      let typ_arg_inner = TypeDef.specialize_poly tdp_arg typs_inner_arg in
      let cstr_inner = gen_cstr cstr typ_param_inner typ_arg_inner in
      if Type.is_nominal typ_param_inner && Type.is_nominal typ_arg_inner then
        gen_cstrs cstr_inner typs_inner_param typs_inner_arg
      else cstr_inner
  | DefT typ_inner_param, _ -> gen_cstr cstr typ_inner_param typ_arg
  | _, DefT typ_inner_arg -> gen_cstr cstr typ_param typ_inner_arg
  | NewT (id_param, typ_inner_param), NewT (id_arg, typ_inner_arg)
    when id_param = id_arg ->
      gen_cstr cstr typ_inner_param typ_inner_arg
  | ListT typ_param, ListT typ_arg -> gen_cstr cstr typ_param typ_arg
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
          let cstr = cstr in
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
  | PackageT typs_param, PackageT typs_arg -> gen_cstrs cstr typs_param typs_arg
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
      let cstr = cstr in
      let typ_old = TIdMap.find key cstr_old in
      let typ_new = TIdMap.find key cstr_new in
      match (typ_old, typ_new) with
      | None, _ -> TIdMap.add key typ_new cstr
      | _, None -> TIdMap.add key typ_old cstr
      | Some typ_old, Some typ_new ->
          if Subtyp.implicit typ_old typ_new then
            TIdMap.add key (Some typ_new) cstr
          else if Subtyp.implicit typ_new typ_old then
            TIdMap.add key (Some typ_old) cstr
          else
            F.asprintf "(merge_cstr) type %a and %a do not match"
              (Type.pp ~level:0) typ_old (Type.pp ~level:0) typ_new
            |> error_no_info)
    keys TIdMap.empty

and gen_cstrs (cstr : cstr_t) (typ_params : Type.t list)
    (typ_args : Type.t list) : cstr_t =
  check
    (List.length typ_params = List.length typ_args)
    (F.asprintf "(gen_cstrs) list lengths do not match: %d and %d"
       (List.length typ_params) (List.length typ_args));
  let cstrs =
    List.map2
      (fun typ_param typ_arg -> gen_cstr cstr typ_param typ_arg)
      typ_params typ_args
  in
  List.fold_left merge_cstr cstr cstrs

let infer_targs (tids_fresh : TId.t list) (params : Types.param list)
    (args_il_typed : (Il.Ast.arg * Type.t) list) : Type.t TIdMap.t =
  let cstr = empty_cstr tids_fresh in
  let typ_params = List.map (fun (_, _, typ, _) -> typ) params in
  let typ_args = List.map snd args_il_typed in
  let cstr = gen_cstrs cstr typ_params typ_args in
  TIdMap.fold
    (fun tid typ_opt theta ->
      match typ_opt with
      | None | Some Types.AnyT ->
          F.asprintf "(infer_targs) type %s cannot be inferred" tid
          |> error_no_info
      | Some typ -> TIdMap.add tid typ theta)
    cstr TIdMap.empty

(* (6.7) L-values

   L-values are expressions that may appear on the left side of an assignment operation
   or as arguments corresponding to out and inout function parameters.
   An l-value represents a storage reference. The following expressions are legal l-values:

   - Identifiers of a base or derived type.
   - Structure, header, and header union field member access operations (using the dot notation).
   - References to elements within header stacks (see Section 8.18): indexing, and references to last and next.
   - Theult of a bit-slice operator [m:l]. *)

let rec check_lvalue (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_il : Il.Ast.expr)
    : unit =
  check
    (check_lvalue' cursor ctx expr_il)
    (F.asprintf "(check_lvalue) %a is not an l-value" (Il.Pp.pp_expr ~level:0)
       expr_il)

and check_lvalue' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_il : Il.Ast.expr) :
    bool =
  match expr_il.it with
  | VarE { var } -> (
      let stype = Ctx.find_opt Ctx.find_stype_opt cursor var ctx in
      match stype with
      | None -> false
      | Some (typ, dir, _, _) -> (
          match dir with In | No -> false | _ -> Type.is_assignable typ))
  | ArrAccE { expr_base; _ }
  | BitAccE { expr_base; _ }
  | ExprAccE { expr_base; _ } ->
      check_lvalue' cursor ctx expr_base
  | _ -> false

(* Type evaluation *)

module TIdMap = MakeTIdEnv (Type)

let rec eval_type (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ) :
    Il.Ast.typ * TId.t list =
  try
    let typ_il, tids_fresh = eval_type' cursor ctx [] typ.it in
    (typ_il $ typ.at, tids_fresh)
  with CheckErr _ as err -> error_pass_info typ.at err

and eval_type_with_check (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : El.Ast.typ)
    : Il.Ast.typ * TId.t list =
  try
    let typ, tids_fresh = eval_type cursor ctx typ in
    WF.check_valid_typ cursor ctx ~tids_fresh typ.it;
    (typ, tids_fresh)
  with CheckErr _ as err -> error_pass_info typ.at err

and eval_type' (cursor : Ctx.cursor) (ctx : Ctx.t) (tids_fresh : TId.t list)
    (typ : El.Ast.typ') : Il.Ast.typ' * TId.t list =
  let fresh_tid () = "__WILD_" ^ string_of_int (Ctx.fresh ()) in
  match typ with
  | VoidT -> (Types.VoidT, tids_fresh)
  | ErrT -> (Types.ErrT, tids_fresh)
  | MatchKindT -> (Types.MatchKindT, tids_fresh)
  | StrT -> (Types.StrT, tids_fresh)
  | BoolT -> (Types.BoolT, tids_fresh)
  | IntT -> (Types.IntT, tids_fresh)
  | FIntT expr_width ->
      let expr_width_il = type_expr cursor ctx expr_width in
      let value_width = Static.eval_expr cursor ctx expr_width_il in
      let width = value_width.it |> Value.get_num in
      (Types.FIntT width, tids_fresh)
  | FBitT expr_width ->
      let expr_width_il = type_expr cursor ctx expr_width in
      let value_width = Static.eval_expr cursor ctx expr_width_il in
      let width = value_width.it |> Value.get_num in
      (Types.FBitT width, tids_fresh)
  | VBitT expr_width ->
      let expr_width_il = type_expr cursor ctx expr_width in
      let value_width = Static.eval_expr cursor ctx expr_width_il in
      let width = value_width.it |> Value.get_num in
      (Types.VBitT width, tids_fresh)
  | StackT (typ_inner, expr_size) ->
      let typ_inner, tids_fresh_inner = eval_type cursor ctx typ_inner in
      let expr_size_il = type_expr cursor ctx expr_size in
      let value_size = Static.eval_expr cursor ctx expr_size_il in
      let size = value_size.it |> Value.get_num in
      let tdp =
        let typ_stack = Types.StackT (Types.VarT "T", size) in
        ([ "T" ], [], typ_stack)
      in
      let typ = Types.SpecT (tdp, [ typ_inner.it ]) in
      let tids_fresh = tids_fresh @ tids_fresh_inner in
      (typ, tids_fresh)
  | ListT typ_inner ->
      let typ_inner, tids_fresh_inner = eval_type cursor ctx typ_inner in
      let tdp =
        let typ_list = Types.ListT (Types.VarT "T") in
        ([ "T" ], [], typ_list)
      in
      let typ = Types.SpecT (tdp, [ typ_inner.it ]) in
      let tids_fresh = tids_fresh @ tids_fresh_inner in
      (typ, tids_fresh)
  | TupleT typs_inner ->
      let typs_inner, tids_fresh =
        eval_types cursor ctx tids_fresh typs_inner
      in
      let typs_inner = List.map it typs_inner in
      let tdp =
        let tparams =
          List.init (List.length typs_inner) (fun i -> "T" ^ string_of_int i)
        in
        let typs_inner = List.map (fun tparam -> Types.VarT tparam) tparams in
        let typ_tuple = Types.TupleT typs_inner in
        (tparams, [], typ_tuple)
      in
      let typ = Types.SpecT (tdp, typs_inner) in
      (typ, tids_fresh)
  | NameT var -> (
      let td = Ctx.find_opt Ctx.find_typdef_opt cursor var ctx in
      match td with
      | Some (MonoD tdm) -> (tdm, tids_fresh)
      | Some (PolyD tdp) ->
          let typ = Types.SpecT (tdp, []) in
          (typ, tids_fresh)
      | None ->
          F.asprintf "(eval_type') type definition %a does not exist"
            El.Pp.pp_var var
          |> error_no_info)
  | SpecT (var, typs) -> (
      let td = Ctx.find_opt Ctx.find_typdef_opt cursor var ctx in
      match td with
      | Some (MonoD tdm) ->
          if typs = [] then (tdm, tids_fresh)
          else
            F.asprintf
              "(eval_type') type definition %a is monomorphic but type \
               arguments are supplied"
              El.Pp.pp_var var
            |> error_no_info
      | Some (PolyD tdp) ->
          let typs, tids_fresh = eval_types cursor ctx tids_fresh typs in
          let typs = List.map it typs in
          let typ = Types.SpecT (tdp, typs) in
          (typ, tids_fresh)
      | None ->
          F.asprintf "(eval_type') type definition %a does not exist"
            El.Pp.pp_var var
          |> error_no_info)
  | AnyT ->
      let tid_fresh = fresh_tid () in
      let typ = Types.VarT tid_fresh in
      (typ, tids_fresh @ [ tid_fresh ])

and eval_types (cursor : Ctx.cursor) (ctx : Ctx.t) (tids_fresh : TId.t list)
    (typs : El.Ast.typ list) : Il.Ast.typ list * TId.t list =
  List.fold_left
    (fun (typs_il, tids_fresh) typ ->
      let typ_il, tids_fresh_new = eval_type cursor ctx typ in
      (typs_il @ [ typ_il ], tids_fresh @ tids_fresh_new))
    ([], tids_fresh) typs

and eval_types_with_check (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tids_fresh : TId.t list) (typs : El.Ast.typ list) :
    Il.Ast.typ list * TId.t list =
  List.fold_left
    (fun (typs_il, tids_fresh) typ ->
      let typ_il, tids_fresh_new = eval_type_with_check cursor ctx typ in
      (typs_il @ [ typ_il ], tids_fresh @ tids_fresh_new))
    ([], tids_fresh) typs

(* Annotation typing *)

and type_anno (cursor : Ctx.cursor) (ctx : Ctx.t) (anno : El.Ast.anno) :
    Il.Ast.anno =
  try
    let anno_il = type_anno' cursor ctx anno.it in
    anno_il $ anno.at
  with CheckErr _ as err -> error_pass_info anno.at err

and type_anno' (cursor : Ctx.cursor) (ctx : Ctx.t) (anno : El.Ast.anno') :
    Il.Ast.anno' =
  match anno with
  | EmptyN text -> Lang.Ast.EmptyN text
  | TextN (text, texts) -> Lang.Ast.TextN (text, texts)
  | ExprN (text, exprs) ->
      let exprs_il = type_exprs cursor ctx exprs in
      Lang.Ast.ExprN (text, exprs_il)
  | RecordN (text, fields) ->
      let members, exprs = List.split fields in
      let exprs_il = type_exprs cursor ctx exprs in
      Lang.Ast.RecordN (text, List.combine members exprs_il)

and type_annos (cursor : Ctx.cursor) (ctx : Ctx.t) (annos : El.Ast.anno list) :
    Il.Ast.anno list =
  List.map (type_anno cursor ctx) annos

(* Parameter typing *)

and type_param (cursor : Ctx.cursor) (ctx : Ctx.t) (param : El.Ast.param) :
    Il.Ast.param * TId.t list =
  try
    let param_il, tids_fresh = type_param' cursor ctx param.it in
    (param_il $ param.at, tids_fresh)
  with CheckErr _ as err -> error_pass_info param.at err

and type_param' (cursor : Ctx.cursor) (ctx : Ctx.t) (param : El.Ast.param') :
    Il.Ast.param' * TId.t list =
  let id, dir, typ, expr_default, annos = param in
  let typ, tids_fresh = eval_type_with_check cursor ctx typ in
  let value_default_il =
    match expr_default with
    | Some expr_default ->
        let expr_default_il = type_expr cursor ctx expr_default in
        let expr_default_il = coerce_type_assign expr_default_il typ.it in
        let value_default_il = Static.eval_expr cursor ctx expr_default_il in
        Some value_default_il
    | None -> None
  in
  let annos_il = type_annos cursor ctx annos in
  let param_il = (id, dir, typ, value_default_il, annos_il) in
  (param_il, tids_fresh)

and type_params (cursor : Ctx.cursor) (ctx : Ctx.t) (params : El.Ast.param list)
    : Il.Ast.param list * TId.t list =
  List.fold_left
    (fun (params_il, tids_fresh) param ->
      let param_il, tids_fresh_new = type_param cursor ctx param in
      (params_il @ [ param_il ], tids_fresh @ tids_fresh_new))
    ([], []) params

and type_cparam (cursor : Ctx.cursor) (ctx : Ctx.t) (cparam : El.Ast.cparam) :
    Il.Ast.cparam * TId.t list =
  type_param cursor ctx cparam

and type_cparams (cursor : Ctx.cursor) (ctx : Ctx.t)
    (cparams : El.Ast.cparam list) : Il.Ast.cparam list * TId.t list =
  type_params cursor ctx cparams

(* Calling convention *)

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
        See Section 14.1 for the meaning of action data, but its meaning includes the following possibilities:
        - The parameter's value is provided in the P4 program.
          In this case, the parameter behaves as if the direction were in.
          Such an argument expression need not be a compile-time known value.
        - The parameter's value is provided by the control plane software when an entry is added to
          a table that uses that action. See Section 14.1. *)

(* (6.8.1) Justification

   Following is a summary of the constraints imposed by the parameter directions:

    - All constructor parameters are evaluated at compilation-time,
      and in consequence they must all be directionless (they cannot be in, out, or inout);
      this applies to package, control, parser, and extern objects.
      Values for these parameters must be specified at compile-time, and must evaluate to compile-time known values.
      See Section 15 for further details.
    - Actions can also be explicitly invoked using function call syntax, either from a control block or from another action.
      In this case, values for all action parameters must be supplied explicitly, including values for the directionless parameters.
      The directionless parameters in this case behave like in parameters. See Section 14.1.1 for further details.
    - Default parameter values are only allowed for ‘in’ or direction-less parameters; these values must evaluate to compile-time constants. *)

and check_eq_typ_alpha (typ_l : Type.t) (typ_r : Type.t) : unit =
  check
    (Type.eq_alpha typ_l typ_r)
    (F.asprintf "(check_eq_typ_alpha) Types %a and %a are not equal"
       (Type.pp ~level:0) typ_l (Type.pp ~level:0) typ_r)

and check_table_apply_as_arg ~(action : bool) (args_il : Il.Ast.arg list) : unit
    =
  let found_table_apply = ref false in
  let walker =
    {
      Il.Walk.walker with
      walk_expr =
        (fun walker (expr : Il.Ast.expr) ->
          if !found_table_apply then ()
          else
            match (expr.note.typ : Type.t) with
            | TableStructT _ -> found_table_apply := true
            | _ -> Il.Walk.walk_expr walker expr);
    }
  in
  Lang.Walk.walk_list (Il.Walk.walk_arg walker) args_il;
  check
    (not (action && !found_table_apply))
    (F.asprintf
       "(check_table_apply_as_arg) Applying tables is forbidden in the \
        expressions supplied as action arguments")

and type_call_convention ~(action : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
    (params : Types.param list) (args_il_typed : (Il.Ast.arg * Type.t) list) :
    Il.Ast.arg list =
  assert (List.length params = List.length args_il_typed);
  check_table_apply_as_arg ~action (List.map fst args_il_typed);
  let rec type_call_convention'' args_il params args_il_typed =
    match (params, args_il_typed) with
    | [], [] -> args_il
    | param :: params, arg_il_typed :: args_il_typed ->
        let arg_il =
          type_call_convention' ~action cursor ctx param arg_il_typed
        in
        type_call_convention'' (args_il @ [ arg_il ]) params args_il_typed
    | [], _ | _, [] -> assert false
  in
  type_call_convention'' [] params args_il_typed

and type_call_convention' ~(action : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
    (param : Types.param) (arg_il_typed : Il.Ast.arg * Type.t) : Il.Ast.arg =
  let arg_il, typ_arg = arg_il_typed in
  let _, dir_param, typ_param, _ = param in
  let type_expr_arg (expr_il : Il.Ast.expr) =
    match dir_param with
    | Lang.Ast.In -> coerce_type_assign expr_il typ_param
    | Lang.Ast.Out | Lang.Ast.InOut ->
        check_eq_typ_alpha typ_arg typ_param;
        check_lvalue cursor ctx expr_il;
        expr_il
    | Lang.Ast.No when action -> coerce_type_assign expr_il typ_param
    | Lang.Ast.No ->
        check_eq_typ_alpha typ_arg typ_param;
        Static.check_ctk expr_il;
        expr_il
  in
  match (arg_il.it : Il.Ast.arg') with
  | ExprA expr_il ->
      let expr_il = type_expr_arg expr_il in
      let arg_il = Lang.Ast.ExprA expr_il $ arg_il.at in
      arg_il
  | NameA (id, Some expr_il) ->
      let expr_il = type_expr_arg expr_il in
      let arg_il = Lang.Ast.NameA (id, Some expr_il) $ arg_il.at in
      arg_il
  | NameA (_, None) | AnyA ->
      if dir_param = Lang.Ast.Out then arg_il
      else
        F.asprintf
          "(type_call_convention') don't care argument can only be used for an \
           out function/method argument"
        |> error_no_info

(* Expression typing *)

and type_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : El.Ast.expr) :
    Il.Ast.expr =
  try
    let typ, ctk, expr_il = type_expr' cursor ctx expr.it in
    Il.Ast.(expr_il $$ expr.at % { typ; ctk })
  with CheckErr _ as err -> error_pass_info expr.at err

and type_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : El.Ast.expr') :
    Type.t * Ctk.t * Il.Ast.expr' =
  match expr with
  | BoolE { boolean } -> type_bool_expr boolean
  | StrE { text } -> type_str_expr text
  | NumE { num } -> type_num_expr num
  | VarE { var } -> type_var_expr cursor ctx var
  | SeqE { exprs } -> type_seq_expr ~default:false cursor ctx exprs
  | SeqDefaultE { exprs } -> type_seq_expr ~default:true cursor ctx exprs
  | RecordE { fields } -> type_record_expr ~default:false cursor ctx fields
  | RecordDefaultE { fields } ->
      type_record_expr ~default:true cursor ctx fields
  | DefaultE -> type_default_expr ()
  | InvalidE -> type_invalid_expr ()
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
  | ErrAccE { member } -> type_error_acc_expr cursor ctx member
  | TypeAccE { var_base; member } ->
      type_type_acc_expr cursor ctx var_base member
  | ExprAccE { expr_base; member } ->
      type_expr_acc_expr cursor ctx expr_base member
  | CallFuncE { var_func; targs; args } ->
      type_call_func_expr cursor ctx var_func targs args
  | CallMethodE { expr_base; member; targs; args } ->
      type_call_method_expr cursor ctx expr_base member targs args
  | CallTypeE { var_typ; member; targs; args } ->
      type_call_type_expr cursor ctx var_typ member targs args
  | InstE { var_inst; targs; args } ->
      type_instantiation_expr cursor ctx var_inst targs args

and type_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : El.Ast.expr list) :
    Il.Ast.expr list =
  List.map (type_expr cursor ctx) exprs

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
        if signed then (Num.int_of_raw_int value width, Types.FIntT width)
        else (Num.bit_of_raw_int value width, Types.FBitT width)
    | value, None -> (Value.IntV value, Types.IntT)
  in
  let ctk = Ctk.LCTK in
  let expr_il = Il.Ast.ValueE { value = value $ num.at } in
  (typ, ctk, expr_il)

and type_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : El.Ast.var) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let stype = Ctx.find_opt Ctx.find_stype_opt cursor var ctx in
  check (Option.is_some stype)
    (F.asprintf "(type_var_expr) %a is a free identifier" El.Pp.pp_var var);
  let typ, _, _, _ = Option.get stype in
  let expr_il = Il.Ast.VarE { var } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.12) Operations on tuple expressions

   The empty tuple expression has type tuple<> - a tuple with no components. *)

(* (8.26) Initializing with default values

   A value of type struct, header, or tuple can also be initialized using a mix of
   explicit values and default values by using the notation ... in a tuple expression initializer;
   in this case all fields not explicitly initialized are initialized with default values.
   When initializing a struct, header, and tuple with a value containing partially default values
   using the ... notation the three dots must appear last in the initializer. *)

and type_seq_expr ~(default : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs : El.Ast.expr list) : Type.t * Ctk.t * Il.Ast.expr' =
  let exprs_il = type_exprs cursor ctx exprs in
  let typs =
    List.map note exprs_il |> List.map (fun Il.Ast.{ typ; _ } -> typ)
  in
  let typ, expr_il =
    if default then
      (Types.SeqDefaultT typs, Il.Ast.SeqDefaultE { exprs = exprs_il })
    else (Types.SeqT typs, Il.Ast.SeqE { exprs = exprs_il })
  in
  WF.check_valid_typ cursor ctx typ;
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

(* (8.26) Initializing with default values

   A value of type struct, header, or tuple can also be initialized using a mix of
   explicit values and default values by using the notation ... in a tuple expression initializer;
   in this case all fields not explicitly initialized are initialized with default values.
   When initializing a struct, header, and tuple with a value containing partially default values
   using the ... notation the three dots must appear last in the initializer. *)

and type_record_expr ~(default : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (El.Ast.member * El.Ast.expr) list) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let members, exprs = List.split fields in
  let exprs_il = type_exprs cursor ctx exprs in
  let typs =
    List.map note exprs_il |> List.map (fun Il.Ast.{ typ; _ } -> typ)
  in
  let typ, expr_il =
    let fields_typ = List.combine (List.map it members) typs in
    let fields = List.combine members exprs_il in
    if default then
      (Types.RecordDefaultT fields_typ, Il.Ast.RecordDefaultE { fields })
    else (Types.RecordT fields_typ, Il.Ast.RecordE { fields })
  in
  WF.check_valid_typ cursor ctx typ;
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.26) Initializing with default values

   A left-value can be initialized automatically with a default value of the
   suitable type using the syntax ... (see Section 7.3). *)

and type_default_expr () : Type.t * Ctk.t * Il.Ast.expr' =
  let value = Value.DefaultV in
  (Types.DefaultT, Ctk.LCTK, Il.Ast.ValueE { value = value $ no_info })

(* (8.17) Operations on headers
   (8.19) Operations on header unions

   The expression {#} represents an invalid header of some type,
   but it can be any header or header union type. A P4 compiler may require an
   explicit cast on this expression in cases where it cannot determine the
   particular header or header union type from the context. *)

and type_invalid_expr () : Type.t * Ctk.t * Il.Ast.expr' =
  let value = Value.InvalidV in
  (Types.InvalidT, Ctk.LCTK, Il.Ast.ValueE { value = value $ no_info })

(* (8.6) Operations on fixed-width bit types (unsigned integers)

   This section discusses all operations that can be performed on expressions of
   type bit<W> for some width W, also known as bit-strings.

   Each of the following operations produces a bit-stringult
   when applied to bit-strings of the same width:

    - Negation, denoted by unary -.
        Theult is computed by subtracting the value from 2W.
        Theult is unsigned and has the same width as the input.
        The semantics is the same as the C negation of unsigned numbers.
    - Unary plus, denoted by +. This operation behaves like a no-op.
    - Bitwise “complement” of a single bit-string, denoted by ~.

   (8.7) Operations on fixed-width signed integers

   This section discusses all operations that can be performed on expressions of type int<W> for some W.
   Recall that the int<W> denotes signed W-bit integers, represented using two's complement.

   The int<W> datatype supports the following operations;
   all binary operations require both operands to have the exact same type.
   Theult always has the same width as the left operand.

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

and check_unop_bnot (typ : Type.t) =
  match typ with FIntT _ | FBitT _ -> true | _ -> false

and check_unop_lnot (typ : Type.t) = match typ with BoolT -> true | _ -> false

and check_unop_uplus (typ : Type.t) =
  match typ with IntT | FIntT _ | FBitT _ -> true | _ -> false

and check_unop_uminus (typ : Type.t) =
  match typ with IntT | FIntT _ | FBitT _ -> true | _ -> false

and type_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : El.Ast.unop)
    (expr : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_il = type_expr cursor ctx expr in
  let expr_il =
    match unop.it with
    | BNotOp -> coerce_type_unary_numeric check_unop_bnot expr_il
    | LNotOp -> coerce_type_unary_numeric check_unop_lnot expr_il
    | UPlusOp -> coerce_type_unary_numeric check_unop_uplus expr_il
    | UMinusOp -> coerce_type_unary_numeric check_unop_uminus expr_il
  in
  let typ = expr_il.note.typ in
  let expr_il = Il.Ast.UnE { unop; expr = expr_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.2) Operaions on error types

    The error type only supports equality (==) and inequality (!=) comparisons.
    Theult of such a comparison is a Boolean value.

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
     - Equality and inequality tests, denoted by == and !=pectively.

    (8.6) Operations on fixed-width bit types (unsigned integers)

    All binary operations except shifts and concatenation require both operands to have the same exact type and width;
    supplying operands with different widths produces an error at compile time.
    No implicit casts are inserted by the compiler to equalize the widths.
    There are no other binary operations that accept signed and unsigned values simultaneously besides shifts and concatenation.
    The following operations are provided on bit-string expressions:

     - Test for equality between bit-strings of the same width, designated by ==. Theult is a Boolean value.
     - Test for inequality between bit-strings of the same width, designated by !=. Theult is a Boolean value.
     - Unsigned comparisons <,>,<=,>=. Both operands must have the same width and theult is a Boolean value.

   Each of the following operations produces a bit-stringult when applied to bit-strings of the same width:

     - Addition, denoted by +.
     - Subtraction, denoted by -.
         Theult is unsigned, and has the same type as the operands.
     - Multiplication, denoted by *.
         Theult has the same width as the operands and is computed by truncating theult to the output's width.
         P4 architectures may impose additionaltrictions
         — e.g., they may only allow multiplication by a non-negative integer power of two.
     - Bitwise “and” between two bit-strings of the same width, denoted by &.
     - Bitwise “or” between two bit-strings of the same width, denoted by |.
     - Bitwise “complement” of a single bit-string, denoted by ~.
     - Bitwise “xor” of two bit-strings of the same width, denoted by ^.
     - Saturating addition, denoted by |+|.
     - Saturating subtraction, denoted by |-|.

    Bit-strings also support the following operations:

     - Logical shift left and right by a (not-necessarily-known-at-compile-time) non-negative integer value,
       denoted by << and >>pectively. In a shift, the left operand is unsigned, and right operand must be either
       an expression of type bit<S> or a non-negative integer value that is known at compile time.
       Theult has the same type as the left operand.

    (8.7) Operations on fixed-width signed integers

    All binary operations except shifts and concatenation require both operands to have the same exact type (signedness)
    and width and supplying operands with different widths or signedness produces a compile-time error.
    No implicit casts are inserted by the compiler to equalize the types. Except for shifts and concatenation,
    P4 does not have any binary operations that operate simultaneously on signed and unsigned values.

    Note that bitwise operations on signed integers are well-defined, since the representation is mandated to be two's complement.

    The int<W> datatype supports the following operations; all binary operations require both operands to have the exact same type.
    Theult always has the same width as the left operand.

     - Addition, denoted by +.
     - Subtraction, denoted by -.
     - Comparison for equality and inequality, denoted == and !=pectively.
         These operations produce a Booleanult.
     - Numeric comparisons, denoted by <,<=,>, and >=.
         These operations produce a Booleanult.
     - Multiplication, denoted by *.
        ult has the same width as the operands.
         P4 architectures may impose additionaltrictions
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
       or a non-negative integer compile-time known value. Theult has the same type as the left operand.
     - Concatenation of bit-strings and/or fixed-width signed integers, denoted by ++.
       The two operands must be either bit<W> or int<W>, and they can be of different signedness and width.
       Theult has the same signedness as the left operand and the width equal to the sum of the two operands' width.

    (8.8) Operations on arbitrary-precsion integers

    The type int denotes arbitrary-precision integers. In P4, all expressions of type int must be compile-time known values. The type int supports the following operations:

     - Addition, denoted by +.
     - Subtraction, denoted by -.
     - Comparison for equality and inequality, denoted by == and !=pectively.
         These operations produce a Booleanult.
     - Numeric comparisons <,<=,>, and >=.
         These operations produce a Booleanult.
     - Multiplication, denoted by *.
     - Truncating integer division between positive values, denoted by /.
     - Modulo between positive values, denoted by %.
     - Arithmetic shift left and right denoted by << and >>.
         These operations produce an intult.
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
    Theult is a bit-string whose length is the sum of the lengths of the inputs
    where the most significant bits are taken from the left operand; the sign of theult is taken from the left operand.

    (8.9.2) A note about shifts

    The left operand of shifts can be any one out of unsigned bit-strings, signed bit-strings,
    and arbitrary-precision integers, and the right operand of shifts must be either an expression of type bit<S>
    or a non-negative integer compile-time known value. Theult has the same type as the left operand.

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

    Tuples can be compared for equality using == and !=; two tuples are equal if and only if all their fields arepectively equal.

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

and check_binop_plus_minus_mult (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | IntT, IntT -> true
  | FIntT width_l, FIntT width_r | FBitT width_l, FBitT width_r ->
      Bigint.(width_l = width_r)
  | _ -> false

and type_binop_plus_minus_mult (binop : Lang.Ast.binop)
    (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr'
    =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_plus_minus_mult expr_l_il expr_r_il
  in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  let typ = expr_l_il.note.typ in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_saturating_plus_minus (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | FIntT width_l, FIntT width_r | FBitT width_l, FBitT width_r ->
      Bigint.(width_l = width_r)
  | _ -> false

and type_binop_saturating_plus_minus (binop : Lang.Ast.binop)
    (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr'
    =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_plus_minus_mult expr_l_il expr_r_il
  in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  let typ = expr_l_il.note.typ in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_div_mod (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with IntT, IntT -> true | _ -> false

and type_binop_div_mod (cursor : Ctx.cursor) (ctx : Ctx.t)
    (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr) (expr_r_il : Il.Ast.expr)
    : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_div_mod expr_l_il expr_r_il
  in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  (* Non-positivity check if the right hand side is local compile-time known *)
  (if Ctk.is_lctk expr_r_il.note.ctk then
     let value_divisor = Static.eval_expr cursor ctx expr_r_il in
     let divisor = value_divisor.it |> Value.get_num in
     check
       (not Bigint.(divisor <= zero))
       (F.asprintf
          "(type_binop_div_mod) Division or modulo by a non-positive integer \
           %a is not allowed"
          (Value.pp ~level:0) value_divisor.it));
  let typ = expr_l_il.note.typ in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_shift (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | IntT, IntT
  | IntT, FIntT _
  | IntT, FBitT _
  | FIntT _, IntT
  | FIntT _, FIntT _
  | FIntT _, FBitT _
  | FBitT _, IntT
  | FBitT _, FIntT _
  | FBitT _, FBitT _ ->
      true
  | _ -> false

and type_binop_shift (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_shift expr_l_il expr_r_il
  in
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  check
    (implies
       (match typ_r with FIntT _ | IntT -> true | _ -> false)
       (Ctk.is_lctk expr_r_il.note.ctk))
    "(type_binop_shift) if a signed integer type is used as the right operand \
     of a shift operator, it must be a local compile-time known integer";
  let typ = typ_l in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_compare (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | IntT, IntT -> true
  | FIntT width_l, FIntT width_r | FBitT width_l, FBitT width_r ->
      Bigint.(width_l = width_r)
  | _ -> false

and type_binop_compare (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_compare expr_l_il expr_r_il
  in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  let typ = Types.BoolT in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and type_binop_compare_equal (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  let typ = expr_l_il.note.typ in
  check (Type.is_equalable typ)
    (F.asprintf
       "(type_binop_compare_equal) type %a cannot be compared of equality"
       (Type.pp ~level:0) typ);
  let typ = Types.BoolT in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_bitwise (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | FIntT width_l, FIntT width_r | FBitT width_l, FBitT width_r ->
      Bigint.(width_l = width_r)
  | _ -> false

and type_binop_bitwise (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_bitwise expr_l_il expr_r_il
  in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  let typ = expr_l_il.note.typ in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_concat (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | FIntT _, FIntT _ | FIntT _, FBitT _ | FBitT _, FIntT _ | FBitT _, FBitT _ ->
      true
  | _ -> false

and type_binop_concat (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_concat expr_l_il expr_r_il
  in
  let typ_l, typ_r = (expr_l_il.note.typ, expr_r_il.note.typ) in
  let typ =
    match (typ_l, typ_r) with
    | FIntT width_l, FIntT width_r -> Types.FIntT Bigint.(width_l + width_r)
    | FIntT width_l, FBitT width_r -> Types.FIntT Bigint.(width_l + width_r)
    | FBitT width_l, FIntT width_r -> Types.FBitT Bigint.(width_l + width_r)
    | FBitT width_l, FBitT width_r -> Types.FBitT Bigint.(width_l + width_r)
    | _ -> assert false
  in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

and check_binop_logical (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with BoolT, BoolT -> true | _ -> false

and type_binop_logical (binop : Lang.Ast.binop) (expr_l_il : Il.Ast.expr)
    (expr_r_il : Il.Ast.expr) : Type.t * Il.Ast.expr' =
  let expr_l_il, expr_r_il = coerce_types_binary expr_l_il expr_r_il in
  let expr_l_il, expr_r_il =
    coerce_types_binary_numeric check_binop_logical expr_l_il expr_r_il
  in
  assert (Type.eq_alpha expr_l_il.note.typ expr_r_il.note.typ);
  let typ = Types.BoolT in
  let expr_il = Il.Ast.BinE { binop; expr_l = expr_l_il; expr_r = expr_r_il } in
  (typ, expr_il)

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
    | DivOp | ModOp -> type_binop_div_mod cursor ctx binop expr_l_il expr_r_il
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
   on theult.

   The first sub-expression e1 must have Boolean type and the second and third sub-expressions must have the same type,
   which cannot both be arbitrary-precision integers unless the condition itself can be evaluated at compilation time.
   This restriction is designed to ensure that the width of theult of the conditional expression can be inferred
   statically at compile time. *)

and type_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_cond : El.Ast.expr) (expr_then : El.Ast.expr)
    (expr_else : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_cond_il = type_expr cursor ctx expr_cond in
  let typ_cond = expr_cond_il.note.typ in
  let ctk_cond = expr_cond_il.note.ctk in
  check (typ_cond = Types.BoolT)
    (F.asprintf "(type_ternop_expr) condition %a must be a boolean"
       (El.Pp.pp_expr ~level:0) expr_cond);
  let expr_then_il = type_expr cursor ctx expr_then in
  let expr_else_il = type_expr cursor ctx expr_else in
  let expr_then_il, expr_else_il =
    coerce_types_binary expr_then_il expr_else_il
  in
  assert (Type.eq_alpha expr_then_il.note.typ expr_else_il.note.typ);
  let typ = expr_then_il.note.typ in
  check
    (implies (typ = Types.IntT) (Ctk.is_ctk ctk_cond))
    (F.asprintf
       "(type_ternop_expr) Branches %a and %a cannot both be \
        arbitrary-precision integers when the condition %a is not compile-time \
        known\n"
       (El.Pp.pp_expr ~level:0) expr_then (El.Pp.pp_expr ~level:0) expr_else
       (El.Pp.pp_expr ~level:0) expr_cond);
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
  let typ_target, tids_fresh = eval_type_with_check cursor ctx typ in
  assert (tids_fresh = []);
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  check
    (Subtyp.explicit typ typ_target.it)
    (F.asprintf "(type_cast_expr) invalid cast from %a to %a" (Type.pp ~level:0)
       typ (Type.pp ~level:0) typ_target.it);
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

and check_mask (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | IntT, IntT -> true
  | FIntT width_l, FIntT width_r | FBitT width_l, FBitT width_r ->
      Bigint.(width_l = width_r)
  | _ -> false

and type_mask_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : El.Ast.expr)
    (expr_mask : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let expr_mask_il = type_expr cursor ctx expr_mask in
  let expr_base_il, expr_mask_il =
    coerce_types_binary expr_base_il expr_mask_il
  in
  let expr_base_il, expr_mask_il =
    coerce_types_binary_numeric check_mask expr_base_il expr_mask_il
  in
  assert (Type.eq_alpha expr_base_il.note.typ expr_mask_il.note.typ);
  let typ = expr_base_il.note.typ in
  let typ = Types.SetT typ in
  (* Well-formedness check is deferred until keyset check,
     where the underlying type will be implicitly cast *)
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

and check_range (typ_l : Type.t) (typ_r : Type.t) : bool =
  match (typ_l, typ_r) with
  | IntT, IntT -> true
  | FIntT width_l, FIntT width_r | FBitT width_l, FBitT width_r ->
      Bigint.(width_l = width_r)
  | _ -> false

and type_range_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_lb : El.Ast.expr)
    (expr_ub : El.Ast.expr) : Type.t * Ctk.t * Il.Ast.expr' =
  let expr_lb_il = type_expr cursor ctx expr_lb in
  let expr_ub_il = type_expr cursor ctx expr_ub in
  let expr_lb_il, expr_ub_il = coerce_types_binary expr_lb_il expr_ub_il in
  let expr_lb_il, expr_ub_il =
    coerce_types_binary_numeric check_range expr_lb_il expr_ub_il
  in
  assert (Type.eq_alpha expr_lb_il.note.typ expr_ub_il.note.typ);
  let typ = expr_lb_il.note.typ in
  let typ = Types.SetT typ in
  (* Well-formedness check is deferred until keyset check,
     where the underlying type will be implicitly cast *)
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

and type_select_case_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typ_key : Type.t) (keyset : El.Ast.keyset) : Il.Ast.keyset =
  let keyset_il = type_select_case_keyset' cursor ctx typ_key keyset.it in
  keyset_il $ keyset.at

and type_select_case_keyset' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typ_key : Type.t) (keyset : El.Ast.keyset') : Il.Ast.keyset' =
  assert (cursor = Ctx.Local);
  match keyset with
  | ExprK expr ->
      let expr_il = type_expr cursor ctx expr in
      let expr_il =
        match expr_il.note.typ with
        | SetT typ_inner ->
            check
              (Subtyp.implicit typ_inner typ_key)
              (F.asprintf
                 "(type_select_case_keyset') keyset expression should have \
                  type %a, but got %a"
                 (Type.pp ~level:0) typ_key (Type.pp ~level:0) typ_inner);
            if Type.eq_alpha typ_inner typ_key then expr_il
            else insert_cast expr_il (Types.SetT typ_key)
        | typ ->
            check
              (Subtyp.implicit typ typ_key)
              (F.asprintf
                 "(type_select_case_keyset') keyset expression should have \
                  type %a, but got %a"
                 (Type.pp ~level:0) typ_key (Type.pp ~level:0) typ);
            insert_cast expr_il (Types.SetT typ_key)
      in
      Lang.Ast.ExprK expr_il
  | DefaultK -> Lang.Ast.DefaultK
  | AnyK -> Lang.Ast.AnyK

and type_select_case_keysets (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typs_key : Type.t list) (keysets : El.Ast.keyset list) : Il.Ast.keyset list
    =
  match (typs_key, keysets) with
  | [ typ_key ], [ keyset ] ->
      let keyset_il = type_select_case_keyset cursor ctx typ_key keyset in
      [ keyset_il ]
  | _, [ keyset ] ->
      let typ_key = Types.SeqT typs_key in
      let keyset_il = type_select_case_keyset cursor ctx typ_key keyset in
      [ keyset_il ]
  | typs_key, keysets ->
      check
        (List.length typs_key = List.length keysets)
        "(type_select_case_keysets) Number of select keys must match the \
         number of keysets";
      let rec type_select_case_keysets' keysets_il typs_key keysets =
        match (typs_key, keysets) with
        | [], [] -> keysets_il
        | typ_key :: typs_key, keyset :: keysets ->
            let keyset_il = type_select_case_keyset cursor ctx typ_key keyset in
            type_select_case_keysets'
              (keysets_il @ [ keyset_il ])
              typs_key keysets
        | _ -> assert false
      in
      type_select_case_keysets' [] typs_key keysets

and type_select_case (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typs_key : Type.t list) (case : El.Ast.select_case) : Il.Ast.select_case =
  let case_il = type_select_case' cursor ctx typs_key case.it in
  case_il $ case.at

and type_select_cases (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typs_key : Type.t list) (cases : El.Ast.select_case list) :
    Il.Ast.select_case list =
  List.map (type_select_case cursor ctx typs_key) cases

and type_select_case' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typs_key : Type.t list) (case : El.Ast.select_case') : Il.Ast.select_case'
    =
  assert (cursor = Ctx.Local);
  let keysets, state_label = case in
  let keysets_il = type_select_case_keysets cursor ctx typs_key keysets in
  let stype_label = Ctx.find_stype_opt cursor state_label.it ctx in
  check
    (match stype_label with Some (Types.StateT, _, _, _) -> true | _ -> false)
    (F.asprintf "(type_select_case) Label %s is not a valid label"
       state_label.it);
  (keysets_il, state_label)

and check_select_key (typ : Type.t) : unit =
  check (check_select_key' typ)
    (F.asprintf "(check_select_key) %a is not a valid select key type"
       (Type.pp ~level:0) typ)

and check_select_key' (typ : Type.t) : bool =
  let typ = Type.canon typ in
  match typ with
  | DefT _ | SpecT _ -> assert false
  | BoolT | FIntT _ | FBitT _ | EnumT _ -> true
  | SEnumT (_, typ_inner, _) -> check_select_key' typ_inner
  | NewT (_, typ_inner) -> check_select_key' typ_inner
  | TupleT typs -> List.for_all check_select_key' typs
  | SeqT typs -> List.for_all check_select_key' typs
  | _ -> false

and type_select_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs_select : El.Ast.expr list) (cases : El.Ast.select_case list) :
    Type.t * Ctk.t * Il.Ast.expr' =
  check
    (cursor = Ctx.Local
    && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
    "(type_select_expr) Select expression must be in a parser state (more \
     strictly, only nested inside a transition statement)";
  let exprs_select_il = type_exprs cursor ctx exprs_select in
  let typs_select =
    List.map (fun expr -> Il.Ast.(expr.note.typ)) exprs_select_il
  in
  List.iter
    (fun typ_select ->
      check_select_key typ_select;
      WF.check_valid_typ cursor ctx (Types.SetT typ_select))
    typs_select;
  let cases_il = type_select_cases cursor ctx typs_select cases in
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
      if hs is an l-value, theult is also an l-value. The header may be invalid.
      Some implementations may impose the constraint that the index expression evaluates to a value
      that is known at compile time. A P4 compiler must give an error if an index value that
      is a compile-time constant is out of range.
      Accessing a header stack hs with an index less than 0 or greater than or equal to hs.size
     ults in an undefined value. See Section 8.25 for more details.
      The index is an expression that must be of numeric types (Section 7.4). *)

and type_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_idx : El.Ast.expr) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let expr_idx_il = type_expr cursor ctx expr_idx in
  let expr_idx_il = coerce_type_unary_numeric Type.is_numeric expr_idx_il in
  let typ, expr_il =
    let typ_base = expr_base_il.note.typ |> Type.canon in
    match typ_base with
    | TupleT typs_base_inner ->
        let value_idx = Static.eval_expr cursor ctx expr_idx_il in
        let idx = value_idx.it |> Value.get_num |> Bigint.to_int_exn in
        check
          (idx >= 0 && idx < List.length typs_base_inner)
          (F.asprintf "(type_array_acc_expr) Index %d out of range for %a\n" idx
             (Il.Pp.pp_expr ~level:0) expr_base_il);
        let typ = List.nth typs_base_inner idx in
        let expr_idx_il =
          Il.Ast.(
            ValueE { value = value_idx } $$ expr_idx_il.at % expr_idx_il.note)
        in
        let expr_il =
          Il.Ast.ArrAccE { expr_base = expr_base_il; expr_idx = expr_idx_il }
        in
        (typ, expr_il)
    | StackT (typ_base_inner, size) ->
        let typ = typ_base_inner in
        let expr_il =
          Il.Ast.ArrAccE { expr_base = expr_base_il; expr_idx = expr_idx_il }
        in
        (* Bounds check if the index is local compile-time known *)
        (if Ctk.is_lctk expr_idx_il.note.ctk then
           let value_idx = Static.eval_expr cursor ctx expr_idx_il in
           let idx = value_idx.it |> Value.get_num in
           check
             (Bigint.(idx >= zero) && Bigint.(idx < size))
             (F.asprintf "(type_array_acc_expr) Index %a out of range for %a"
                (Value.pp ~level:0) value_idx.it (Il.Pp.pp_expr ~level:0)
                expr_base_il));
        (typ, expr_il)
    | _ ->
        F.asprintf "(type_array_acc_expr) %a cannot be indexed"
          (Il.Pp.pp_expr ~level:0) expr_base_il
        |> error_no_info
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.6) Operations on fixed-width bit types (unsigned integers)

   Bit-strings also support the following operations:

    - Extraction of a set of contiguous bits, also known as a slice, denoted by [H:L],
      where H and L must be expressions that evaluate to non-negative compile-time known values, and H >= L.
      The types of H and L (which do not need to be identical) must be numeric (Section 7.4).
      Theult is a bit-string of width H - L + 1, including the bits numbered from L
      (which becomes the least significant bit of theult) to H (the most significant bit of theult)
      from the source operand. The conditions 0 <= L <= H < W are checked statically
      (where W is the length of the source bit-string). Note that both endpoints of the extraction are inclusive.
      The bounds are required to be known-at-compile-time values so that theult width can be computed at
      compile time. Slices are also l-values, which means that P4 supports assigning to a slice:  e[H:L] = x .
      The effect of this statement is to set bits H through L (inclusive of both) of e to the
      bit-pattern represented by x, and leaves all other bits of e unchanged.
      A slice of an unsigned integer is an unsigned integer.

   (8.7) Operations on fixed-width signed integers

    - Extraction of a set of contiguous bits, also known as a slice, denoted by [H:L],
      where H and L must be expressions that evaluate to non-negative compile-time known values,
      and H >= L must be true.
      Theult is an unsigned bit-string of width H - L + 1, including the bits numbered from L
      (which becomes the least significant bit of theult) to H (the most significant bit of theult)
      from the source operand.

   (8.8) Operations on arbitrary-precision integers

   Bit slices, denoted by [H:L], where H and L must be expressions that evaluate to
   non-negative compile-time known values, and H >= L must be true. The types of H and L
   (which do not need to be identical) must be one of the following:

    - int - an arbitrary-precision integer (section 7.1.6.5)
    - bit<W> - a W-bit unsigned integer where W >= 0 (section 7.1.6.2)
    - int<W> - a W-bit signed integer where W >= 1 (section 7.1.6.3)
    - a serializable enum with an underlying type that is bit<W> or int<W> (section 7.2.1). *)

and check_bitstring_base (typ : Type.t) : bool =
  let typ = Type.canon typ in
  match typ with
  | IntT -> true
  | FIntT width -> Bigint.(width > zero)
  | FBitT width -> Bigint.(width >= zero)
  | _ -> false

and check_bitstring_index (typ : Type.t) : bool =
  let typ = Type.canon typ in
  match typ with IntT | FIntT _ | FBitT _ -> true | _ -> false

and check_bitstring_slice_range' (typ_base : Type.t) (idx_lo : Bigint.t)
    (idx_hi : Bigint.t) : bool =
  let typ_base = Type.canon typ_base in
  match typ_base with
  | IntT -> true
  | FIntT width_base | FBitT width_base ->
      let width_slice = Bigint.(idx_hi - idx_lo + one) in
      Bigint.(idx_hi <= width_base) && Bigint.(width_slice <= width_base)
  | _ -> false

and check_bitstring_slice_range (typ_base : Type.t) (idx_lo : Bigint.t)
    (idx_hi : Bigint.t) : unit =
  check
    (Bigint.(idx_lo >= zero)
    && Bigint.(idx_hi >= zero)
    && Bigint.(idx_lo <= idx_hi)
    && check_bitstring_slice_range' typ_base idx_lo idx_hi)
    (F.asprintf "(check_bitstring_slice_range) Invalid slice [%a:%a] for %a\n"
       Bigint.pp idx_lo Bigint.pp idx_hi (Type.pp ~level:0) typ_base)

and type_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (expr_lo : El.Ast.expr) (expr_hi : El.Ast.expr) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let expr_base_il =
    coerce_type_unary_numeric check_bitstring_base expr_base_il
  in
  let typ_base = expr_base_il.note.typ in
  let expr_lo_il = type_expr cursor ctx expr_lo in
  let expr_lo_il = coerce_type_unary_numeric check_bitstring_index expr_lo_il in
  let value_lo = Static.eval_expr cursor ctx expr_lo_il in
  let idx_lo = value_lo.it |> Value.get_num in
  let expr_hi_il = type_expr cursor ctx expr_hi in
  let expr_hi_il = coerce_type_unary_numeric check_bitstring_index expr_hi_il in
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
  check
    (Option.is_some value_error)
    (F.asprintf "(type_error_acc_expr) Member %s does not exist in error"
       member.it);
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
  let td_base = Ctx.find_opt Ctx.find_typdef_opt cursor var_base ctx in
  check (Option.is_some td_base)
    (F.asprintf "(type_type_acc_expr) %a is a free identifier" El.Pp.pp_var
       var_base);
  let td_base = Option.get td_base in
  let typ, value =
    let typ_base =
      match td_base with
      | MonoD typ_base -> typ_base
      | _ ->
          F.asprintf "(type_type_acc_expr) Cannot access a generic type %a"
            (TypeDef.pp ~level:0) td_base
          |> error_no_info
    in
    match Type.canon typ_base with
    | EnumT (id, members) ->
        check
          (List.mem member.it members)
          (F.asprintf "(type_type_acc_expr) member %s does not exist in %a"
             member.it (TypeDef.pp ~level:0) td_base);
        let value = Value.EnumFieldV (id, member.it) in
        (typ_base, value)
    | SEnumT (id, _, fields) ->
        check
          (List.mem_assoc member.it fields)
          (F.asprintf "(type_type_acc_expr) member %s does not exist in %a"
             member.it (TypeDef.pp ~level:0) td_base);
        let value_inner = List.assoc member.it fields in
        let value = Value.SEnumFieldV (id, member.it, value_inner) in
        (typ_base, value)
    | _ ->
        F.asprintf "(type_type_acc_expr) %a cannot be accessed\n"
          (TypeDef.pp ~level:0) td_base
        |> error_no_info
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
     then evaluating this expressionults in a transition to reject and sets the error to error.StackOutOfBounds.
     If hs is an l-value, then hs.next is also an l-value.
   - hs.last: produces a reference to the element with index hs.nextIndex - 1 in the stack, if such an element exists.
     May only be used in a parser. If the nextIndex counter is less than 1, or greater than size,
     then evaluating this expressionults in a transition to reject and sets the error to error.StackOutOfBounds.
     Unlike hs.next, theulting reference is never an l-value.
   - hs.lastIndex: produces a 32-bit unsigned integer that encodes the index hs.nextIndex - 1.
     May only be used in a parser. If the nextIndex counter is 0, then evaluating this expression produces an undefined value.

   (8.19) Operations on header unions *)

and type_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : El.Ast.expr) (member : El.Ast.member) :
    Type.t * Ctk.t * Il.Ast.expr' =
  let expr_base_il = type_expr cursor ctx expr_base in
  let typ =
    let typ_base = expr_base_il.note.typ |> Type.canon in
    match typ_base with
    | StackT (typ_inner, _) -> (
        check
          (implies
             (match member.it with "last" | "lastIndex" -> true | _ -> false)
             ((cursor = Ctx.Block && ctx.block.kind = Ctx.Parser)
             || (cursor = Ctx.Local && ctx.local.kind = Ctx.ParserState)))
          (F.asprintf
             "(type_expr_acc_expr) last and lastIndex of %a may only be \
              accessed within a parser"
             (El.Pp.pp_expr ~level:0) expr_base);
        match member.it with
        | "size" | "lastIndex" -> Types.FBitT (Bigint.of_int 32)
        | "next" | "last" -> typ_inner
        | _ ->
            F.asprintf "(type_expr_acc_expr) invalid member %a for header stack"
              Il.Pp.pp_member member
            |> error_no_info)
    | StructT (_, fields)
    | HeaderT (_, fields)
    | UnionT (_, fields)
    | TableStructT (_, fields) ->
        let typ_inner = List.assoc_opt member.it fields in
        check (Option.is_some typ_inner)
          (F.asprintf "(type_expr_acc_expr) Member %s does not exist in %a"
             member.it (El.Pp.pp_expr ~level:0) expr_base);
        let typ_inner = Option.get typ_inner in
        typ_inner
    | _ ->
        F.asprintf "(type_expr_acc_expr) %a cannot be accessed"
          (El.Pp.pp_expr ~level:0) expr_base
        |> error_no_info
  in
  let expr_il = Il.Ast.ExprAccE { expr_base = expr_base_il; member } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.20) Method invocations and function calls

   Function arguments are evaluated in the order they appear, left to right, before the function invocation takes place.

   The calling convention is copy-in/copy-out (Section 6.8).
   For generic functions the type arguments can be explicitly specified in the function call.
   The compiler only inserts implicit casts for direction in arguments to methods or functions as described in Section 8.11.
   The types for all other arguments must match the parameter types exactly.

   Theult returned by a function call is discarded when the function call is used as a statement.

   The “don't care” identifier (_) can only be used for an out function/method argument,
   when the value of returned in that argument is ignored by subsequent computations.
   When used in generic functions or methods, the compiler may reject the program if it is
   unable to infer a type for the don't care argument. *)

(* (Appendix F)trictions on compile time and run time calls

   The next table liststrictions on what kinds of calls can be made from which places in a P4 program.
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
   particular externs may have additionaltrictions not listed in this table.
   Any suchtrictions should be documented in the description for each extern,
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
   required to store the expression. Thus, theult type of these methods has type int.
   Except in certain situations involving type variables, discussed below, these method calls produce
   local compile-time known values; otherwise they produce compile-time known values. None of these methods evaluate
   the expression that is the receiver of the method call, so it may be invalid (e.g., an out-of-bounds header stack access).

   The methods can also be applied to type name expressions e:

    - if the type of e is a type introduced by type, theult is the application of the method to the underlying type
    - if e is the name of a type (e.g., introduced by a typedef declaration), where the type given a name is one of the above,
      then theult is obtained by applying the method to the underlying type.

   These methods are defined for:

    - all serializable types
    - for a type that does not contain varbit fields, both methods return the sameult
    - for a type that does contain varbit fields, maxSizeInBits is the worst-case size
      of the serialized representation of the data and minSizeInBits is the “best” case.
    - Every other case is undefined and will produce a compile-time error. *)

and check_call_arity (ft : FuncType.t) (params : Types.param list)
    (args : Il.Ast.arg list) : unit =
  let arity_params = List.length params in
  let arity_args = List.length args in
  check
    (arity_params = arity_args)
    (F.asprintf
       "(check_call_arity) function %a expects %d arguments but %d were given\n"
       (FuncType.pp ~level:0) ft arity_params arity_args)

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

and check_call_site (cursor : Ctx.cursor) (ctx : Ctx.t) (ft : FuncType.t) : unit
    =
  match cursor with
  | Global ->
      F.asprintf "(check_call_site) %a cannot be called from top level"
        (FuncType.pp ~level:0) ft
      |> error_no_info
  | Block -> (
      let kind = ctx.block.kind in
      match (kind, ft) with
      | ( Parser,
          ( ExternFunctionT _ | ExternMethodT _ | ExternAbstractMethodT _
          | BuiltinMethodT _ ) )
      | ( Control,
          ( ExternFunctionT _ | ExternMethodT _ | ExternAbstractMethodT _
          | BuiltinMethodT _ ) ) ->
          ()
      | _ ->
          F.asprintf "(check_call_site) %a cannot be called from %a"
            (FuncType.pp ~level:0) ft Ctx.pp_blockkind kind
          |> error_no_info)
  | Local -> (
      let kind = ctx.local.kind in
      match (kind, ft) with
      | Function _, (FunctionT _ | BuiltinMethodT _)
      | ( Action,
          ( ActionT _ | FunctionT _ | ExternFunctionT _ | ExternMethodT _
          | ExternAbstractMethodT _ | BuiltinMethodT _ ) )
      | ( ExternAbstractMethod _,
          ( ExternFunctionT _ | ExternMethodT _ | ExternAbstractMethodT _
          | BuiltinMethodT _ ) )
      | ( ParserState,
          ( ExternFunctionT _ | FunctionT _ | ExternMethodT _
          | ExternAbstractMethodT _ | ParserApplyMethodT _ | BuiltinMethodT _ )
        )
      | ( ControlApplyMethod,
          ( ActionT _ | ExternFunctionT _ | FunctionT _ | ExternMethodT _
          | ExternAbstractMethodT _ | ControlApplyMethodT _ | BuiltinMethodT _
          | TableApplyMethodT _ ) )
      | ( TableApplyMethod,
          ( ActionT _ | ExternFunctionT _ | FunctionT _ | ExternMethodT _
          | ExternAbstractMethodT _ | TableApplyMethodT _ | BuiltinMethodT _ ) )
        ->
          ()
      | _ ->
          F.asprintf "(check_call_site) %a cannot be called from %a"
            (FuncType.pp ~level:0) ft Ctx.pp_localkind kind
          |> error_no_info)

and type_func (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : El.Ast.var)
    (targs_il : Il.Ast.targ list) (args : El.Ast.arg list) :
    FuncType.t * TId.t list * Types.tparam list * Il.Ast.id' list =
  let targs = List.map it targs_il in
  let fd_matched =
    let args = FId.to_names args in
    Ctx.find_f_opt Ctx.find_funcdef_opt cursor var_func args ctx
  in
  check
    (Option.is_some fd_matched)
    (F.asprintf "(type_func) function %a not found" El.Pp.pp_var var_func);
  let _, fd, args_default = Option.get fd_matched in
  let tparams = FuncDef.get_tparams fd |> fst in
  let ft, tids_fresh = FuncDef.specialize Ctx.fresh fd targs in
  (ft, tids_fresh, tparams, args_default)

and type_method (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : El.Ast.expr)
    (member : El.Ast.member) (targs_il : Il.Ast.targ list)
    (args : El.Ast.arg list) :
    FuncType.t * Il.Ast.expr * TId.t list * Types.tparam list * Il.Ast.id' list
    =
  let error_not_found () =
    F.asprintf "(type_method) method %a not found for %a" El.Pp.pp_member member
      (El.Pp.pp_expr ~level:0) expr_base
    |> error_no_info
  in
  let targs = List.map it targs_il in
  let expr_base_il = type_expr cursor ctx expr_base in
  let typ_base = expr_base_il.note.typ |> Type.canon in
  let ft, tids_fresh, tparams, args_default =
    let wrap_builtin ft = (ft, [], [], []) in
    let find_method fdenv =
      let fd_matched =
        let args = FId.to_names args in
        FDEnv.find_func_opt (member.it, args) fdenv
      in
      match fd_matched with
      | Some (_, fd, args_default) ->
          let ft, tids_fresh = FuncDef.specialize Ctx.fresh fd targs in
          let tparams = FuncDef.get_tparams fd |> fst in
          (ft, tids_fresh, tparams, args_default)
      | None -> error_not_found ()
    in
    match (typ_base, member.it) with
    | _, "minSizeInBits"
    | _, "minSizeInBytes"
    | _, "maxSizeInBits"
    | _, "maxSizeInBytes" ->
        Types.BuiltinMethodT ([], Types.IntT) |> wrap_builtin
    | StackT _, "push_front" | StackT _, "pop_front" ->
        let params = [ ("count", Lang.Ast.In, Types.IntT, None) ] in
        let typ_ret = Types.VoidT in
        Types.BuiltinMethodT (params, typ_ret) |> wrap_builtin
    | HeaderT _, "isValid" ->
        Types.BuiltinMethodT ([], Types.BoolT) |> wrap_builtin
    | HeaderT _, "setValid" | HeaderT _, "setInvalid" ->
        Types.BuiltinMethodT ([], Types.VoidT) |> wrap_builtin
    | UnionT _, "isValid" ->
        Types.BuiltinMethodT ([], Types.BoolT) |> wrap_builtin
    | ExternT (_, fdenv), _ -> find_method fdenv
    | ParserT params, _ ->
        let fd =
          let ft = Types.ParserApplyMethodT params in
          Types.MonoFD ft
        in
        let fdenv =
          let params =
            List.map
              (fun (id, _, _, value_default) ->
                (id, Option.is_some value_default))
              params
          in
          let fid = ("apply", params) in
          FDEnv.add_nodup_non_overloaded fid fd FDEnv.empty
        in
        find_method fdenv
    | ControlT params, _ ->
        let fd =
          let ft = Types.ControlApplyMethodT params in
          Types.MonoFD ft
        in
        let fdenv =
          let params =
            List.map
              (fun (id, _, _, value_default) ->
                (id, Option.is_some value_default))
              params
          in
          let fid = ("apply", params) in
          FDEnv.add_nodup_non_overloaded fid fd FDEnv.empty
        in
        find_method fdenv
    | TableT (_, typ), _ -> (
        match member.it with
        | "apply" -> Types.TableApplyMethodT typ |> wrap_builtin
        | _ -> error_not_found ())
    | _ -> error_not_found ()
  in
  (ft, expr_base_il, tids_fresh, tparams, args_default)

and type_call (cursor : Ctx.cursor) (ctx : Ctx.t) (tids_fresh : TId.t list)
    (ft : FuncType.t) (tparams : Types.tparam list)
    (targs_il : Il.Ast.targ list) (args : El.Ast.arg list)
    (args_default : Il.Ast.id' list) :
    Il.Ast.targ list * Il.Ast.arg list * Type.t =
  let params = FuncType.get_params ft in
  let params =
    List.filter (fun (id, _, _, _) -> not (List.mem id args_default)) params
  in
  let args_il_typed = type_args cursor ctx args in
  let args_il, typ_args = List.split args_il_typed in
  check_call_arity ft params args_il;
  let params, typ_args, args_il =
    align_params_with_args params typ_args args_il
  in
  let args_il_typed = List.combine args_il typ_args in
  let typ_ret = FuncType.get_typ_ret ft in
  let ft, targs_il, params, typ_ret =
    match tids_fresh with
    | [] -> (ft, targs_il, params, typ_ret)
    | _ ->
        let theta = infer_targs tids_fresh params args_il_typed in
        let targs_il =
          targs_il
          @ (List.map (fun tid_fresh -> TIdMap.find tid_fresh theta) tids_fresh
            |> List.map (fun typ -> typ $ no_info))
        in
        let targs_il =
          List.filteri (fun i _ -> i < List.length tparams) targs_il
        in
        let ft = FuncType.subst theta ft in
        let params =
          List.map (Runtime_static.Tdomain.Subst.subst_param theta) params
        in
        let typ_ret = Type.subst theta typ_ret in
        (ft, targs_il, params, typ_ret)
  in
  WF.check_valid_functyp cursor ctx ft;
  check_call_site cursor ctx ft;
  let action = FuncType.is_action ft in
  let args_il = type_call_convention ~action cursor ctx params args_il_typed in
  (targs_il, args_il, typ_ret)

and type_call_func_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_func : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  (* Find the function definition and specialize it if generic *)
  let targs_il, tids_fresh = eval_types_with_check cursor ctx [] targs in
  let ft, tids_fresh_inserted, tparams, args_default =
    type_func cursor ctx var_func targs_il args
  in
  let tids_fresh = tids_fresh @ tids_fresh_inserted in
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, typ =
    type_call cursor ctx tids_fresh ft tparams targs_il args args_default
  in
  check (typ <> Types.VoidT)
    (F.asprintf
       "(type_call_expr) function call as an expression must return a value");
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
  let targs_il, tids_fresh = eval_types_with_check cursor ctx [] targs in
  let ft, expr_base_il, tids_fresh_inserted, tparams, args_default =
    type_method cursor ctx expr_base member targs_il args
  in
  let tids_fresh = tids_fresh @ tids_fresh_inserted in
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, typ =
    type_call cursor ctx tids_fresh ft tparams targs_il args args_default
  in
  check (typ <> Types.VoidT)
    (F.asprintf
       "(type_call_expr) function call as an expression must return a value");
  let expr_il =
    Il.Ast.CallMethodE
      { expr_base = expr_base_il; member; targs = targs_il; args = args_il }
  in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (9) Compile-time size determination

   If e is the name of a type (e.g., introduced by a typedef declaration),
   where the type given a name is one of the above,
   then theult is obtained by applying the method to the underlying type. *)

and type_call_type_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_typ : El.Ast.var) (member : El.Ast.member) (targs : El.Ast.targ list)
    (args : El.Ast.arg list) : Type.t * Ctk.t * Il.Ast.expr' =
  check
    ((member.it = "minSizeInBits"
     || member.it = "minSizeInBytes"
     || member.it = "maxSizeInBits"
     || member.it = "maxSizeInBytes")
    && List.length targs = 0
    && List.length args = 0)
    (F.asprintf "(type_call_type_expr) Invalid method %s for type %a\n"
       member.it El.Pp.pp_var var_typ);
  let td = Ctx.find_opt Ctx.find_typdef_opt cursor var_typ ctx in
  check (Option.is_some td)
    (F.asprintf "(type_call_type_expr) %a is a free identifier\n" El.Pp.pp_var
       var_typ);
  let td = Option.get td in
  let typ = TypeDef.specialize td [] in
  let expr_il = Il.Ast.CallTypeE { typ = typ $ var_typ.at; member } in
  let typ = Types.IntT in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

(* (8.21) Constructor invocations

   The syntax for a constructor invocation is similar to a function call;
   constructors can also be called using named arguments. Constructors are evaluated entirely
   at compilation time (see Section 18). In consequence, all constructor arguments must also
   be expressions that can be evaluated at compilation time. When performing type inference
   and overloadolution, constructor invocations are treated similar to methods or functions. *)

(* (11.3) Instantiations

    Instantiations are similar to variable declarations, but areerved for
    the types with constructors (extern objects, control blocks, parsers, and packages).

    An instantiation is written as a constructor invocation followed by a name.
    Instantiations are always executed at compilation time (Section 18.1).
    The effect is to allocate an object with the specified name, and to bind it to the
   ult of the constructor invocation. Note that instantiation arguments can be specified by name. *)

(* (Appendix F)trictions on compile time and run time calls

   The next table liststrictions on where one may perform instantiations (see Section 11.3)
   of different types. The answer for package is always “no” because there is no “inside a package”
   where instantiations can be written in P4_16. One can definitely make constructor calls and
   use instances of stateful types as parameters when instantiating a package,
   andtrictions on those types are in the table above.

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
  let arity_cparams = List.length cparams in
  let arity_args = List.length args in
  check
    (arity_cparams = arity_args)
    (F.asprintf
       "(check_instantiation_arity) Instance %a expects %d arguments but %d \
        were given"
       El.Pp.pp_var var_inst arity_cparams arity_args)

and align_cparams_with_args (cparams : Types.cparam list)
    (typ_args : Type.t list) (args_il : Il.Ast.arg list) =
  align_params_with_args cparams typ_args args_il

and check_instantiation_site (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typ_inst : Type.t) : unit =
  let typ_inst = Type.canon typ_inst in
  match cursor with
  | Global -> (
      match typ_inst with
      | ExternT _ | PackageT _ -> ()
      | _ ->
          F.asprintf
            "(check_instantiation_site) %a cannot be instantiated at top level"
            (Type.pp ~level:0) typ_inst
          |> error_no_info)
  | Block -> (
      let kind = ctx.block.kind in
      match (kind, typ_inst) with
      | Ctx.Package, (ExternT _ | ParserT _ | ControlT _ | PackageT _)
      | Ctx.Parser, (ExternT _ | ParserT _)
      | Ctx.Control, (ExternT _ | ControlT _ | TableT _) ->
          ()
      | _ ->
          F.asprintf
            "(check_instantiation_site) %a cannot be instantiated in %a"
            (Type.pp ~level:0) typ_inst Ctx.pp_blockkind kind
          |> error_no_info)
  | Local -> (
      let kind = ctx.local.kind in
      match (kind, typ_inst) with
      | Ctx.ParserState, (ExternT _ | ParserT _)
      | Ctx.ControlApplyMethod, (ExternT _ | ControlT _) ->
          ()
      | Ctx.TableApplyMethod, ExternT _ -> ()
      | _ ->
          F.asprintf
            "(check_instantiation_site) %a cannot be instantiated in %a"
            (Type.pp ~level:0) typ_inst Ctx.pp_localkind kind
          |> error_no_info)

and type_instantiation (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  (* Find the constructor definition and specialize it if necessary *)
  let targs_il, tids_fresh = eval_types_with_check cursor ctx [] targs in
  let cd_matched =
    let args = FId.to_names args in
    Ctx.find_f_opt Ctx.find_consdef_opt cursor var_inst args ctx
  in
  check
    (Option.is_some cd_matched)
    (F.asprintf "(type_instantiation) instance %a not found" El.Pp.pp_var
       var_inst);
  let _, cd, args_default = Option.get cd_matched in
  let ct, tids_fresh_inserted =
    let targs = List.map it targs_il in
    ConsDef.specialize Ctx.fresh cd targs
  in
  let tids_fresh = tids_fresh @ tids_fresh_inserted in
  let cparams, typ_inst = ct in
  (* Check if the arguments match the parameters *)
  let cparams =
    List.filter (fun (id, _, _, _) -> not (List.mem id args_default)) cparams
  in
  let args_il, typ_args =
    (* Adjust the context if instantiating a package at top level
       Actually, the spec is quite imprecise intricting parser, control
       instantiations in top-level, while they are necessarily instantiated
       (namelessly) in a package instantiation *)
    let cursor, ctx =
      let typ_inst = Type.canon typ_inst in
      match (cursor, typ_inst) with
      | Global, PackageT _ ->
          let cursor = Ctx.Block in
          let ctx = Ctx.set_blockkind Ctx.Package ctx in
          (cursor, ctx)
      | _ -> (cursor, ctx)
    in
    let args_il_typed = type_args cursor ctx args in
    List.split args_il_typed
  in
  check_instantiation_arity var_inst cparams args_il;
  let cparams, typ_args, args_il =
    align_cparams_with_args cparams typ_args args_il
  in
  let args_il_typed = List.combine args_il typ_args in
  let ct, targs_il, cparams, typ_inst =
    match tids_fresh with
    | [] -> (ct, targs_il, cparams, typ_inst)
    | _ ->
        let theta = infer_targs tids_fresh cparams args_il_typed in
        let targs_il =
          targs_il
          @ (List.map (fun tid_fresh -> TIdMap.find tid_fresh theta) tids_fresh
            |> List.map (fun typ -> typ $ no_info))
        in
        let targs_il =
          let tparams, _, _, _ = cd in
          List.filteri (fun i _ -> i < List.length tparams) targs_il
        in
        let ct = ConsType.subst theta ct in
        let cparams =
          List.map (Runtime_static.Tdomain.Subst.subst_cparam theta) cparams
        in
        let typ_inst = Type.subst theta typ_inst in
        (ct, targs_il, cparams, typ_inst)
  in
  WF.check_valid_constyp cursor ctx ct;
  check_instantiation_site cursor ctx typ_inst;
  let args_il =
    type_call_convention ~action:false cursor ctx cparams args_il_typed
  in
  let typ = typ_inst in
  let expr_il = Il.Ast.InstE { var_inst; targs = targs_il; args = args_il } in
  let ctk = Static.ctk_expr cursor ctx expr_il in
  (typ, ctk, expr_il)

and type_instantiation_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Type.t * Ctk.t * Il.Ast.expr' =
  let typ, ctk, expr_il = type_instantiation cursor ctx var_inst targs args in
  check
    (match Type.canon typ with
    | ExternT (_, fdenv_extern) ->
        not
          (FDEnv.exists
             (fun _ (fd : FuncDef.t) ->
               match fd with
               | PolyFD (_, _, ExternAbstractMethodT _) -> true
               | _ -> false)
             fdenv_extern)
    | _ -> true)
    "(type_instantiation_expr) cannot instantiate an abstract extern object at \
     expression level";
  (typ, ctk, expr_il)

(* Argument typing *)

and type_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : El.Ast.arg) :
    Il.Ast.arg * Type.t =
  try
    let arg_il, typ = type_arg' cursor ctx arg.it in
    (arg_il $ arg.at, typ)
  with CheckErr _ as err -> error_pass_info arg.at err

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
  | NameA (id, None) -> (Lang.Ast.NameA (id, None), AnyT)
  | AnyA -> (Lang.Ast.AnyA, AnyT)

and type_args (cursor : Ctx.cursor) (ctx : Ctx.t) (args : El.Ast.arg list) :
    (Il.Ast.arg * Type.t) list =
  List.map (type_arg cursor ctx) args

(* Statement typing *)

let rec type_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmt : El.Ast.stmt) : Ctx.t * Flow.t * Il.Ast.stmt =
  try
    let ctx, flow, stmt_il = type_stmt' cursor ctx flow stmt.it in
    (ctx, flow, stmt_il $ stmt.at)
  with CheckErr _ as err -> error_pass_info stmt.at err

and type_stmt' (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmt : El.Ast.stmt') : Ctx.t * Flow.t * Il.Ast.stmt' =
  check (cursor = Ctx.Local) "Statements must be local";
  match stmt with
  | EmptyS -> (ctx, flow, Il.Ast.EmptyS)
  | AssignS { expr_l; expr_r } -> type_assign_stmt cursor ctx flow expr_l expr_r
  | SwitchS { expr_switch; cases } ->
      type_switch_stmt cursor ctx flow expr_switch cases
  | IfS { expr_cond; stmt_then; stmt_else } ->
      type_if_stmt cursor ctx flow expr_cond stmt_then stmt_else
  | BlockS { block } -> type_block_stmt cursor ctx flow block
  | ExitS -> (ctx, flow, Il.Ast.ExitS)
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
  let stmt_il = Il.Ast.AssignS { expr_l = expr_l_il; expr_r = expr_r_il } in
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
  check
    (cursor = Ctx.Local && ctx.local.kind = Ctx.ControlApplyMethod)
    "(type_switch_stmt) switch statements can only be used within a control \
     apply block";
  let expr_switch_il = type_expr cursor ctx expr_switch in
  let typ_switch = expr_switch_il.note.typ in
  match typ_switch with
  | TableEnumT (id, _) ->
      let id_table = String.sub id 12 (String.length id - 12) in
      let id_table = String.sub id_table 0 (String.length id_table - 1) in
      type_switch_table_stmt cursor ctx flow expr_switch_il id_table cases
  | _ -> type_switch_general_stmt cursor ctx flow expr_switch_il cases

(* (12.7.1) Switch statement with action_run expression

   For this variant of switch statement, the expression must be of the form
   t.apply().action_run, where t is the name of a table (see Section 14.2.2).
   All switch labels must be names of actions of the table t, or default.

   Note that the default label of the switch statement is used to match
   on the kind of action executed, no matter whether there was a table hit or miss.
   The default label does not indicate that the table missed and
   the default_action was executed. *)

and type_switch_table_label (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id_table : Il.Ast.id') (label : El.Ast.switch_label) : Il.Ast.switch_label
    =
  try
    let label_il = type_switch_table_label' cursor ctx id_table label.it in
    label_il $ label.at
  with CheckErr _ as err -> error_pass_info label.at err

and type_switch_table_label' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id_table : Il.Ast.id') (label : El.Ast.switch_label') :
    Il.Ast.switch_label' =
  match label with
  | ExprL
      {
        it = VarE { var = { it = Current id_action; at = at_var; _ } };
        at = at_expr;
        _;
      } ->
      let id_enum = "action_list(" ^ id_table ^ ")" in
      let member = id_action.it in
      let id_field = id_enum ^ "." ^ member in
      let value_enum = Ctx.find_value_opt cursor id_field ctx in
      check
        (match value_enum with
        | Some (Value.TableEnumFieldV (id_enum', member'))
          when id_enum = id_enum' && member = member' ->
            true
        | _ -> false)
        (F.asprintf
           "(type_switch_table_label) action %a was not declared in table %a"
           Il.Pp.pp_id id_action Il.Pp.pp_id' id_table);
      let typ_enum, _, ctk_enum, _ = Ctx.find_stype cursor id_field ctx in
      let expr_il =
        Il.Ast.(
          VarE
            {
              var = { it = Lang.Ast.Current id_action; at = at_var; note = () };
            }
          $$ (at_expr, { typ = typ_enum; ctk = ctk_enum }))
      in
      Lang.Ast.ExprL expr_il
  | ExprL _ ->
      F.asprintf "(type_switch_table_label) switch label must be an action name"
      |> error_no_info
  | DefaultL -> Lang.Ast.DefaultL

and type_switch_table_case (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (id_table : Il.Ast.id') (case : El.Ast.switch_case) :
    Flow.t * Il.Ast.switch_label * Il.Ast.switch_case =
  match case.it with
  | MatchC (label, block) ->
      let label_il = type_switch_table_label cursor ctx id_table label in
      let _ctx, flow, block_il =
        type_block ~start:false cursor ctx flow block
      in
      let case_il = Lang.Ast.MatchC (label_il, block_il) $ case.at in
      (flow, label_il, case_il)
  | FallC label ->
      let label_il = type_switch_table_label cursor ctx id_table label in
      let case_il = Lang.Ast.FallC label_il $ case.at in
      (flow, label_il, case_il)

and type_switch_table_cases (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (id_table : Il.Ast.id') (cases : El.Ast.switch_case list) :
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

and type_switch_table_cases' (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (id_table : Il.Ast.id') (flows_case : Flow.t list)
    (labels_seen : string list) (cases_il : Il.Ast.switch_case list)
    (cases : El.Ast.switch_case list) :
    Flow.t list * string list * Il.Ast.switch_case list =
  match cases with
  | [] -> (flows_case, labels_seen, cases_il)
  | case :: cases ->
      let flow_case, label_il, case_il =
        type_switch_table_case cursor ctx flow id_table case
      in
      let label_il_str = F.asprintf "%a" Il.Pp.pp_switch_label label_il in
      check
        (not (List.mem label_il_str labels_seen))
        (F.asprintf
           "(type_switch_table_cases') label %s was used multiple times"
           label_il_str);
      check
        (implies
           (match (label_il.it : Il.Ast.switch_label') with
           | DefaultL -> true
           | _ -> false)
           (List.length cases = 0))
        (F.asprintf
           "(type_switch_table_cases') default label must be the last switch \
            label");
      let flows_case = flows_case @ [ flow_case ] in
      let labels_seen = labels_seen @ [ label_il_str ] in
      let cases_il = cases_il @ [ case_il ] in
      type_switch_table_cases' cursor ctx flow id_table flows_case labels_seen
        cases_il cases

and type_switch_table_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_switch_il : Il.Ast.expr) (id_table : Il.Ast.id')
    (cases : El.Ast.switch_case list) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let flow, cases_il = type_switch_table_cases cursor ctx flow id_table cases in
  let stmt_il =
    Il.Ast.SwitchS { expr_switch = expr_switch_il; cases = cases_il }
  in
  (ctx, flow, stmt_il)

(* (12.7.2) Switch statement with integer or enumerated type expression

   For this variant of switch statement, the expression must evaluate
   to ault with one of these types:

    - bit<W>
    - int<W>
    - enum, either with or without an underlying representation specified
    - error

   All switch labels must be expressions with compile-time known values,
   and must have a type that can be implicitly cast to the type of the
   switch expression (see Section 8.11.2). Switch labels must not begin with
   a left brace character {, to avoid ambiguity with a block statement. *)

and type_switch_general_label (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typ_switch : Type.t) (label : El.Ast.switch_label) : Il.Ast.switch_label =
  try
    let label_il = type_switch_general_label' cursor ctx typ_switch label.it in
    label_il $ label.at
  with CheckErr _ as err -> error_pass_info label.at err

and type_switch_general_label' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (typ_switch : Type.t) (label : El.Ast.switch_label') : Il.Ast.switch_label'
    =
  match label with
  | ExprL expr ->
      let expr_il = type_expr cursor ctx expr in
      let expr_il = coerce_type_assign expr_il typ_switch in
      Static.check_lctk expr_il;
      Lang.Ast.ExprL expr_il
  | DefaultL -> Lang.Ast.DefaultL

and type_switch_general_case (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (typ_switch : Type.t) (case : El.Ast.switch_case) :
    Flow.t * Il.Ast.switch_label * Il.Ast.switch_case =
  match case.it with
  | MatchC (label, block) ->
      let label_il = type_switch_general_label cursor ctx typ_switch label in
      let _ctx, flow, block_il =
        type_block ~start:false cursor ctx flow block
      in
      let case_il = Lang.Ast.MatchC (label_il, block_il) $ case.at in
      (flow, label_il, case_il)
  | FallC label ->
      let label_il = type_switch_general_label cursor ctx typ_switch label in
      let case_il = Lang.Ast.FallC label_il $ case.at in
      (flow, label_il, case_il)

and type_switch_general_cases (cursor : Ctx.cursor) (ctx : Ctx.t)
    (flow : Flow.t) (typ_switch : Type.t) (cases : El.Ast.switch_case list) :
    Flow.t * Il.Ast.switch_case list =
  let flows_case, _, cases_il =
    type_switch_general_cases' cursor ctx flow typ_switch [] [] [] cases
  in
  let flow =
    if List.for_all (fun flow_case -> flow_case = Flow.Ret) flows_case then
      Flow.Ret
    else Flow.Cont
  in
  (flow, cases_il)

and type_switch_general_cases' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (flow : Flow.t) (typ_switch : Type.t) (flows_case : Flow.t list)
    (labels_seen : string list) (cases_il : Il.Ast.switch_case list)
    (cases : El.Ast.switch_case list) :
    Flow.t list * string list * Il.Ast.switch_case list =
  match cases with
  | [] -> (flows_case, labels_seen, cases_il)
  | case :: cases ->
      let flow_case, label_il, case_il =
        type_switch_general_case cursor ctx flow typ_switch case
      in
      let label_il_str = F.asprintf "%a" Il.Pp.pp_switch_label label_il in
      check
        (not (List.mem label_il_str labels_seen))
        (F.asprintf
           "(type_switch_table_cases') label %s was used multiple times"
           label_il_str);
      check
        (implies
           (match (label_il.it : Il.Ast.switch_label') with
           | DefaultL -> true
           | _ -> false)
           (List.length cases = 0))
        (F.asprintf
           "(type_switch_table_cases') default label must be the last switch \
            label");
      let flows_case = flows_case @ [ flow_case ] in
      let labels_seen = labels_seen @ [ label_il_str ] in
      let cases_il = cases_il @ [ case_il ] in
      type_switch_general_cases' cursor ctx flow typ_switch flows_case
        labels_seen cases_il cases

and type_switch_general_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_switch_il : Il.Ast.expr) (cases : El.Ast.switch_case list) :
    Ctx.t * Flow.t * Il.Ast.stmt' =
  let typ_switch = expr_switch_il.note.typ in
  check
    (match Type.canon typ_switch with
    | ErrT | FIntT _ | FBitT _ | EnumT _ | SEnumT _ -> true
    | _ -> false)
    (F.asprintf
       "(type_switch_general_stmt) switch expression is unsupported for type %a"
       (Type.pp ~level:0) typ_switch);
  let flow, cases_il =
    type_switch_general_cases cursor ctx flow typ_switch cases
  in
  let stmt_il =
    Il.Ast.SwitchS { expr_switch = expr_switch_il; cases = cases_il }
  in
  (ctx, flow, stmt_il)

(* (12.6) Conditional statement

   However, the condition expression in P4 is required to be a Boolean
   (and not an integer). *)

and type_if_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_cond : El.Ast.expr) (stmt_then : El.Ast.stmt)
    (stmt_else : El.Ast.stmt) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let expr_cond_il = type_expr cursor ctx expr_cond in
  let typ_cond = expr_cond_il.note.typ in
  check (typ_cond = Types.BoolT)
    (F.asprintf "(type_if_stmt) condition %a must be a boolean"
       (El.Pp.pp_expr ~level:0) expr_cond);
  let _ctx', flow_then, stmt_then_il = type_stmt cursor ctx flow stmt_then in
  let _ctx', flow_else, stmt_else_il = type_stmt cursor ctx flow stmt_else in
  let stmt_il =
    Il.Ast.IfS
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

and type_block ~(start : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
    (flow : Flow.t) (block : El.Ast.block) : Ctx.t * Flow.t * Il.Ast.block =
  let stmts, annos = block.it in
  let ctx = if start then ctx else Ctx.enter_frame ctx in
  let ctx, flow, block_il = type_block' cursor ctx flow stmts annos in
  let ctx = if start then ctx else Ctx.exit_frame ctx in
  (ctx, flow, block_il $ block.at)

and type_block' (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (stmts : El.Ast.stmt list) (annos : El.Ast.anno list) :
    Ctx.t * Flow.t * Il.Ast.block' =
  let annos_il = type_annos cursor ctx annos in
  let ctx, flow, stmts_il = type_stmts cursor ctx flow stmts in
  let block_il = (stmts_il, annos_il) in
  (ctx, flow, block_il)

and type_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (block : El.Ast.block) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let ctx, flow, block_il = type_block ~start:false cursor ctx flow block in
  let stmt_il = Il.Ast.BlockS { block = block_il } in
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
  check
    (match ctx.local.kind with
    | Function _ | Action | ExternAbstractMethod _ | ControlApplyMethod -> true
    | _ -> false)
    "(type_return_stmt) return statement must be in a function, action, \
     abstract extern method, and control method";
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
        check
          (typ_ret_func = Types.VoidT)
          (F.asprintf
             "(type_return_stmt) function must return a value of type %a"
             (Type.pp ~level:0) typ_ret_func);
        None
  in
  let stmt_il = Il.Ast.RetS { expr_ret = expr_ret_il } in
  (ctx, Flow.Ret, stmt_il)

(* (8.20) Method invocations and function calls *)

(* (13.7) verify

   The verify statement provides a simple form of error handling.
   verify can only be invoked within a parser;
   it is used syntactically as if it were a function with the following signature:

    extern void verify(in bool condition, in error err); *)

and type_call_func_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (var_func : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    : Ctx.t * Flow.t * Il.Ast.stmt' =
  check
    (implies
       (match var_func.it with
       | Current { it = "verify"; _ } -> true
       | _ -> false)
       ((cursor = Ctx.Block && ctx.block.kind = Ctx.Parser)
       || (cursor = Ctx.Local && ctx.local.kind = Ctx.ParserState)))
    "(type_call_func_stmt) verify must be invoked within a parser";
  (* Find the function definition and specialize if it is generic *)
  let targs_il, tids_fresh = eval_types_with_check cursor ctx [] targs in
  let ft, tids_fresh_inserted, tparams, args_default =
    type_func cursor ctx var_func targs_il args
  in
  let tids_fresh = tids_fresh @ tids_fresh_inserted in
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, _typ =
    type_call cursor ctx tids_fresh ft tparams targs_il args args_default
  in
  let stmt_il =
    Il.Ast.CallFuncS { var_func; targs = targs_il; args = args_il }
  in
  (ctx, flow, stmt_il)

and type_call_method_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_base : El.Ast.expr) (member : El.Ast.member)
    (targs : El.Ast.targ list) (args : El.Ast.arg list) :
    Ctx.t * Flow.t * Il.Ast.stmt' =
  (* Find the function definition and specialize if it is generic *)
  let targs_il, tids_fresh = eval_types_with_check cursor ctx [] targs in
  let ft, expr_base_il, tids_fresh_inserted, tparams, args_default =
    type_method cursor ctx expr_base member targs_il args
  in
  let tids_fresh = tids_fresh @ tids_fresh_inserted in
  (* Check if the arguments match the parameters *)
  let targs_il, args_il, _typ =
    type_call cursor ctx tids_fresh ft tparams targs_il args args_default
  in
  let stmt_il =
    Il.Ast.CallMethodS
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

    - Direct type invocation in different scopes willult in different local
      instances with different fully-qualified control names.
    - In the same scope, direct type invocation willult in a different local
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
  check
    (match Type.canon typ with ControlT _ | ParserT _ -> true | _ -> false)
    "(type_call_inst_stmt) direct type invocation is only defined for a \
     control or parser";
  let var_inst, targs_il =
    match expr_il with
    | Il.Ast.InstE { var_inst; targs = targs_il; _ } -> (var_inst, targs_il)
    | _ -> assert false
  in
  let id = F.asprintf "%a" Il.Pp.pp_var var_inst $ no_info in
  let ctx' = Ctx.add_type cursor id.it typ Lang.Ast.No Ctk.CTK ctx in
  let expr_base =
    El.Ast.VarE { var = Lang.Ast.Current id $ no_info } $ no_info
  in
  let member = "apply" $ no_info in
  let _ctx', flow, stmt_il =
    type_call_method_stmt cursor ctx' flow expr_base member [] args
  in
  let args_il =
    match stmt_il with
    | Il.Ast.CallMethodS { args = args_il; _ } -> args_il
    | _ -> assert false
  in
  let stmt_il =
    Il.Ast.CallInstS
      { typ = typ $ no_info; var_inst; targs = targs_il; args = args_il }
  in
  (ctx, flow, stmt_il)

(* (13.5) Transition statements

   The last statement in a parser state is an optional transition statement,
   which transfers control to another state, possibly accept or reject. *)

(* (TODO) Consider modeling states as function instead of a value *)

and type_transition_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (expr_label : El.Ast.expr) : Ctx.t * Flow.t * Il.Ast.stmt' =
  check
    (match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
    "(type_transition_stmt) transition statement must be in a parser state";
  let expr_label_il = type_expr cursor ctx expr_label in
  let typ_label = expr_label_il.note.typ in
  check
    (match typ_label with StateT -> true | _ -> false)
    (F.asprintf "(type_transition_stmt) label %a must be a state"
       (El.Pp.pp_expr ~level:0) expr_label);
  let stmt_il = Il.Ast.TransS { expr_label = expr_label_il } in
  (ctx, flow, stmt_il)

and type_decl_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (flow : Flow.t)
    (decl : El.Ast.decl) : Ctx.t * Flow.t * Il.Ast.stmt' =
  let ctx, decl_il = type_decl cursor ctx decl in
  let stmt_il = Il.Ast.DeclS { decl = decl_il } in
  (ctx, flow, stmt_il)

(* Declaration typing *)

and type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : El.Ast.decl) :
    Ctx.t * Il.Ast.decl =
  try
    let ctx, decl_il = type_decl' cursor ctx decl.it in
    (ctx, decl_il $ decl.at)
  with CheckErr _ as err -> error_pass_info decl.at err

and type_decl' (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : El.Ast.decl') :
    Ctx.t * Il.Ast.decl' =
  match decl with
  (* Constant, variable, error, match_kind, and instance declarations *)
  | ConstD { id; typ; value; annos } ->
      type_const_decl cursor ctx id typ value annos
  | VarD { id; typ; init; annos } -> type_var_decl cursor ctx id typ init annos
  | ErrD { members } -> type_error_decl cursor ctx members
  | MatchKindD { members } -> type_match_kind_decl cursor ctx members
  | InstD { id; var_inst; targs; args; init; annos } ->
      type_instantiation_decl cursor ctx id var_inst targs args init annos
  (* Derived type declarations *)
  | StructD { id; tparams; fields; annos } ->
      type_struct_decl cursor ctx id tparams fields annos
  | HeaderD { id; tparams; fields; annos } ->
      type_header_decl cursor ctx id tparams fields annos
  | UnionD { id; tparams; fields; annos } ->
      type_union_decl cursor ctx id tparams fields annos
  | EnumD { id; members; annos } -> type_enum_decl cursor ctx id members annos
  | SEnumD { id; typ; fields; annos } ->
      type_senum_decl cursor ctx id typ fields annos
  | NewTypeD { id; typdef; annos } ->
      type_newtype_decl cursor ctx id typdef annos
  | TypeDefD { id; typdef; annos } ->
      type_typedef_decl cursor ctx id typdef annos
  (* Function declarations *)
  | ActionD { id; params; body; annos } ->
      type_action_decl cursor ctx id params body annos
  | FuncD { id; typ_ret; tparams; params; body } ->
      type_function_decl cursor ctx id tparams params typ_ret body
  | ExternFuncD { id; typ_ret; tparams; params; annos } ->
      type_extern_function_decl cursor ctx id tparams params typ_ret annos
  (* Object declarations *)
  (* Extern *)
  | ExternObjectD { id; tparams; mthds; annos } ->
      type_extern_object_decl cursor ctx id tparams mthds annos
  (* Parser *)
  | ValueSetD { id; typ; size; annos } ->
      type_value_set_decl cursor ctx id typ size annos
  | ParserTypeD { id; tparams; params; annos } ->
      type_parser_type_decl cursor ctx id tparams params annos
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      type_parser_decl cursor ctx id tparams params cparams locals states annos
  (* Control *)
  | TableD { id; table; annos } -> type_table_decl cursor ctx id table annos
  | ControlTypeD { id; tparams; params; annos } ->
      type_control_type_decl cursor ctx id tparams params annos
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      type_control_decl cursor ctx id tparams params cparams locals body annos
  (* Package *)
  | PackageTypeD { id; tparams; cparams; annos } ->
      type_package_type_decl cursor ctx id tparams cparams annos

and type_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : El.Ast.decl list) :
    Ctx.t * Il.Ast.decl list =
  List.fold_left
    (fun (ctx, decls_il) decl ->
      let ctx, decl_il = type_decl cursor ctx decl in
      (ctx, decls_il @ [ decl_il ]))
    (ctx, []) decls

(* (11.1) Constants

   The initializer expression must be a compile-time known value. *)

and type_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr : El.Ast.expr) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  let annos_il = type_annos cursor ctx annos in
  let typ_target, tids_fresh = eval_type_with_check cursor ctx typ in
  assert (tids_fresh = []);
  let expr_il = type_expr cursor ctx expr in
  let expr_il = coerce_type_assign expr_il typ_target.it in
  let value = Static.eval_expr cursor ctx expr_il in
  let ctx =
    Ctx.add_stype cursor id.it typ_target.it Lang.Ast.No Ctk.LCTK
      (Some value.it) ctx
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
   The language places fewtrictions on the types of the variables: most P4 types that can be written explicitly can be used
   (e.g., base types, struct, header, header stack, tuple). However, it is impossible to declare variables with type int,
   or with types that are only synthesized by the compiler (e.g., set) In addition, variables of
   type parser, control, package, or extern types must be declared using instantiations (see Section 11.3).

   Reading the value of a variable that has not been initialized yields an undefinedult.
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
  check (Type.is_assignable typ)
    (F.asprintf "(check_valid_var_type) type %a is not a valid variable type"
       (Type.pp ~level:0) typ)

and type_var_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr_init : El.Ast.expr option)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check
    ((cursor = Ctx.Block
     && match ctx.block.kind with Parser | Control -> true | _ -> false)
    || cursor = Ctx.Local
       &&
       match ctx.local.kind with
       | Function _ | Action | ExternAbstractMethod _ | ParserState
       | ControlApplyMethod ->
           true
       | _ -> false)
    "(type_var_decl) variables must be declared in a block statement, a parser \
     state, an action body, a control block's apply sub-block, the list of \
     local declarations in a parser or a control";
  let annos_il = type_annos cursor ctx annos in
  let typ_target, tids_fresh = eval_type_with_check cursor ctx typ in
  assert (tids_fresh = []);
  check_valid_var_type typ_target.it;
  let expr_init_il =
    match expr_init with
    | Some expr_init ->
        let expr_init_il = type_expr cursor ctx expr_init in
        let expr_init_il = coerce_type_assign expr_init_il typ_target.it in
        Some expr_init_il
    | None -> None
  in
  let ctx =
    Ctx.add_type cursor id.it typ_target.it Lang.Ast.InOut Ctk.DYN ctx
  in
  let decl_il =
    Il.Ast.VarD { id; typ = typ_target; init = expr_init_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.1.2) The error type

   All error constants are inserted into the error namespace, irrespective of the place where an error is defined.
   error is similar to an enumeration (enum) type in other languages. A program can contain multiple error declarations,
   which the compiler will merge together. It is an error to declare the same identifier multiple times. *)

and type_error_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : El.Ast.member list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_error_decl) error declarations must be global";
  let ctx =
    List.fold_left
      (fun ctx member ->
        let id = "error." ^ member.it in
        check
          (Ctx.find_value_opt cursor id ctx |> Option.is_none)
          (F.asprintf "(type_error_decl) error %s was already defined" id);
        let value = Value.ErrV member.it in
        let typ = Types.ErrT in
        Ctx.add_stype cursor id typ Lang.Ast.No Ctk.LCTK (Some value) ctx)
      ctx members
  in
  let decl_il = Il.Ast.ErrD { members } in
  (ctx, decl_il)

(* (7.1.3) The match kind type

   The match_kind type is very similar to the error type and is used to declare a set of distinct names
   that may be used in a table's key property (described in Section 14.2.1).
   All identifiers are inserted into the top-level namespace.
   It is an error to declare the same match_kind identifier multiple times.

   (TODO) Can the type system enforce the following constraint?

   The declaration of new match_kinds can only occur within model description files;
   P4 programmers cannot declare new match kinds. *)

and type_match_kind_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : El.Ast.member list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_match_kind_decl) match kind declarations must be global";
  let ctx =
    List.fold_left
      (fun ctx member ->
        let id = member.it in
        check
          (Ctx.find_value_opt cursor id ctx |> Option.is_none)
          (F.asprintf "(type_match_kind_decl) match kind %s was already defined"
             id);
        let value = Value.MatchKindV member.it in
        let typ = Types.MatchKindT in
        Ctx.add_stype cursor id typ Lang.Ast.No Ctk.LCTK (Some value) ctx)
      ctx members
  in
  let decl_il = Il.Ast.MatchKindD { members } in
  (ctx, decl_il)

(* (8.21) Constructor invocations *)

(* (10.3.1) Instantiating objects with abstract methods

   When instantiating an extern type that has abstract methods users have to supply
   implementations for all such methods. This is done using object initializers.

   The abstract methods can only use the supplied arguments or refer to values that are
   in the top-level scope. When calling another method of the same instance the this
   keyword is used to indicate the current object instance. *)

and type_instantiation_init_extern_abstract_method_decl (cursor : Ctx.cursor)
    (ctx : Ctx.t) (id : El.Ast.id) (tparams : El.Ast.tparam list)
    (params : El.Ast.param list) (typ_ret : El.Ast.typ) (body : El.Ast.block) :
    FuncDef.t * Il.Ast.decl' =
  check
    (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern)
    "(type_instantiation_init_extern_abstract_method_decl) Extern abstract \
     method initializer declarations must be in a block";
  (* Construct function layer context *)
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx in
  let typ_ret, tids_fresh = eval_type_with_check Ctx.Local ctx' typ_ret in
  assert (tids_fresh = []);
  let ctx' = Ctx.set_localkind (Ctx.ExternAbstractMethod typ_ret.it) ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il, tids_fresh = type_params Ctx.Local ctx' params in
  let ctx' = Ctx.add_params Ctx.Local params_il ctx' in
  let ctx', tparams_hidden =
    let tparams_hidden =
      List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
    in
    let ctx' = Ctx.add_tparams Ctx.Local tparams_hidden ctx' in
    (ctx', tparams_hidden)
  in
  (* Typecheck body *)
  let _ctx', flow, block_il = type_block ~start:true Ctx.Local ctx' Cont body in
  check
    (flow = Flow.Ret || typ_ret.it = Types.VoidT)
    "(type_instantiation_init_extern_abstract_method_decl) A function must \
     return a value on all possible execution paths";
  (* Create a function definition *)
  let fd =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ExternMethodT (params, typ_ret.it) in
    Types.PolyFD (tparams, tparams_hidden, ft)
  in
  let decl_il =
    Il.Ast.FuncD { id; typ_ret; tparams; params = params_il; body = block_il }
  in
  WF.check_valid_funcdef cursor ctx fd;
  (fd, decl_il)

and type_instantiation_init_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (frame_abstract : Frame.t) (fdenv_abstract : FDEnv.t) (init : El.Ast.decl) :
    Frame.t * FDEnv.t * Il.Ast.decl =
  let frame_abstract, fdenv_abstract, init_il =
    type_instantiation_init_decl' cursor ctx frame_abstract fdenv_abstract
      init.it
  in
  (frame_abstract, fdenv_abstract, init_il $ init.at)

and type_instantiation_init_decl' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (frame_abstract : Frame.t) (fdenv_abstract : FDEnv.t) (init : El.Ast.decl')
    : Frame.t * FDEnv.t * Il.Ast.decl' =
  match init with
  | InstD { id; var_inst; targs; args; init; annos } ->
      let ctx, decl_il =
        type_instantiation_decl cursor ctx id var_inst targs args init annos
      in
      let stype = Ctx.find_stype cursor id.it ctx in
      let frame_abstract = Frame.add_nodup id.it stype frame_abstract in
      (frame_abstract, fdenv_abstract, decl_il)
  | FuncD { id; typ_ret; tparams; params; body } ->
      let fid = FId.to_fid id params in
      let ctx =
        {
          ctx with
          block = { ctx.block with kind = Ctx.Extern; frame = frame_abstract };
        }
      in
      let fd, decl_il =
        type_instantiation_init_extern_abstract_method_decl Ctx.Block ctx id
          tparams params typ_ret body
      in
      let fdenv_abstract = FDEnv.add_nodup_overloaded fid fd fdenv_abstract in
      (frame_abstract, fdenv_abstract, decl_il)
  | _ ->
      F.asprintf
        "(type_instantiation_init_decl) Instantiation initializer should be an \
         instantiation or function declaration"
      |> error_no_info

and type_instantiation_init_decls (ctx : Ctx.t) (inits : El.Ast.decl list) :
    Frame.t * FDEnv.t * Il.Ast.decl list =
  List.fold_left
    (fun (frame_abstract, fdenv_abstract, inits_il) init ->
      let frame_abstract, fdenv_abstract, init_il =
        type_instantiation_init_decl Ctx.Global ctx frame_abstract
          fdenv_abstract init
      in
      (frame_abstract, fdenv_abstract, inits_il @ [ init_il ]))
    (Frame.empty, FDEnv.empty, [])
    inits

and type_instantiation_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (var_inst : El.Ast.var) (targs : El.Ast.targ list) (args : El.Ast.arg list)
    (init : El.Ast.decl list) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl'
    =
  let annos_il = type_annos cursor ctx annos in
  let typ, _, expr_il = type_instantiation cursor ctx var_inst targs args in
  let targs_il, args_il =
    match expr_il with
    | Il.Ast.InstE { targs = targs_il; args = args_il; _ } -> (targs_il, args_il)
    | _ -> assert false
  in
  (* Typecheck abstract methods defined by object initializers (for externs only) *)
  let typ, init_il =
    if init = [] then (typ, [])
    else
      let id, tparams, tparams_hidden, fdenv_extern, typs_inner =
        match typ with
        | SpecT
            ((tparams, tparams_hidden, ExternT (id, fdenv_extern)), typs_inner)
          ->
            (id, tparams, tparams_hidden, fdenv_extern, typs_inner)
        | _ ->
            F.asprintf
              "(type_instantiation_decl) initializers are only allowed for \
               extern objects"
            |> error_no_info
      in
      let ctx =
        { Ctx.empty with global = ctx.global }
        |> Ctx.add_type Ctx.Local "this" typ Lang.Ast.No Ctk.CTK
      in
      let _, fdenv_abstract, init_il = type_instantiation_init_decls ctx init in
      let fdenv_extern =
        List.fold_left
          (fun fdenv_extern (fid, fd_abstract) ->
            let fd_extern = FDEnv.find_opt fid fdenv_extern in
            let fd_extern =
              match (fd_extern : FuncDef.t option) with
              | Some
                  (PolyFD
                    ( tparams,
                      tparams_hidden,
                      ExternAbstractMethodT (params, typ_ret) )) ->
                  let ft = Types.ExternMethodT (params, typ_ret) in
                  let fd = Types.PolyFD (tparams, tparams_hidden, ft) in
                  fd
              | _ ->
                  F.asprintf
                    "(type_instantiation_decl) abstract method %a was not \
                     declared"
                    FId.pp fid
                  |> error_no_info
            in
            let fd_extern_inner =
              let theta =
                List.combine (tparams @ tparams_hidden) typs_inner
                |> TIdMap.of_list
              in
              FuncDef.subst theta fd_extern
            in
            check
              (FuncDef.eq_alpha fd_extern_inner fd_abstract)
              (F.asprintf
                 "(type_instantiation_decl) abstract method %a does not match \
                  the declared type"
                 FId.pp fid);
            let fdenv_extern = FDEnv.add fid fd_extern fdenv_extern in
            fdenv_extern)
          fdenv_extern
          (FDEnv.bindings fdenv_abstract)
      in
      List.iter
        (fun (fid, fd) ->
          match fd with
          | Types.PolyFD (_, _, ExternAbstractMethodT _) ->
              F.asprintf
                "(type_instantiation_decl) abstract method %a was not \
                 implemented"
                FId.pp fid
              |> error_no_info
          | _ -> ())
        (FDEnv.bindings fdenv_extern);
      let tdp =
        let typ_extern = Types.ExternT (id, fdenv_extern) in
        (tparams, tparams_hidden, typ_extern)
      in
      let typ = Types.SpecT (tdp, typs_inner) in
      (typ, init_il)
  in
  let ctx = Ctx.add_type cursor id.it typ Lang.Ast.No Ctk.CTK ctx in
  let decl_il =
    Il.Ast.InstD
      {
        id;
        typ = typ $ no_info;
        var_inst;
        targs = targs_il;
        args = args_il;
        init = init_il;
        annos = annos_il;
      }
  in
  (ctx, decl_il)

(* (7.2.5) Struct types

   This declaration introduces a new type with the specified name in the current scope.
   Field names have to be distinct. An empty struct (with no fields) is legal. *)

and type_struct_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_struct_decl) struct declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let members, typs, annoss =
    List.fold_left
      (fun (members, typs, annoss) (member, typ, annos) ->
        (members @ [ member ], typs @ [ typ ], annoss @ [ annos ]))
      ([], [], []) fields
  in
  let annoss_il =
    List.fold_left
      (fun annoss_il annos ->
        let annos_il = type_annos cursor ctx annos in
        annoss_il @ [ annos_il ])
      [] annoss
  in
  let typs, tids_fresh =
    let ctx = Ctx.add_tparams Ctx.Block tparams ctx in
    eval_types_with_check Ctx.Block ctx [] typs
  in
  let tparams_hidden =
    List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
  in
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let members = List.map it members in
    let typs = List.map it typs in
    let fields = List.combine members typs in
    let typ_struct = Types.StructT (id.it, fields) in
    Types.PolyD (tparams, tparams_hidden, typ_struct)
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il =
    let fields =
      List.combine members typs
      |> List.map2
           (fun annos_il (member, typ) -> (member, typ, annos_il))
           annoss_il
    in
    Il.Ast.StructD { id; tparams; tparams_hidden; fields; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.2.2) Header types *)

and type_header_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_header_decl) header declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let members, typs, annoss =
    List.fold_left
      (fun (members, typs, annoss) (member, typ, annos) ->
        (members @ [ member ], typs @ [ typ ], annoss @ [ annos ]))
      ([], [], []) fields
  in
  let annoss_il = List.map (List.map (type_anno cursor ctx)) annoss in
  let typs, tids_fresh =
    let ctx = Ctx.add_tparams Ctx.Block tparams ctx in
    eval_types_with_check Ctx.Block ctx [] typs
  in
  let tparams_hidden =
    List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
  in
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let members = List.map it members in
    let typs = List.map it typs in
    let fields = List.combine members typs in
    let typ_header = Types.HeaderT (id.it, fields) in
    Types.PolyD (tparams, tparams_hidden, typ_header)
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il =
    let fields =
      List.combine members typs
      |> List.map2
           (fun annos_il (member, typ) -> (member, typ, annos_il))
           annoss_il
    in
    Il.Ast.HeaderD { id; tparams; tparams_hidden; fields; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.2.4) Header unions *)

and type_union_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list)
    (fields : (El.Ast.member * El.Ast.typ * El.Ast.anno list) list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_union_decl) header union declarations must be global";
  let annos_il = List.map (type_anno cursor ctx) annos in
  let members, typs, annoss =
    List.fold_left
      (fun (members, typs, annoss) (member, typ, annos) ->
        (members @ [ member ], typs @ [ typ ], annoss @ [ annos ]))
      ([], [], []) fields
  in
  let annoss_il =
    List.fold_left
      (fun annoss_il annos ->
        let annos_il = type_annos cursor ctx annos in
        annoss_il @ [ annos_il ])
      [] annoss
  in
  let typs, tids_fresh =
    let ctx = Ctx.add_tparams Ctx.Block tparams ctx in
    eval_types_with_check Ctx.Block ctx [] typs
  in
  let tparams_hidden =
    List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
  in
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let members = List.map it members in
    let typs = List.map it typs in
    let fields = List.combine members typs in
    let typ_union = Types.UnionT (id.it, fields) in
    Types.PolyD (tparams, tparams_hidden, typ_union)
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il =
    let fields =
      List.combine members typs
      |> List.map2
           (fun annos_il (member, typ) -> (member, typ, annos_il))
           annoss_il
    in
    Il.Ast.UnionD { id; tparams; tparams_hidden; fields; annos = annos_il }
  in
  (ctx, decl_il)

(* (7.2.1) Enumeration types

   An enum declaration introduces a new identifier in the current scope for
   naming the created type along with its distinct constants. *)

and type_enum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (members : El.Ast.member list) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_enum_decl) enum declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let typ_enum =
    let members = List.map it members in
    Types.EnumT (id.it, members)
  in
  let td = Types.MonoD typ_enum in
  let ctx =
    List.fold_left
      (fun ctx member ->
        let value_field = Value.EnumFieldV (id.it, member.it) in
        let typ_field = typ_enum in
        let id_field = id.it ^ "." ^ member.it in
        Ctx.add_stype cursor id_field typ_field Lang.Ast.No Ctk.LCTK
          (Some value_field) ctx)
      ctx members
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il = Il.Ast.EnumD { id; members; annos = annos_il } in
  (ctx, decl_il)

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
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_senum_decl) serializable enum declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let typ, tids_fresh = eval_type_with_check cursor ctx typ in
  assert (tids_fresh = []);
  (* Temporarily add members to the senum block context,
     to allow initializers to refer to earlier members *)
  let ctx, fields =
    List.fold_left
      (fun (ctx, fields) (member, expr_field) ->
        let expr_field_il = type_expr Ctx.Block ctx expr_field in
        let expr_field_il = coerce_type_assign expr_field_il typ.it in
        let value_field = Static.eval_expr Ctx.Block ctx expr_field_il in
        let value_enum_field =
          Value.SEnumFieldV (id.it, member.it, value_field.it)
        in
        let typ_field =
          let fields =
            List.map (fun (member, value) -> (member.it, value.it)) fields
          in
          Types.SEnumT (id.it, typ.it, fields)
        in
        let ctx =
          Ctx.add_stype Ctx.Block member.it typ_field Lang.Ast.No Ctk.LCTK
            (Some value_enum_field) ctx
        in
        (ctx, fields @ [ (member, value_field) ]))
      (ctx, []) fields
  in
  (* Clear out the block context *)
  let typ_senum =
    let fields =
      List.map (fun (member, value) -> (member.it, value.it)) fields
    in
    Types.SEnumT (id.it, typ.it, fields)
  in
  let td = Types.MonoD typ_senum in
  let ctx =
    let members = List.map fst fields in
    let ctx =
      List.fold_left
        (fun ctx member ->
          let value_field = Ctx.find_value Ctx.Block member.it ctx in
          let typ_field = typ_senum in
          let id_field = id.it ^ "." ^ member.it in
          Ctx.add_stype cursor id_field typ_field Lang.Ast.No Ctk.LCTK
            (Some value_field) ctx)
        ctx members
    in
    { Ctx.empty with global = ctx.global }
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il = Il.Ast.SEnumD { id; typ; fields; annos = annos_il } in
  (ctx, decl_il)

(* (7.6) Introducing new types

   Similarly to typedef, the keyword type can be used to introduce a new type.
   While similar to typedef, the type keyword introduces a new type which is not a synonym with the original type:
   values of the original type and the newly introduced type cannot be mixed in expressions.
   Currently the types that can be created by the type keyword aretricted to one of:
   bit<>, int<>, bool, or types defined using type from such types. *)

and type_newtype_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typdef : (El.Ast.typ, El.Ast.decl) El.Ast.alt) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_newtype_decl) new type declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let typ, typdef_il =
    match typdef with
    | Left typ ->
        let typ, tids_fresh = eval_type_with_check cursor ctx typ in
        assert (tids_fresh = []);
        let typdef_il = Lang.Ast.Left typ in
        (typ.it, typdef_il)
    | Right decl ->
        let ctx', decl_il = type_decl cursor ctx decl in
        let tid_newtype =
          TIdSet.diff
            (TDEnv.keys ctx'.global.tdenv |> TIdSet.of_list)
            (TDEnv.keys ctx.global.tdenv |> TIdSet.of_list)
        in
        assert (TIdSet.cardinal tid_newtype = 1);
        let tid_newtype = TIdSet.choose tid_newtype in
        let td_newtype = Ctx.find_typdef cursor tid_newtype ctx' in
        let typ =
          match td_newtype with
          | MonoD typ -> typ
          | PolyD td_poly -> Types.SpecT (td_poly, [])
        in
        let typdef_il = Lang.Ast.Right decl_il in
        (typ, typdef_il)
  in
  let td =
    let typ_new = Types.NewT (id.it, typ) in
    Types.MonoD typ_new
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il = Il.Ast.NewTypeD { id; typdef = typdef_il; annos = annos_il } in
  (ctx, decl_il)

(* (7.5) typedef

   A typedef declaration can be used to give an alternative name to a type.
   The two types are treated as synonyms, and all operations that can be executed using
   the original type can be also executed using the newly created type.
   If typedef is used with a generic type the type must be specialized with the suitable number of type arguments: *)

and type_typedef_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typdef : (El.Ast.typ, El.Ast.decl) El.Ast.alt) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_typedef_decl) typedef declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let typ, typdef_il =
    match typdef with
    | Left typ ->
        let typ, tids_fresh = eval_type_with_check cursor ctx typ in
        assert (tids_fresh = []);
        let typdef_il = Lang.Ast.Left typ in
        (typ.it, typdef_il)
    | Right decl ->
        let ctx', decl_il = type_decl cursor ctx decl in
        let tid_typedef =
          TIdSet.diff
            (TDEnv.keys ctx'.global.tdenv |> TIdSet.of_list)
            (TDEnv.keys ctx.global.tdenv |> TIdSet.of_list)
        in
        assert (TIdSet.cardinal tid_typedef = 1);
        let tid_typedef = TIdSet.choose tid_typedef in
        let td_typedef = Ctx.find_typdef cursor tid_typedef ctx' in
        let typ =
          match td_typedef with
          | MonoD typ -> typ
          | PolyD td_poly -> Types.SpecT (td_poly, [])
        in
        let typdef_il = Lang.Ast.Right decl_il in
        (typ, typdef_il)
  in
  let td =
    let typ_def = Types.DefT typ in
    Types.MonoD typ_def
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il = Il.Ast.TypeDefD { id; typdef = typdef_il; annos = annos_il } in
  (ctx, decl_il)

(* (14.1) Actions

   Actions are code fragments that can read and write the data being processed.
   Actions may contain data values taht can be written by the control plane and read by the data plane.

   Syntactically actionsemble functions with no return value.
   Actions may be declared within a control block: in this case they can only be used within
   instances of that control block.

   Action parameters may not have extern types. Action parameters that have no direction
   (e.g., port in the previous example) indicate "action data." All such parameters must appear
   at the end of the parameter list. When used in a match-action table (see Section 14.2.1.2), these
   parameters will be provided by the table entries (e.g., as specified by the control plane, the
   default_action table property, or the entries table property).

   The body of an action consists of a sequence of statements and declarations. No table, control, or parser
   applications can appear within actions.

   Some targets may impose additionaltrictions on action bodies-e.g., only allowing straight-line
   code, with no conditional statements or expressions. *)

and type_action_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (params : El.Ast.param list) (body : El.Ast.block)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check
    (cursor = Ctx.Global || (cursor = Ctx.Block && ctx.block.kind = Ctx.Control))
    "(type_action_decl) action declarations must be global or in a control \
     block";
  let annos_il = type_annos cursor ctx annos in
  let fid = FId.to_fid id params in
  (* Construct action layer context *)
  let ctx' = Ctx.set_localkind Ctx.Action ctx in
  (* Typecheck and add parameters to the local context *)
  let params_il, tids_fresh = type_params cursor ctx' params in
  assert (tids_fresh = []);
  let ctx' = Ctx.add_params Ctx.Local params_il ctx' in
  (* Typecheck body *)
  let _ctx', _flow, block_il =
    type_block ~start:true Ctx.Local ctx' Cont body
  in
  (* Create an action definition *)
  let fd =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ActionT params in
    Types.MonoFD ft
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef_non_overload cursor fid fd ctx in
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

and type_function_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (body : El.Ast.block) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_function_decl) function declarations must be global";
  let fid = FId.to_fid id params in
  (* Construct function layer context *)
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx in
  let typ_ret, tids_fresh = eval_type_with_check Ctx.Local ctx' typ_ret in
  assert (tids_fresh = []);
  let ctx' = Ctx.set_localkind (Ctx.Function typ_ret.it) ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il, tids_fresh = type_params Ctx.Local ctx' params in
  let ctx' = Ctx.add_params Ctx.Local params_il ctx' in
  let ctx', tparams_hidden =
    let tparams_hidden =
      List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
    in
    let ctx' = Ctx.add_tparams Ctx.Local tparams_hidden ctx' in
    (ctx', tparams_hidden)
  in
  (* Typecheck body *)
  let _ctx', flow, block_il = type_block ~start:true Ctx.Local ctx' Cont body in
  check
    (flow = Flow.Ret || typ_ret.it = Types.VoidT)
    "(type_function_decl) a function must return a value on all possible \
     execution paths";
  (* Create a function definition *)
  let fd =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.FunctionT (params, typ_ret.it) in
    Types.PolyFD (tparams, tparams_hidden, ft)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef_overload cursor fid fd ctx in
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
  check (cursor = Ctx.Global)
    "(type_extern_function_decl) extern function declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  let fid = FId.to_fid id params in
  (* Construct extern function layer context *)
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx in
  let typ_ret, tids_fresh = eval_type_with_check Ctx.Local ctx' typ_ret in
  assert (tids_fresh = []);
  let ctx' = Ctx.set_localkind Ctx.ExternFunction ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il, tids_fresh = type_params Ctx.Local ctx' params in
  (* Create an extern function definition *)
  let fd =
    let tparams = List.map it tparams in
    let tparams_hidden = tids_fresh in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ExternFunctionT (params, typ_ret.it) in
    Types.PolyFD (tparams, tparams_hidden, ft)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef_overload cursor fid fd ctx in
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

and type_extern_constructor_mthd (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (tparams : El.Ast.tparam list)
    (cparams : El.Ast.cparam list) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.mthd' =
  assert (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern);
  let annos_il = type_annos cursor ctx annos in
  let cid = FId.to_fid id cparams in
  let cparams_il, tids_fresh = type_cparams cursor ctx cparams in
  let tdp =
    let td = Ctx.find_typdef Ctx.Global id.it ctx in
    match td with
    | Types.PolyD ((_, _, ExternT _) as tdp) -> tdp
    | _ ->
        "(type_extern_constructor_mthd) constructors are only allowed for \
         extern objects" |> error_no_info
  in
  let typ_args = List.map (fun tparam -> Types.VarT tparam.it) tparams in
  let typ = Types.SpecT (tdp, typ_args) in
  let cd =
    let tparams = List.map it tparams in
    let tparams_hidden = tids_fresh in
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    (tparams, tparams_hidden, cparams, typ)
  in
  WF.check_valid_consdef cursor ctx cd;
  let ctx = Ctx.add_consdef cid cd ctx in
  let mthd_il =
    Lang.Ast.ExternConsM { id; cparams = cparams_il; annos = annos_il }
  in
  (ctx, mthd_il)

(* (7.2.10.2) Extern objects - Abstract methods

   However, some types of extern objects may provide methods that can be implemented by the P4 programmers.
   Such methods are described with the abstract keyword prior to the method definition.
   When such an object is instantiated the user has to supply an implementation of all the abstract methods. *)

and type_extern_abstract_method_mthd (cursor : Ctx.cursor) (ctx : Ctx.t)
    (id : El.Ast.id) (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.mthd' =
  assert (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern);
  let annos_il = type_annos cursor ctx annos in
  let fid = FId.to_fid id params in
  (* Construct extern abstract method layer context *)
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx in
  let typ_ret, tids_fresh = eval_type_with_check Ctx.Local ctx' typ_ret in
  assert (tids_fresh = []);
  let ctx' = Ctx.set_localkind (Ctx.ExternAbstractMethod typ_ret.it) ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il, tids_fresh = type_params cursor ctx' params in
  (* Create an extern abstract method definition *)
  let fd =
    let tparams = List.map it tparams in
    let tparams_hidden = tids_fresh in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ExternAbstractMethodT (params, typ_ret.it) in
    Types.PolyFD (tparams, tparams_hidden, ft)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef_overload cursor fid fd ctx in
  let mthd_il =
    Lang.Ast.ExternAbstractM
      { id; tparams; params = params_il; typ_ret; annos = annos_il }
  in
  (ctx, mthd_il)

and type_extern_method_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (typ_ret : El.Ast.typ) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.mthd' =
  assert (cursor = Ctx.Block && ctx.block.kind = Ctx.Extern);
  let annos_il = type_annos cursor ctx annos in
  let fid = FId.to_fid id params in
  (* Construct extern method layer context *)
  let ctx' = Ctx.add_tparams Ctx.Local tparams ctx in
  let typ_ret, tids_fresh = eval_type_with_check Ctx.Local ctx' typ_ret in
  assert (tids_fresh = []);
  let ctx' = Ctx.set_localkind Ctx.ExternMethod ctx' in
  (* Typecheck and add parameters to the local context *)
  let params_il, tids_fresh = type_params Ctx.Local ctx' params in
  (* Create an extern method definition *)
  let fd =
    let tparams = List.map it tparams in
    let tparams_hidden = tids_fresh in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ExternMethodT (params, typ_ret.it) in
    Types.PolyFD (tparams, tparams_hidden, ft)
  in
  WF.check_valid_funcdef cursor ctx fd;
  let ctx = Ctx.add_funcdef_overload cursor fid fd ctx in
  let mthd_il =
    Lang.Ast.ExternM
      { id; tparams; params = params_il; typ_ret; annos = annos_il }
  in
  (ctx, mthd_il)

and type_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (tparams : El.Ast.tparam list)
    (mthd : El.Ast.mthd) : Ctx.t * Il.Ast.mthd =
  try
    let ctx, mthd_il = type_mthd' cursor ctx tparams mthd.it in
    (ctx, mthd_il $ mthd.at)
  with CheckErr _ as err -> error_pass_info mthd.at err

and type_mthd' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tparams : El.Ast.tparam list) (mthd : El.Ast.mthd') : Ctx.t * Il.Ast.mthd'
    =
  match mthd with
  | ExternConsM { id; cparams; annos } ->
      type_extern_constructor_mthd cursor ctx id tparams cparams annos
  | ExternAbstractM { id; tparams; params; typ_ret; annos } ->
      type_extern_abstract_method_mthd cursor ctx id tparams params typ_ret
        annos
  | ExternM { id; tparams; params; typ_ret; annos } ->
      type_extern_method_mthd cursor ctx id tparams params typ_ret annos

and type_mthds (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tparams : El.Ast.tparam list) (mthds : El.Ast.mthd list) :
    Ctx.t * Il.Ast.mthd list =
  List.fold_left
    (fun (ctx, mthds_il) mthd ->
      let ctx, mthd_il = type_mthd cursor ctx tparams mthd in
      (ctx, mthds_il @ [ mthd_il ]))
    (ctx, []) mthds

and type_extern_object_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (mthds : El.Ast.mthd list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_extern_object_decl) extern object declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  (* Separate constructors and methods *)
  let cons, mthds =
    List.partition
      (fun (mthd : El.Ast.mthd) ->
        match mthd.it with ExternConsM _ -> true | _ -> false)
      mthds
  in
  (* Check that method names do not overlap with the object name *)
  let mthds_names =
    List.map
      (fun (mthd : El.Ast.mthd) ->
        match mthd.it with
        | ExternAbstractM { id; _ } | ExternM { id; _ } -> id.it
        | _ -> assert false)
      mthds
  in
  check
    (not (List.exists (fun mthd_name -> mthd_name = id.it) mthds_names))
    "(type_extern_object_decl) method names must not overlap with the object \
     name";
  (* Typecheck methods and abstract methods
     to construct function definition environment *)
  let ctx' = Ctx.set_blockkind Ctx.Extern ctx in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let ctx', mthds_il = type_mthds Ctx.Block ctx' tparams mthds in
  (* Create an extern object type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    let typ_extern = Types.ExternT (id.it, ctx'.block.fdenv) in
    Types.PolyD (tparams, [], typ_extern)
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  (* Typecheck constructors to update constructor definition environment
     This comes after method typing to prevent recursive instantiation *)
  let ctx'' = Ctx.set_blockkind Ctx.Extern ctx in
  let ctx'' = Ctx.add_tparams Ctx.Block tparams ctx'' in
  let ctx'', cons_il = type_mthds Ctx.Block ctx'' tparams cons in
  (* Update the context with the constructor definition environment *)
  let cdenv_diff = CDEnv.diff ctx''.global.cdenv ctx.global.cdenv in
  let ctx =
    List.fold_left
      (fun ctx (cid, cd) -> Ctx.add_consdef cid cd ctx)
      ctx
      (CDEnv.bindings cdenv_diff)
  in
  let decl_il =
    Il.Ast.ExternObjectD
      { id; tparams; mthds = cons_il @ mthds_il; annos = annos_il }
  in
  (ctx, decl_il)

(* (13.11) Parser Value Sets

   Value sets are declared locally within a parser. They should be declared before
   being referenced in parser keysetExpression and can be
   used as a label in a select expression.

   Parser Value Sets support a size argument to provide hints to the compiler
   toerve hardwareources to implement the value set.

   The semantics of the size argument is similar to the size property of a table.
   If a value set has a size argument with value N, it is recommended that a compiler
   should choose a data plane implementation that is capable of storing N value set entries.
   See “Size property of P4 tables and parser value sets” P4SizeProperty for
   further discussion on the implementation of parser value set size.

   The value set is populated by the control plane by methods
   specified in the P4Runtime_static specification. *)

(* (13.6) Select expressions

   Some targets may support parser value sets; see Section 13.11.
   Given a type T for the type parameter of the value set, the type of the value set is set<T>.
   The type of the values in the set must be either bit<>, int<>, tuple, struct, or serializable enum. *)

and type_value_set_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (typ : El.Ast.typ) (expr_size : El.Ast.expr) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  check
    (cursor = Ctx.Global || (cursor = Block && ctx.block.kind = Parser))
    "(type_value_set_decl) value set declarations must be global or in a parser";
  let annos_il = type_annos cursor ctx annos in
  let typ_inner, tids_fresh = eval_type_with_check cursor ctx typ in
  assert (tids_fresh = []);
  let expr_size_il = type_expr cursor ctx expr_size in
  Static.check_ctk expr_size_il;
  let typ = Types.SetT typ_inner.it in
  WF.check_valid_typ cursor ctx typ;
  let ctx = Ctx.add_type cursor id.it typ Lang.Ast.No Ctk.CTK ctx in
  let decl_il =
    Il.Ast.ValueSetD
      { id; typ = typ_inner; size = expr_size_il; annos = annos_il }
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
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_parser_type_decl) parser type declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  (* Typecheck implicit "apply" method
     to construct function definition environment *)
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let params_il, tids_fresh = type_params Ctx.Block ctx' params in
  let tparams_hidden =
    List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
  in
  (* Create a parser type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let typ_param = Types.ParserT params in
    Types.PolyD (tparams, tparams_hidden, typ_param)
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il =
    Il.Ast.ParserTypeD
      { id; tparams; tparams_hidden; params = params_il; annos = annos_il }
  in
  (ctx, decl_il)

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
  check
    (cursor = Ctx.Local
    && ctx.block.kind = Ctx.Parser
    && match ctx.local.kind with Ctx.ParserState -> true | _ -> false)
    "(type_parser_state) Parser state must be local";
  let annos_il = type_annos cursor ctx annos in
  let ctx, flow, block_il = type_block ~start:true Ctx.Local ctx Cont block in
  assert (flow = Flow.Cont);
  let state_il = (label, block_il, annos_il) in
  (ctx, state_il)

and type_parser_states (_cursor : Ctx.cursor) (ctx : Ctx.t)
    (states : El.Ast.parser_state list) : Ctx.t * Il.Ast.parser_state list =
  let labels = List.map it states |> List.map (fun (label, _, _) -> label.it) in
  check (List.mem "start" labels)
    "(type_parser_states) a \"start\" state must exist";
  check
    ((not (List.mem "accept" labels)) && not (List.mem "reject" labels))
    "(type_parser_states) \"accpet\" and \"reject\" states are reserved";
  let labels = "accept" :: "reject" :: labels in
  WF.check_distinct_names labels;
  let ctx' =
    List.fold_left
      (fun ctx' label ->
        Ctx.add_type Ctx.Block label Types.StateT Lang.Ast.No Ctk.DYN ctx')
      ctx labels
  in
  let ctx' = Ctx.set_localkind Ctx.ParserState ctx' in
  let states_il =
    List.map (fun state -> type_parser_state Ctx.Local ctx' state |> snd) states
  in
  let decl_il = states_il in
  (ctx, decl_il)

and type_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (cparams : El.Ast.cparam list) (locals : El.Ast.decl list)
    (states : El.Ast.parser_state list) (annos : El.Ast.anno list) :
    Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_parser_decl) parser declarations must be global";
  check (tparams = [])
    "(type_parser_decl) parser declarations cannot be generic";
  let annos_il = type_annos cursor ctx annos in
  let cid = FId.to_fid id cparams in
  let ctx' = Ctx.set_blockkind Ctx.Parser ctx in
  (* Typecheck and add constructor parameters to the block context *)
  let cparams_il, tids_fresh = type_cparams cursor ctx' cparams in
  check (tids_fresh = [])
    "(type_parser_decl) a parser cannot have implicit type parameters";
  let ctx' = Ctx.add_cparams Ctx.Block cparams_il ctx' in
  (* Typecheck parser apply method *)
  let params_il, tids_fresh = type_params Ctx.Block ctx' params in
  check (tids_fresh = [])
    "(type_parser_decl) a parser cannot have implicit type parameters";
  let fd_apply =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ParserApplyMethodT params in
    Types.MonoFD ft
  in
  WF.check_valid_funcdef Ctx.Block ctx fd_apply;
  (* Add apply parameters to the block context *)
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
    let typ_parser = Types.ParserT params in
    let tdp = ([], [], typ_parser) in
    Types.SpecT (tdp, [])
  in
  let cd =
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    ([], [], cparams, typ)
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

    - default_action:
        an action to execute when the lookup in the lookup table
        fails to find a match for the key used.
    - size: an integer specifying the desired size of the table.
    - entries:
        entries that are initially added to a table when the P4 program is loaded,
        some or all of which may be unchangeable by the control plane software.
    - largest_priority_wins:
        Only useful for some tables with the entries property.
        See section 14.2.1.4 for details.
    - priority_delta:
        Only useful for some tables with the entries property.
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

and check_table_properties (table : El.Ast.table) : unit =
  let keys =
    List.filter
      (fun (table_property : El.Ast.table_property) ->
        match table_property with KeyP _ -> true | _ -> false)
      table
    |> List.length
  in
  check (keys <= 1)
    "(check_table_properties) a table should have at most one key property";
  let actions =
    List.filter
      (fun (table_property : El.Ast.table_property) ->
        match table_property with ActionP _ -> true | _ -> false)
      table
    |> List.length
  in
  check (actions = 1)
    "(check_table_properties) a table should have one action property"

and type_table_property (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_property : El.Ast.table_property) :
    Tblctx.t * Il.Ast.table_property =
  match table_property with
  | KeyP table_keys ->
      let table_ctx, table_keys_il =
        type_table_keys cursor ctx table_ctx table_keys
      in
      (table_ctx, Lang.Ast.KeyP table_keys_il)
  | ActionP table_actions ->
      let table_ctx, table_actions_il =
        type_table_actions cursor ctx table_ctx table_actions
      in
      (table_ctx, Lang.Ast.ActionP table_actions_il)
  | EntryP table_entries ->
      let table_ctx, table_entries_il =
        type_table_entries cursor ctx table_ctx table_entries
      in
      (table_ctx, Lang.Ast.EntryP table_entries_il)
  | DefaultP table_default ->
      let table_default_il =
        type_table_default cursor ctx table_ctx table_default
      in
      (table_ctx, Lang.Ast.DefaultP table_default_il)
  | CustomP table_custom ->
      let table_ctx, table_custom_il =
        type_table_custom cursor ctx table_ctx table_custom
      in
      (table_ctx, Lang.Ast.CustomP table_custom_il)

and type_table_properties (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_properties : El.Ast.table_property list) :
    Tblctx.t * Il.Ast.table =
  List.fold_left
    (fun (table_ctx, table_properties_il) table_property ->
      let table_ctx, table_property_il =
        type_table_property cursor ctx table_ctx table_property
      in
      (table_ctx, table_properties_il @ [ table_property_il ]))
    (table_ctx, []) table_properties

and type_table_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (id : El.Ast.id) : Ctx.t * Type.t =
  let id_enum = "action_list(" ^ id.it ^ ")" in
  let members =
    List.map
      (fun (var, _, _) -> F.asprintf "%a" Il.Pp.pp_var' var)
      table_ctx.actions
  in
  let typ_enum = Types.TableEnumT (id_enum, members) in
  let ctx =
    List.fold_left
      (fun ctx member ->
        let value = Value.TableEnumFieldV (id_enum, member) in
        let id_field = id_enum ^ "." ^ member in
        Ctx.add_stype cursor id_field typ_enum Lang.Ast.No Ctk.LCTK (Some value)
          ctx)
      ctx members
  in
  let id_struct = "apply_result(" ^ id.it ^ ")" in
  let typ_struct =
    Types.TableStructT
      ( id_struct,
        [
          ("hit", Types.BoolT); ("miss", Types.BoolT); ("action_run", typ_enum);
        ] )
  in
  (ctx, typ_struct)

and type_table_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (table : El.Ast.table) (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check
    (cursor = Ctx.Block && ctx.block.kind = Ctx.Control)
    "(type_table_decl) table declarations must be in a control block";
  let annos_il = type_annos cursor ctx annos in
  let table_ctx = Tblctx.empty in
  check_table_properties table;
  let table_ctx, table_il =
    let ctx = Ctx.set_localkind Ctx.TableApplyMethod ctx in
    type_table_properties Ctx.Local ctx table_ctx table
  in
  let ctx, typ_struct = type_table_type_decl cursor ctx table_ctx id in
  let typ = Types.TableT (id.it, typ_struct) in
  WF.check_valid_typ cursor ctx typ;
  let ctx = Ctx.add_type cursor id.it typ Lang.Ast.No Ctk.DYN ctx in
  let decl_il =
    Il.Ast.TableD
      { id; typ = typ $ no_info; table = table_il; annos = annos_il }
  in
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
  check
    (check_table_key' match_kind typ)
    (F.asprintf
       "(check_table_key) %a is not a valid table key type for match kind %s"
       (Type.pp ~level:0) typ match_kind)

and check_table_key' (match_kind : string) (typ : Type.t) : bool =
  let typ = Type.canon typ in
  match match_kind with
  | "exact" | "optional" -> (
      match typ with
      | DefT _ | SpecT _ -> assert false
      | ErrT | BoolT | FIntT _ | FBitT _ | VBitT _ | EnumT _ -> true
      | SEnumT (_, typ_inner, _) -> check_table_key' match_kind typ_inner
      | NewT (_, typ_inner) -> check_table_key' match_kind typ_inner
      | _ -> false)
  | "lpm" | "ternary" | "range" -> (
      match typ with
      | DefT _ | SpecT _ -> assert false
      | FIntT _ | FBitT _ -> true
      | SEnumT (_, typ_inner, _) -> check_table_key' match_kind typ_inner
      | NewT (_, typs_inner) -> check_table_key' match_kind typs_inner
      | _ -> false)
  | _ -> false

and type_table_key (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_key : El.Ast.table_key) : Tblctx.t * Il.Ast.table_key =
  let table_ctx, table_key_il =
    type_table_key' cursor ctx table_ctx table_key.it
  in
  (table_ctx, table_key_il $ table_key.at)

and type_table_key' (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_key : El.Ast.table_key') : Tblctx.t * Il.Ast.table_key' =
  let expr, match_kind, annos = table_key in
  let annos_il = type_annos cursor ctx annos in
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  WF.check_valid_typ cursor ctx (Types.SetT typ);
  let value_match_kind = Ctx.find_value_opt cursor match_kind.it ctx in
  let value_match_kind =
    match value_match_kind with
    | Some (Value.MatchKindV value_match_kind)
      when match_kind.it = value_match_kind ->
        value_match_kind
    | _ ->
        F.asprintf "(type_table_key) %a is not a valid match_kind"
          El.Pp.pp_match_kind match_kind
        |> error_no_info
  in
  check_table_key value_match_kind typ;
  let table_ctx = Tblctx.update_mode value_match_kind typ table_ctx in
  let table_key_il = (expr_il, match_kind, annos_il) in
  let table_ctx = Tblctx.add_key (typ, value_match_kind) table_ctx in
  (table_ctx, table_key_il)

and type_table_keys' (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_keys : El.Ast.table_keys') : Tblctx.t * Il.Ast.table_keys' =
  List.fold_left
    (fun (table_ctx, table_keys_il) table_key ->
      let table_ctx, table_key_il =
        type_table_key cursor ctx table_ctx table_key
      in
      (table_ctx, table_keys_il @ [ table_key_il ]))
    (table_ctx, []) table_keys

and type_table_keys (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_keys : El.Ast.table_keys) : Tblctx.t * Il.Ast.table_keys =
  try
    let table_ctx, table_keys_il =
      type_table_keys' cursor ctx table_ctx table_keys.it
    in
    (table_ctx, table_keys_il $ table_keys.at)
  with CheckErr _ as err -> error_pass_info table_keys.at err

(* (14.2.1.2) Actions

   Each action in the list of actions for a table must have a distinct name.

   Each action parameter that has a direction (in, inout, or out) must be bound in the actions list specification;
   conversely, no directionless parameters may be bound in the list.
   The expressions supplied as arguments to an action are not evaluated until the action is invoked.
   Applying tables, whether directly via an expression like table1.apply().hit, or indirectly,
   are forbidden in the expressions supplied as action arguments. *)

and type_call_action_partial (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var : El.Ast.var) (params : Types.param list)
    (args_il_typed : (Il.Ast.arg * Type.t) list) :
    Il.Ast.arg list * Il.Ast.param list * Il.Ast.param list =
  (* Rule out directionless parameters, that will be supplied by the control plane *)
  let params_specified, params_control =
    List.partition
      (fun param ->
        let _, dir, _, _ = param in
        match (dir : Lang.Ast.dir') with No -> false | _ -> true)
      params
  in
  check
    (List.length params_specified = List.length args_il_typed)
    (F.asprintf
       "(type_call_action_partial) %a expects %d arguments but %d were given"
       El.Pp.pp_var var
       (List.length params_specified)
       (List.length args_il_typed));
  let args_il =
    type_call_convention ~action:true cursor ctx params_specified args_il_typed
  in
  let params_specified_il =
    List.map
      (fun (id, dir, typ, value_default) ->
        let value_default =
          Option.map
            (fun value_default -> value_default $ no_info)
            value_default
        in
        (id $ no_info, dir $ no_info, typ $ no_info, value_default, [])
        $ no_info)
      params_specified
  in
  let params_control_il =
    List.map
      (fun (id, dir, typ, value_default) ->
        let value_default =
          Option.map
            (fun value_default -> value_default $ no_info)
            value_default
        in
        (id $ no_info, dir $ no_info, typ $ no_info, value_default, [])
        $ no_info)
      params_control
  in
  (args_il, params_specified_il, params_control_il)

and type_table_action (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_action : El.Ast.table_action) : Tblctx.t * Il.Ast.table_action =
  try
    let table_ctx, table_action_il =
      type_table_action' cursor ctx table_ctx table_action.it
    in
    (table_ctx, table_action_il $ table_action.at)
  with CheckErr _ as err -> error_pass_info table_action.at err

and type_table_action' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_action : El.Ast.table_action') :
    Tblctx.t * Il.Ast.table_action' =
  let var_action, args, annos = table_action in
  let fd =
    let args = FId.to_names args in
    Ctx.find_f_opt Ctx.find_funcdef_by_name_opt cursor var_action args ctx
  in
  check (Option.is_some fd)
    (F.asprintf "(type_table_action) there is no action named %a\n" El.Pp.pp_var
       var_action);
  let fd = Option.get fd in
  check
    (match (fd : FuncDef.t) with MonoFD (ActionT _) -> true | _ -> false)
    (F.asprintf "(type_table_action) %a is not an action" El.Pp.pp_var
       var_action);
  let params = FuncDef.get_params fd in
  let args_il_typed = type_args cursor ctx args in
  let args_il_specified = List.map fst args_il_typed in
  let args_il, params_data_il, params_control_il =
    type_call_action_partial cursor ctx var_action params args_il_typed
  in
  let annos_il = type_annos cursor ctx annos in
  let table_action_il =
    (var_action, args_il, annos_il, params_data_il, params_control_il)
  in
  let table_ctx =
    Tblctx.add_action (var_action.it, params, args_il_specified) table_ctx
  in
  (table_ctx, table_action_il)

and type_table_actions' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_actions : El.Ast.table_actions') :
    Tblctx.t * Il.Ast.table_actions' =
  List.fold_left
    (fun (table_ctx, table_actions_il) table_action ->
      let table_ctx, table_action_il =
        type_table_action cursor ctx table_ctx table_action
      in
      (table_ctx, table_actions_il @ [ table_action_il ]))
    (table_ctx, []) table_actions

and type_table_actions (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_actions : El.Ast.table_actions) :
    Tblctx.t * Il.Ast.table_actions =
  try
    let table_ctx, table_actions_il =
      type_table_actions' cursor ctx table_ctx table_actions.it
    in
    List.map it table_actions_il
    |> List.map (fun (action_name, _, _, _, _) -> action_name)
    |> WF.check_distinct_vars;
    (table_ctx, table_actions_il $ table_actions.at)
  with CheckErr _ as err -> error_pass_info table_actions.at err

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
    (var : El.Ast.var) (params : Types.param list)
    (args_il_typed : (Il.Ast.arg * Type.t) list) (args_action : Il.Ast.arg list)
    : Il.Ast.arg list =
  check
    (List.length params = List.length args_il_typed)
    (F.asprintf
       "(type_call_default_action) %a expects %d arguments but %d were given"
       El.Pp.pp_var var (List.length params)
       (List.length args_il_typed));
  let args_il_dyn =
    List.map2
      (fun param arg_il_typed ->
        let arg_il, _ = arg_il_typed in
        let _, dir, _, _ = param in
        match dir with Lang.Ast.No -> None | _ -> Some arg_il)
      params args_il_typed
    |> List.filter_map (fun x -> x)
  in
  check
    (Il.Eq.eq_args args_action args_il_dyn)
    (F.asprintf
       "(type_call_default_action) arguments %a and %a are syntactically \
        different"
       Il.Pp.pp_args args_action Il.Pp.pp_args args_il_dyn);
  type_call_convention ~action:true cursor ctx params args_il_typed

and type_table_default_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tblctx : Tblctx.t) (table_action : El.Ast.table_action) :
    Il.Ast.table_action =
  let table_action_il =
    type_table_default_action' cursor ctx tblctx table_action.it
  in
  table_action_il $ table_action.at

and type_table_default_action' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (tblctx : Tblctx.t) (table_action : El.Ast.table_action') :
    Il.Ast.table_action' =
  let var, args, annos = table_action in
  let annos_il = type_annos cursor ctx annos in
  let table_action = Tblctx.find_action tblctx var in
  check
    (Option.is_some table_action)
    (F.asprintf
       "(type_table_action_default) there is no action named %a in actions list"
       El.Pp.pp_var var);
  let params, args_action = Option.get table_action in
  let args_il_typed = type_args cursor ctx args in
  let args_il =
    type_call_default_action cursor ctx var params args_il_typed args_action
  in
  let params_data_il =
    List.map
      (fun (id, dir, typ, value_default) ->
        let value_default =
          Option.map
            (fun value_default -> value_default $ no_info)
            value_default
        in
        (id $ no_info, dir $ no_info, typ $ no_info, value_default, [])
        $ no_info)
      params
  in
  (var, args_il, annos_il, params_data_il, [])

and type_table_default' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_default : El.Ast.table_default') :
    Il.Ast.table_default' =
  let action, default_const = table_default in
  let action_il = type_table_default_action cursor ctx table_ctx action in
  (action_il, default_const)

and type_table_default (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_default : El.Ast.table_default) :
    Il.Ast.table_default =
  try
    let table_default_il =
      type_table_default' cursor ctx table_ctx table_default.it
    in
    table_default_il $ table_default.at
  with CheckErr _ as err -> error_pass_info table_default.at err

(* (14.2.1.4) Entries

   Entries cannot be specified for a table with no key (see Sec. 14.2.1.1).

   The keysetExpression component of an entry is a tuple that must provide
   a field for each key in the table keys (see Sec. 14.2.1). The table key type must match
   the type of the element of the set. The actionRef component must be an action which appears
   in the table actions list (and must not have the @defaultonly annotation), with all its arguments bound. *)

(* (14.2.1.4) Entry priorities

   If a table has fields where their match_kinds are all exact or lpm, there is no reason to
   assign numeric priorities to its entries. If they are all exact, duplicate keys are not allowed,
   and thus every lookup key can match at most one entry, so there is no need for a tiebreaker.
   If there is an lpm field, the priority of the entry corresponds to the length of the prefix,
   i.e. if a lookup key matches multiple prefixes, the longest prefix is always the winner.

   Thus there is a table property largest_priority_wins. If explicitly specified for a table,
   its value must be boolean. If true, then the priority values use the largest_priority_wins convention.
   If false, then the priority values use the smallest_priority_wins convention. If the table property
   is not present at all, then the default convention is true, corresponding to largest_priority_wins.

   In some cases, developers may wish the initial priority values to have “gaps” between their values,
   to leave room for possible later insertion of new entries between two initial entries.
   They can achieve this by explicitly specifying all priority values, of course, but as a convenience
   we define the table property priority_delta to be a positive integer value, with a default value
   of 1 if not specified for a table, to use as a default difference between the priorities of
   consecutive entries.

   There are two steps that occur at compile time for a table with the entries property
   involving entry priorities:

    - Determine the value of the priority of every entry in the entries list.
    - Issue any errors or warnings that are appropriate for these priority values.
      Warnings may be suppressed via an appropriate @noWarn annotation.

   These steps are performed independently for each table with the entries property,
   and each is described in more detail below.

   In general, if the developer specifies a priority value for an entry,
   that is the value that will be used.

   If the developer does not specify priority values for any entry, then the compiler calculates
   priority values for every entry as follows:

    // For this pseudocode, table entries in the `entries` list are
    // numbered 0 through n-1, 0 being the first to appear in order in the
    // source code.  Their priority values are named prio[0] through
    // prio[n-1].
    int p = 1;
    if (largest_priority_wins == true) {
      for (int j = n-1; j >= 0; j -= 1) {
        prio[j] = p;
        p += priority_delta;
      }
    } else {
      for (int j = 0; j < n; j += 1) {
        prio[j] = p;
        p += priority_delta;
      }
    }

   If the developer specifies priority values for at least one entry, then in order to simplify
   the rules for determining priorities of entries without one in the source code, the first entry must
   have a priority value explicitly provided. The priorities of entries that do not have one in
   the source code (if any) are determined as follows:

    // Same conventions here as in the previous block of pseudocode above.
    // If entry j has a priority value specified in the source code,
    // prio_specified[j] is true, otherwise it is false.
    assert(prio_specified[0]);  // compile time error if prio_specified[0] is false
    p = prio[0];
    for (int j = 1; j < n; j += 1) {
      if (prio_specified[j]) {
        p = prio[j];
      } else {
        if (largest_priority_wins == true) {
            p -= priority_delta;
        } else {
            p += priority_delta;
        }
        prio[j] = p;
      }
    }

   In the second step, the compiler issues errors for out of range priority values,
   and/or warnings for certain combinations of entry priorities that might be unintended by the developer,
   unless the developer explicitly disables those warnings.

   If any priority values are negative, or larger than the maximum supported value,
   that is a compile time error. *)

and type_table_entry_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_ctx_key : Type.t * Il.Ast.match_kind')
    (keyset : El.Ast.keyset) : Tblctx.state * Il.Ast.keyset =
  let entry_state, table_entry_keyset_il =
    type_table_entry_keyset' cursor ctx table_ctx table_ctx_key keyset.it
  in
  (entry_state, table_entry_keyset_il $ keyset.at)

and type_table_entry_keyset' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_ctx_key : Type.t * Il.Ast.match_kind')
    (keyset : El.Ast.keyset') : Tblctx.state * Il.Ast.keyset' =
  let typ_key, match_kind = table_ctx_key in
  match keyset with
  | ExprK expr ->
      let expr_il = type_expr cursor ctx expr in
      let entry_state =
        match (match_kind, table_ctx.mode) with
        | "lpm", NoPriLpm prefix_max -> (
            match expr_il.it with
            | MaskE { expr_mask; _ } ->
                let prefix_max = prefix_max |> Bigint.of_int in
                let value_mask = Static.eval_expr cursor ctx expr_mask in
                let mask = value_mask.it |> Value.get_num in
                let mask = Num.bit_of_raw_int mask prefix_max in
                Tblctx.get_lpm_prefix mask
            | _ -> Tblctx.Lpm prefix_max)
        | _ -> Tblctx.NoLpm
      in
      let expr_il =
        match expr_il with
        | { it = MaskE _; note = { typ; _ }; _ } -> (
            check
              (match_kind = "lpm" || match_kind = "ternary")
              (F.asprintf
                 "(type_table_entry_keyset') match_kind %s cannot use mask \
                  expression"
                 match_kind);
            match typ with
            | SetT typ_inner when Subtyp.implicit typ_inner typ_key ->
                if Type.eq_alpha typ_inner typ_key then expr_il
                else insert_cast expr_il (Types.SetT typ_key)
            | _ ->
                F.asprintf
                  "(type_table_entry_keyset') keyset expression should have \
                   type %a, but got %a"
                  (Type.pp ~level:0) typ_key (Type.pp ~level:0) typ
                |> error_no_info)
        | { it = RangeE _; note = { typ; _ }; _ } -> (
            check (match_kind = "range")
              (F.asprintf
                 "(type_table_entry_keyset') match_kind %s cannot use range \
                  expression"
                 match_kind);
            match typ with
            | SetT typ_inner when Subtyp.implicit typ_inner typ_key ->
                if Type.eq_alpha typ_inner typ_key then expr_il
                else insert_cast expr_il (Types.SetT typ_key)
            | _ ->
                F.asprintf
                  "(type_table_entry_keyset') keyset expression should have \
                   type %a, but got %a"
                  (Type.pp ~level:0) typ_key (Type.pp ~level:0) typ
                |> error_no_info)
        | { it = _; note = { typ; _ }; _ } ->
            check
              (Subtyp.implicit typ typ_key)
              (F.asprintf
                 "(type_table_entry_keyset') keyset expression should have \
                  type %a, but got %a"
                 (Type.pp ~level:0) typ_key (Type.pp ~level:0) typ);
            insert_cast expr_il (Types.SetT typ_key)
      in
      (entry_state, Lang.Ast.ExprK expr_il)
  | DefaultK ->
      check (match_kind <> "exact")
        "(type_table_entry_keyset') exact match does not allow default \
         expression";
      let entry_state =
        match (match_kind, table_ctx.mode) with
        | "lpm", NoPriLpm prefix_max -> Tblctx.Lpm prefix_max
        | _ -> Tblctx.NoLpm
      in
      (entry_state, Lang.Ast.DefaultK)
  | AnyK ->
      check (match_kind <> "exact")
        "(type_table_entry_keyset') exact match does not allow wildcard \
         expression";
      let entry_state =
        match (match_kind, table_ctx.mode) with
        | "lpm", NoPriLpm _ -> Tblctx.Lpm 0
        | _ -> Tblctx.NoLpm
      in
      (entry_state, Lang.Ast.AnyK)

and type_table_entry_keysets (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (keysets : El.Ast.keyset list) :
    Tblctx.state * Il.Ast.keyset list =
  match (table_ctx.keys, keysets) with
  (* (TODO) Check if this logic is redundant *)
  | _, [ { it = DefaultK; at; note } ] ->
      let keyset_il = Lang.Ast.DefaultK $$ at % note in
      let entry_state =
        match table_ctx.mode with
        | NoPriLpm prefix_max -> Tblctx.Lpm prefix_max
        | _ -> Tblctx.NoLpm
      in
      (entry_state, [ keyset_il ])
  | _, [ { it = AnyK; at; note } ] ->
      let keyset_il = Lang.Ast.AnyK $$ at % note in
      let entry_state =
        match table_ctx.mode with
        | NoPriLpm _ -> Tblctx.Lpm 0
        | _ -> Tblctx.NoLpm
      in
      (entry_state, [ keyset_il ])
  | table_ctx_keys, keysets ->
      check
        (List.length table_ctx_keys = List.length keysets)
        "(type_table_entry_keysets) number of select keys must match the \
         number of cases";
      List.fold_left
        (fun (entry_state, table_entry_keysets_il) keyset ->
          let table_ctx_key, table_entry_keyset = keyset in
          let entry_state_curr, table_entry_keyset_il =
            type_table_entry_keyset cursor ctx table_ctx table_ctx_key
              table_entry_keyset
          in
          let entry_state = Tblctx.update_state entry_state entry_state_curr in
          (entry_state, table_entry_keysets_il @ [ table_entry_keyset_il ]))
        (Tblctx.NoLpm, [])
        (List.combine table_ctx_keys keysets)

and type_call_entry_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (var : El.Ast.var) (params : Types.param list)
    (args_il_typed : (Il.Ast.arg * Type.t) list) (args_action : Il.Ast.arg list)
    : Il.Ast.arg list =
  type_call_default_action cursor ctx var params args_il_typed args_action

and type_table_entry_action (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_action : El.Ast.table_action) :
    Il.Ast.table_action =
  let table_action_il =
    type_table_entry_action' cursor ctx table_ctx table_action.it
  in
  table_action_il $ table_action.at

and type_table_entry_action' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_action : El.Ast.table_action') :
    Il.Ast.table_action' =
  let var, args, annos = table_action in
  let annos_il = type_annos cursor ctx annos in
  let table_action = Tblctx.find_action table_ctx var in
  check
    (Option.is_some table_action)
    (F.asprintf
       "(type_table_action_entry) there is no action named %a in actions list"
       El.Pp.pp_var var);
  let params, args_action = Option.get table_action in
  let args_il_typed = type_args cursor ctx args in
  let args_il =
    type_call_entry_action cursor ctx var params args_il_typed args_action
  in
  let params_data_il =
    List.map
      (fun (id, dir, typ, value_default) ->
        let value_default =
          Option.map
            (fun value_default -> value_default $ no_info)
            value_default
        in
        (id $ no_info, dir $ no_info, typ $ no_info, value_default, [])
        $ no_info)
      params
  in
  (var, args_il, annos_il, params_data_il, [])

and check_table_entry_priority (table_ctx : Tblctx.t) (priority_curr : int) :
    unit =
  check (priority_curr >= 0)
    "(check_table_entry_priority) priority must not be negative";
  if table_ctx.priorities.values = [] then ()
  else
    let largest_wins = table_ctx.priorities.largest_wins in
    let priority_prev = Tblctx.find_last_priority table_ctx in
    if
      (largest_wins && priority_curr > priority_prev)
      || ((not largest_wins) && priority_curr < priority_prev)
    then
      F.eprintf
        "(check_table_entry_priority) Warning: entries_out_of_priority_order\n";
    if List.mem priority_curr table_ctx.priorities.values then
      F.eprintf "(check_table_entry_priority) Warning: Duplicate priority %d\n"
        priority_curr;
    ()

and type_table_entry_priority (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (entry_state : Tblctx.state)
    (priority : El.Ast.expr option) : Tblctx.t * Il.Ast.value option =
  check
    (implies table_ctx.entries.const (Option.is_none priority))
    "(type_table_entry_priority) cannot define priority within constant entries";
  match table_ctx.mode with
  | NoPri ->
      check (Option.is_none priority)
        "(type_table_entry_priority) cannot define priority when there are \
         only exact fields";
      (table_ctx, None)
  | NoPriLpm _ ->
      check (Option.is_none priority)
        "(type_table_entry_priority) cannot define priority when there are \
         only lpm fields";
      let value_prefix =
        match entry_state with
        | Lpm prefix -> Bigint.of_int prefix
        | _ -> assert false
      in
      let priority_il = Value.IntV value_prefix $ no_info |> Option.some in
      (table_ctx, priority_il)
  (* Neglect lpm prefix when lpm is used with explicit priority for other match kinds *)
  | _ when table_ctx.priorities.values = [] ->
      let value_priority =
        match priority with
        | Some priority ->
            let priority_il = type_expr cursor ctx priority in
            let value_priority = Static.eval_expr cursor ctx priority_il in
            let priority = value_priority.it |> Value.get_num in
            priority
        | None ->
            let largest_wins = table_ctx.priorities.largest_wins in
            let delta = table_ctx.priorities.delta in
            let size = table_ctx.entries.size in
            let priority =
              if largest_wins then 1 + ((size - 1) * delta) |> Bigint.of_int
              else Bigint.one
            in
            priority
      in
      let priority_il = Value.IntV value_priority $ no_info |> Option.some in
      let value_priority = value_priority |> Bigint.to_int_exn in
      check_table_entry_priority table_ctx value_priority;
      let table_ctx =
        if Option.is_some priority then Tblctx.set_priority_init true table_ctx
        else table_ctx
      in
      let table_ctx = Tblctx.add_priority value_priority table_ctx in
      (table_ctx, priority_il)
  | _ ->
      check
        (implies (Option.is_some priority) table_ctx.priorities.init)
        "(type_table_entry_priority) the priority of the first entry must be \
         defined if priorities are explicitly specified";
      let value_priority =
        match priority with
        | Some priority ->
            let priority_il = type_expr cursor ctx priority in
            let value_priority = Static.eval_expr cursor ctx priority_il in
            let priority = value_priority.it |> Value.get_num in
            priority
        | None ->
            let largest_wins = table_ctx.priorities.largest_wins in
            let delta = table_ctx.priorities.delta in
            let priority_prev = Tblctx.find_last_priority table_ctx in
            let priority =
              if largest_wins then priority_prev - delta |> Bigint.of_int
              else priority_prev + delta |> Bigint.of_int
            in
            priority
      in
      let priority_il = Value.IntV value_priority $ no_info |> Option.some in
      let value_priority = value_priority |> Bigint.to_int_exn in
      check_table_entry_priority table_ctx value_priority;
      let table_ctx = Tblctx.add_priority value_priority table_ctx in
      (table_ctx, priority_il)

and type_table_entry (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_entry : El.Ast.table_entry) : Tblctx.t * Il.Ast.table_entry =
  try
    let table_ctx, table_entry_il =
      type_table_entry' cursor ctx table_ctx table_entry.it
    in
    (table_ctx, table_entry_il $ table_entry.at)
  with CheckErr _ as err -> error_pass_info table_entry.at err

and type_table_entry' (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_entry : El.Ast.table_entry') : Tblctx.t * Il.Ast.table_entry' =
  let keysets, action, priority, table_entry_const, annos = table_entry in
  let annos_il = type_annos cursor ctx annos in
  let entry_state, keysets_il =
    type_table_entry_keysets cursor ctx table_ctx keysets
  in
  let action_il = type_table_entry_action cursor ctx table_ctx action in
  let table_ctx, priority_il =
    type_table_entry_priority cursor ctx table_ctx entry_state priority
  in
  let table_entry_il =
    (keysets_il, action_il, priority_il, table_entry_const, annos_il)
  in
  (table_ctx, table_entry_il)

and type_table_entries' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_entries : El.Ast.table_entries') :
    Tblctx.t * Il.Ast.table_entries' =
  let table_entries, table_entries_const = table_entries in
  check
    (implies (table_ctx.keys = []) (table_entries = []))
    "(type_table_entries') entries cannot be specified for a table with no key";
  let table_ctx =
    let entries_size = List.length table_entries in
    Tblctx.set_entries_size entries_size table_ctx
  in
  let table_ctx = Tblctx.set_entries_const table_entries_const table_ctx in
  let table_ctx, table_entries_il =
    List.fold_left
      (fun (table_ctx, table_entries_il) table_entry ->
        let table_ctx, table_entry_il =
          type_table_entry cursor ctx table_ctx table_entry
        in
        (table_ctx, table_entries_il @ [ table_entry_il ]))
      (table_ctx, []) table_entries
  in
  (table_ctx, (table_entries_il, table_entries_const))

and type_table_entries (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_entries : El.Ast.table_entries) :
    Tblctx.t * Il.Ast.table_entries =
  try
    let table_ctx, table_entries_il =
      type_table_entries' cursor ctx table_ctx table_entries.it
    in
    (table_ctx, table_entries_il $ table_entries.at)
  with CheckErr _ as err -> error_pass_info table_entries.at err

(* (14.2.1.5) Size

   The size is an optional property of a table. When present, its value must always be
   an integer compile-time known value. It is specified in units of number of table entries.

   (14.2.1.6) Additional properties

   A table declaration defines its essential control and data plane interfaces—i.e., keys and actions.
   However, the best way to implement a table may actually depend on the nature of the entries
   that will be installed at runtime (for example, tables could be dense or sparse, could be implemented as
   hash-tables, associative memories, tries, etc.)

   However, these architecture-specific properties may not change the semantics of table lookups,
   which always produce either a hit and an action or a miss—they can only change how thoseults are
   interpreted on the state of the data plane. Thistriction is needed to ensure that it is possible to
   reason about the behavior of tables during compilation. *)

and type_table_custom (cursor : Ctx.cursor) (ctx : Ctx.t) (table_ctx : Tblctx.t)
    (table_custom : El.Ast.table_custom) : Tblctx.t * Il.Ast.table_custom =
  try
    let table_ctx, table_custom_il =
      type_table_custom' cursor ctx table_ctx table_custom.it
    in
    (table_ctx, table_custom_il $ table_custom.at)
  with CheckErr _ as err -> error_pass_info table_custom.at err

and type_table_custom' (cursor : Ctx.cursor) (ctx : Ctx.t)
    (table_ctx : Tblctx.t) (table_custom : El.Ast.table_custom') :
    Tblctx.t * Il.Ast.table_custom' =
  let member, expr, custom_const, annos = table_custom in
  let annos_il = type_annos cursor ctx annos in
  let expr_il = type_expr cursor ctx expr in
  let typ = expr_il.note.typ in
  let table_ctx =
    match member.it with
    | "size" ->
        check (Type.is_numeric typ)
          (F.asprintf
             "(type_table_custom) size should be a numeric type, not %a"
             (Type.pp ~level:0) typ);
        table_ctx
    | "largest_priority_wins" ->
        check (typ = BoolT)
          (F.asprintf
             "(type_table_custom) largest_priority_wins should be a boolean \
              type, not %a"
             (Type.pp ~level:0) typ);
        let value = Static.eval_expr cursor ctx expr_il in
        let largest_priority_wins = value.it |> Value.get_bool in
        let table_ctx =
          Tblctx.set_largest_priority_wins largest_priority_wins table_ctx
        in
        table_ctx
    | "priority_delta" ->
        check (Type.is_numeric typ)
          (F.asprintf
             "(type_table_custom) priority_delta should be a numeric type, not \
              %a"
             (Type.pp ~level:0) typ);
        let value = Static.eval_expr cursor ctx expr_il in
        let priority_delta = value.it |> Value.get_num |> Bigint.to_int_exn in
        check (priority_delta > 0)
          (F.asprintf
             "(type_table_custom) priority_delta should be a positive integer, \
              not %d"
             priority_delta);
        let table_ctx = Tblctx.set_priority_delta priority_delta table_ctx in
        table_ctx
    (* (TODO) Maybe define architecture-specific custom table element typing/validation *)
    | _ -> table_ctx
  in
  (table_ctx, (member, expr_il, custom_const, annos_il))

(* (7.2.12.2) Control type declarations *)

and type_control_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (params : El.Ast.param list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_control_type_decl) Control type declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  (* Typecheck implicit "apply" method
     to construct function definition environment *)
  let ctx' = Ctx.set_blockkind Ctx.Control ctx in
  let ctx' = Ctx.add_tparams Ctx.Block tparams ctx' in
  let params_il, tids_fresh = type_params Ctx.Local ctx' params in
  let tparams_hidden =
    List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
  in
  (* Create a control type definition
     and add it to the context *)
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let typ_control = Types.ControlT params in
    Types.PolyD (tparams, tparams_hidden, typ_control)
  in
  WF.check_valid_typdef cursor ctx td;
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let decl_il =
    Il.Ast.ControlTypeD
      { id; tparams; tparams_hidden; params = params_il; annos = annos_il }
  in
  (ctx, decl_il)

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
  check (cursor = Ctx.Global)
    "(type_control_decl) Control declarations must be global";
  check (tparams = [])
    "(type_control_decl) Control declarations cannot be generic";
  let annos_il = type_annos cursor ctx annos in
  let cid = FId.to_fid id cparams in
  let ctx' = Ctx.set_blockkind Ctx.Control ctx in
  (* Typecheck and add constructor parameters to the block context *)
  let cparams_il, tids_fresh = type_cparams Ctx.Block ctx' cparams in
  check (tids_fresh = [])
    "(type_control_decl) a control cannot have implicit type parameters";
  let ctx' = Ctx.add_cparams Ctx.Block cparams_il ctx' in
  (* Typecheck control apply method *)
  let params_il, tids_fresh = type_params Ctx.Local ctx' params in
  check (tids_fresh = [])
    "(type_control_decl) a control cannot have implicit type parameters";
  let fd_apply =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let ft = Types.ControlApplyMethodT params in
    Types.MonoFD ft
  in
  WF.check_valid_funcdef Ctx.Block ctx fd_apply;
  (* Add apply parameters to the block context *)
  let ctx' = Ctx.add_params Ctx.Block params_il ctx' in
  (* Typecheck and add local declarations to the block context *)
  let ctx', locals_il = type_decls Ctx.Block ctx' locals in
  (* Typecheck implicit "apply" method *)
  let ctx' = Ctx.set_localkind Ctx.ControlApplyMethod ctx' in
  let _ctx', _flow, body_il = type_block ~start:true Ctx.Local ctx' Cont body in
  (* Create a control constructor definition *)
  let typ =
    let params =
      List.map it params_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    let typ_control = Types.ControlT params in
    let tdp = ([], [], typ_control) in
    Types.SpecT (tdp, [])
  in
  let cd =
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    ([], [], cparams, typ)
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
    (tparams : El.Ast.tparam list) (cparams : El.Ast.cparam list) :
    TypeDef.t * ConsDef.t * Il.Ast.tparam list * Il.Ast.cparam list =
  check
    (cursor = Ctx.Block && ctx.block.kind = Package)
    "(type_package_constructor_decl) Package constructor declaration must be \
     in a package block";
  let cparams_il, tids_fresh = type_cparams Ctx.Block ctx cparams in
  let tparams_hidden =
    List.map (fun tid_fresh -> tid_fresh $ no_info) tids_fresh
  in
  let tdp =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let typs_inner =
      List.map it cparams_il
      |> List.map (fun (_, _, typ_inner, _, _) -> typ_inner.it)
    in
    let typ_package = Types.PackageT typs_inner in
    (tparams, tparams_hidden, typ_package)
  in
  let td = Types.PolyD tdp in
  WF.check_valid_typdef cursor ctx td;
  let typ_args =
    List.map (fun tparam -> Types.VarT tparam.it) tparams
    @ List.map (fun tparam_hidden -> Types.VarT tparam_hidden.it) tparams_hidden
  in
  let typ = Types.SpecT (tdp, typ_args) in
  let cd =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let cparams =
      List.map it cparams_il
      |> List.map (fun (id, dir, typ, value_default, _) ->
             (id.it, dir.it, typ.it, Option.map it value_default))
    in
    (tparams, tparams_hidden, cparams, typ)
  in
  WF.check_valid_consdef Ctx.Block ctx cd;
  (td, cd, tparams, cparams_il)

and type_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : El.Ast.id)
    (tparams : El.Ast.tparam list) (cparams : El.Ast.cparam list)
    (annos : El.Ast.anno list) : Ctx.t * Il.Ast.decl' =
  check (cursor = Ctx.Global)
    "(type_package_type_decl) Package type declarations must be global";
  let annos_il = type_annos cursor ctx annos in
  (* Package type declaration is implicitly a constructor declaration *)
  let td, cd, tparams, cparams_il =
    let ctx = Ctx.set_blockkind Ctx.Package ctx in
    let ctx = Ctx.add_tparams Ctx.Block tparams ctx in
    type_package_constructor_decl Ctx.Block ctx tparams cparams
  in
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let ctx =
    let cid = FId.to_fid id cparams in
    Ctx.add_consdef cid cd ctx
  in
  let decl_il =
    Il.Ast.PackageTypeD { id; tparams; cparams = cparams_il; annos = annos_il }
  in
  (ctx, decl_il)

(* Entry point : Program typing *)

let type_program (program : El.Ast.program) : Il.Ast.program =
  Ctx.refresh ();
  let _ctx, program_il = type_decls Ctx.Global Ctx.empty program in
  program_il
