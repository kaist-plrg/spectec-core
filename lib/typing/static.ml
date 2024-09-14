module Ctk = Runtime.Ctk
module Value = Runtime.Value
module Types = Runtime.Types
module Type = Types.Type
module F = Format
open Util.Source

(* (18.1) Compile-time known and local compile-time known values

   Certain expressions in a P4 program have the property that their value can be
   determined at compile time. Moreover, for some of these expressions,
   their value can be determined only using information in the current scope.
   We call these compile-time known values and local compile-time known values respectively.

   The following are local compile-time known values:

    - Integer literals, Boolean literals, and string literals.
    - Identifiers declared in an error, enum, or match_kind declaration.
    - The default identifier.
    - The size field of a value with type header stack.
    - The _ identifier when used as a select expression label
    - The expression {#} representing an invalid header or header union value.
    - Instances constructed by instance declarations (Section 11.3) and constructor invocations.
    - Identifiers that represent declared types, actions,
        functions, tables, parsers, controls, or packages.
    - Tuple expression where all components are local compile-time known values.
    - Structure-valued expressions, where all fields are local compile-time known values.
    - Expressions evaluating to a list type, where all elements are local compile-time known values.
    - Legal casts applied to local compile-time known values.
    - The following expressions
        (+, -, |+|, |-|, *, / , %, !, &, |, ^, &&, ||, << , >>, ~, /, >, <, ==, !=, <=, >=, ++, [:], ?:)
        when their operands are all local compile-time known values.
    - Expressions of the form e.minSizeInBits(), e.minSizeInBytes(), e.maxSizeInBits()
        and e.maxSizeInBytes() where the type of e is not generic.

   The following are compile-time known values:

    - All local compile-time known values.
    - Constructor parameters (i.e., the declared parameters for a parser, control, etc.)
    - Identifiers declared as constants using the const keyword.
    - Tuple expression where all components are compile-time known values.
    - Expressions evaluating to a list type, where all elements are compile-time known values.
    - Structure-valued expressions, where all fields are compile-time known values.
    - Expressions evaluating to a list type, where all elements are compile-time known values.
    - Legal casts applied to compile-time known values.
    - The following expressions
        (+, -, |+|, |-|, *, / , %, cast, !, &, |, ^, &&, ||, << , >> , ~, /, >, <, ==, !=, <=, >=, ++, [:], ?:)
        when their operands are all compile-time known values.
    - Expressions of the form e.minSizeInBits(), e.minSizeInBytes(), e.maxSizeInBits()
        and e.maxSizeInBytes() where the the type of e is generic. *)

(* Tracking (1) local compile-time known (2) compile-time known (3) dynamic expressions *)

let check_lctk (expr : Il.Ast.expr) : unit =
  if not (Ctk.is_lctk expr.note.ctk) then (
    Format.eprintf
      "(check_ctk) %a is not a local compile-time known expression\n"
      (Il.Pp.pp_expr ~level:0) expr;
    assert false)
  else ()

let check_ctk (expr : Il.Ast.expr) : unit =
  if not (Ctk.is_ctk expr.note.ctk) then (
    Format.eprintf "(check_ctk) %a is not a compile-time known expression\n"
      (Il.Pp.pp_expr ~level:0) expr;
    assert false)
  else ()

let rec ctk_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : Il.Ast.expr') :
    Ctk.t =
  match expr with
  | ValueE _ -> LCTK
  | VarE { var } -> ctk_var_expr cursor ctx var
  | TupleE { exprs } -> ctk_tuple_expr exprs
  | RecordE { fields } -> ctk_record_expr fields
  | UnE { unop; expr } -> ctk_unop_expr unop expr
  | BinE { binop; expr_l; expr_r } -> ctk_binop_expr binop expr_l expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      ctk_ternop_expr expr_cond expr_then expr_else
  | CastE { typ; expr } -> ctk_cast_expr typ expr
  | MaskE _ | RangeE _ | SelectE _ | ArrAccE _ -> DYN
  | BitAccE { expr_base; value_lo; value_hi } ->
      ctk_bitstring_acc_expr expr_base value_lo value_hi
  | ExprAccE { expr_base; member } -> ctk_expr_acc_expr expr_base member
  | CallFuncE _ -> DYN
  | CallMethodE { expr_base; member; targs; args } ->
      ctk_call_method_expr expr_base member targs args
  | InstE _ -> CTK

and ctk_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : Il.Ast.var) : Ctk.t
    =
  let rtype = Ctx.find_opt Ctx.find_rtype_opt cursor var ctx in
  if Option.is_none rtype then (
    Format.eprintf "(ctk_var_expr) %a a free variable\n" Il.Pp.pp_var var;
    assert false);
  let _, _, ctk = Option.get rtype in
  ctk

and ctk_tuple_expr (exprs : Il.Ast.expr list) : Ctk.t =
  let ctks = List.map (fun expr -> Il.Ast.(expr.note.ctk)) exprs in
  Ctk.joins ctks

and ctk_record_expr (fields : (Il.Ast.member * Il.Ast.expr) list) : Ctk.t =
  let exprs = List.map snd fields in
  let ctks = List.map (fun expr -> Il.Ast.(expr.note.ctk)) exprs in
  Ctk.joins ctks

and ctk_unop_expr (_unop : Il.Ast.unop) (expr : Il.Ast.expr) : Ctk.t =
  expr.note.ctk

and ctk_binop_expr (_binop : Il.Ast.binop) (expr_l : Il.Ast.expr)
    (expr_r : Il.Ast.expr) : Ctk.t =
  let ctk_l = expr_l.note.ctk in
  let ctk_r = expr_r.note.ctk in
  Ctk.join ctk_l ctk_r

and ctk_ternop_expr (expr_cond : Il.Ast.expr) (expr_then : Il.Ast.expr)
    (expr_else : Il.Ast.expr) : Ctk.t =
  let ctks = [ expr_cond.note.ctk; expr_then.note.ctk; expr_else.note.ctk ] in
  Ctk.joins ctks

and ctk_cast_expr (_typ : Il.Ast.typ) (expr : Il.Ast.expr) : Ctk.t =
  expr.note.ctk

and ctk_bitstring_acc_expr (expr_base : Il.Ast.expr) (_value_lo : Il.Ast.value)
    (_value_hi : Il.Ast.value) : Ctk.t =
  expr_base.note.ctk

and ctk_expr_acc_expr (expr_base : Il.Ast.expr) (member : Il.Ast.member) : Ctk.t
    =
  let typ_base = expr_base.note.typ in
  let ctk_base = expr_base.note.ctk in
  match ctk_base with
  | LCTK -> (
      match typ_base with
      | Types.StackT _ when member.it = "size" -> LCTK
      | _ -> DYN)
  | CTK | DYN -> DYN

and ctk_call_method_expr (expr_base : Il.Ast.expr) (member : Il.Ast.member)
    (targs : Il.Ast.targ list) (args : Il.Ast.arg list) : Ctk.t =
  let typ_base = expr_base.note.typ in
  match member.it with
  | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
      if not (targs = [] && args = []) then (
        Format.eprintf
          "(ctk_call_method_expr) %s does not take type arguments or arguments\n"
          member.it;
        assert false);
      if Type.is_ground typ_base then LCTK else CTK
  | _ -> DYN

(* Static evaluation of local compile-time known expressions *)

let rec eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : Il.Ast.expr) :
    Il.Ast.value =
  assert (expr.note.ctk = Ctk.LCTK);
  let value = eval_expr' cursor ctx expr.it in
  value $ expr.at

and eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : Il.Ast.expr list) :
    Il.Ast.value list =
  List.map (eval_expr cursor ctx) exprs

and eval_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : Il.Ast.expr') :
    Il.Ast.value' =
  match expr with
  | ValueE { value } -> value.it
  | VarE { var } -> eval_var_expr cursor ctx var
  | TupleE { exprs } -> eval_tuple_expr cursor ctx exprs
  | RecordE { fields } -> eval_record_expr cursor ctx fields
  | UnE { unop; expr } -> eval_unop_expr cursor ctx unop expr
  | BinE { binop; expr_l; expr_r } ->
      eval_binop_expr cursor ctx binop expr_l expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      eval_ternop_expr cursor ctx expr_cond expr_then expr_else
  | CastE { typ; expr } -> eval_cast_expr cursor ctx typ expr
  | BitAccE { expr_base; value_lo; value_hi } ->
      eval_bitstring_acc_expr cursor ctx expr_base value_lo value_hi
  | ExprAccE { expr_base; member } ->
      eval_expr_acc_expr cursor ctx expr_base member
  | CallMethodE { expr_base; member; targs; args } ->
      eval_call_method_expr cursor ctx expr_base member targs args
  | _ -> assert false

and eval_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : Il.Ast.var) :
    Value.t =
  Ctx.find Ctx.find_value_opt cursor var ctx

and eval_tuple_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs : Il.Ast.expr list) : Value.t =
  let values = eval_exprs cursor ctx exprs in
  let values = List.map it values in
  Value.TupleV values

and eval_record_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (Il.Ast.member * Il.Ast.expr) list) : Value.t =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = eval_exprs cursor ctx exprs in
  let values = List.map it values in
  let fields = List.combine members values in
  Value.RecordV fields

and eval_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : Il.Ast.unop)
    (expr : Il.Ast.expr) : Value.t =
  let value = eval_expr cursor ctx expr in
  Runtime.Numerics.eval_unop unop value.it

and eval_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : Il.Ast.binop)
    (expr_l : Il.Ast.expr) (expr_r : Il.Ast.expr) : Value.t =
  let value_l = eval_expr cursor ctx expr_l |> it in
  let value_r = eval_expr cursor ctx expr_r |> it in
  Runtime.Numerics.eval_binop binop value_l value_r

and eval_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_cond : Il.Ast.expr) (expr_then : Il.Ast.expr)
    (expr_else : Il.Ast.expr) : Value.t =
  let value_cond = eval_expr cursor ctx expr_cond in
  let cond = Value.get_bool value_cond.it in
  let expr = if cond then expr_then else expr_else in
  eval_expr cursor ctx expr |> it

and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : Il.Ast.typ)
    (expr : Il.Ast.expr) : Value.t =
  let value = eval_expr cursor ctx expr in
  Runtime.Numerics.eval_cast typ.it value.it

and eval_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : Il.Ast.expr) (value_lo : Il.Ast.value)
    (value_hi : Il.Ast.value) : Value.t =
  let value_base = eval_expr cursor ctx expr_base in
  let value_base = value_base.it in
  let value_lo, value_hi = (value_lo.it, value_hi.it) in
  Runtime.Numerics.eval_bitstring_access value_base value_hi value_lo

and eval_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : Il.Ast.expr) (member : Il.Ast.member) : Value.t =
  let value_base = eval_expr cursor ctx expr_base in
  match value_base.it with
  | StackV (_, _, size) when member.it = "size" -> Value.IntV size
  | _ -> assert false

and eval_call_method_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t)
    (expr_base : Il.Ast.expr) (member : Il.Ast.member)
    (targs : Il.Ast.targ list) (args : Il.Ast.arg list) : Value.t =
  match member.it with
  | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
      assert (targs = [] && args = []);
      let typ_base = expr_base.note.typ in
      Runtime.Numerics.eval_size typ_base member.it
  | _ -> assert false
