module Ctk = Runtime_static.Ctk
module Value = Runtime_static.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Numerics = Runtime_static.Numerics
module Builtins = Runtime_static.Builtins
module F = Format
open Util.Source
open Util.Error

let check = check_checker
let error_no_info = error_checker_no_info
let error_pass_info = error_checker_pass_info

(* (18.1) Compile-time known and local compile-time known values

   Certain expressions in a P4 program have the property that their value can be
   determined at compile time. Moreover, for some of these expressions,
   their value can be determined only using information in the current scope.
   We call these compile-time known values and local compile-time known valuespectively.

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
  check
    (Ctk.is_lctk expr.note.ctk)
    (Format.asprintf
       "(check_ctk) %a is not a local compile-time known expression"
       (Il.Pp.pp_expr ~level:0) expr)

let check_ctk (expr : Il.Ast.expr) : unit =
  check (Ctk.is_ctk expr.note.ctk)
    (Format.asprintf "(check_ctk) %a is not a compile-time known expression"
       (Il.Pp.pp_expr ~level:0) expr)

let rec ctk_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : Il.Ast.expr') :
    Ctk.t =
  match expr with
  | ValueE _ -> LCTK
  | VarE { var } -> ctk_var_expr cursor ctx var
  | SeqE { exprs } | SeqDefaultE { exprs } -> ctk_seq_expr exprs
  | RecordE { fields } | RecordDefaultE { fields } -> ctk_record_expr fields
  | DefaultE -> LCTK
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
  | CallTypeE { typ; member } -> ctk_call_type_expr typ member
  | InstE _ -> CTK

and ctk_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : Il.Ast.var) : Ctk.t
    =
  let rtype = Ctx.find_opt Ctx.find_rtype_opt cursor var ctx in
  check (Option.is_some rtype)
    (Format.asprintf "(ctk_var_expr) %a a free variable\n" Il.Pp.pp_var var);
  let _, _, ctk = Option.get rtype in
  ctk

and ctk_seq_expr (exprs : Il.Ast.expr list) : Ctk.t =
  let ctks = List.map (fun expr -> Il.Ast.(expr.note.ctk)) exprs in
  let ctk = Ctk.joins ctks in
  ctk

and ctk_record_expr (fields : (Il.Ast.member * Il.Ast.expr) list) : Ctk.t =
  let exprs = List.map snd fields in
  let ctks = List.map (fun expr -> Il.Ast.(expr.note.ctk)) exprs in
  let ctk = Ctk.joins ctks in
  ctk

and ctk_unop_expr (_unop : Il.Ast.unop) (expr : Il.Ast.expr) : Ctk.t =
  let ctk = expr.note.ctk in
  ctk

and ctk_binop_expr (_binop : Il.Ast.binop) (expr_l : Il.Ast.expr)
    (expr_r : Il.Ast.expr) : Ctk.t =
  let ctk_l = expr_l.note.ctk in
  let ctk_r = expr_r.note.ctk in
  let ctk = Ctk.join ctk_l ctk_r in
  ctk

and ctk_ternop_expr (expr_cond : Il.Ast.expr) (expr_then : Il.Ast.expr)
    (expr_else : Il.Ast.expr) : Ctk.t =
  let ctks = [ expr_cond.note.ctk; expr_then.note.ctk; expr_else.note.ctk ] in
  let ctk = Ctk.joins ctks in
  ctk

and ctk_cast_expr (_typ : Il.Ast.typ) (expr : Il.Ast.expr) : Ctk.t =
  let ctk = expr.note.ctk in
  ctk

and ctk_bitstring_acc_expr (expr_base : Il.Ast.expr) (_value_lo : Il.Ast.value)
    (_value_hi : Il.Ast.value) : Ctk.t =
  let ctk = expr_base.note.ctk in
  ctk

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
      check
        (targs = [] && args = [])
        (Format.asprintf
           "(ctk_call_method_expr) %s does not take type arguments or arguments"
           member.it);
      let ctk = if Type.is_ground typ_base then Ctk.LCTK else Ctk.CTK in
      ctk
  | _ -> DYN

and ctk_call_type_expr (typ : Il.Ast.typ) (member : Il.Ast.member) : Ctk.t =
  match member.it with
  | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
      let ctk = if Type.is_ground typ.it then Ctk.LCTK else Ctk.CTK in
      ctk
  | _ -> DYN

(* Static evaluation of local compile-time known expressions *)

let rec eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : Il.Ast.expr) :
    Il.Ast.value =
  try
    check_lctk expr;
    let value = eval_expr' cursor ctx expr.it in
    value $ expr.at
  with CheckErr _ as err -> error_pass_info expr.at err

and eval_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : Il.Ast.expr') :
    Il.Ast.value' =
  match expr with
  | ValueE { value } -> value.it
  | VarE { var } -> eval_var_expr cursor ctx var
  | SeqE { exprs } -> eval_seq_expr cursor ctx exprs
  | SeqDefaultE { exprs } -> eval_seq_default_expr cursor ctx exprs
  | RecordE { fields } -> eval_record_expr cursor ctx fields
  | RecordDefaultE { fields } -> eval_record_default_expr cursor ctx fields
  | DefaultE -> Value.DefaultV
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
  | CallTypeE { typ; member } -> eval_call_type_expr cursor ctx typ member
  | _ ->
      F.asprintf "(eval_expr) not a local compile-time known expression"
      |> error_no_info

and eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : Il.Ast.expr list) :
    Il.Ast.value list =
  List.map (eval_expr cursor ctx) exprs

and eval_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : Il.Ast.var) :
    Value.t =
  let value = Ctx.find Ctx.find_value_opt cursor var ctx in
  value

and eval_seq_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : Il.Ast.expr list)
    : Value.t =
  let values = eval_exprs cursor ctx exprs in
  let values = List.map it values in
  let value = Value.SeqV values in
  value

and eval_seq_default_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (exprs : Il.Ast.expr list) : Value.t =
  let values = eval_exprs cursor ctx exprs in
  let values = List.map it values in
  let value = Value.SeqDefaultV values in
  value

and eval_record_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (Il.Ast.member * Il.Ast.expr) list) : Value.t =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = eval_exprs cursor ctx exprs in
  let values = List.map it values in
  let fields = List.combine members values in
  let value = Value.RecordV fields in
  value

and eval_record_default_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (fields : (Il.Ast.member * Il.Ast.expr) list) : Value.t =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let values = eval_exprs cursor ctx exprs in
  let values = List.map it values in
  let fields = List.combine members values in
  let value = Value.RecordDefaultV fields in
  value

and eval_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : Il.Ast.unop)
    (expr : Il.Ast.expr) : Value.t =
  let value = eval_expr cursor ctx expr in
  let value = Numerics.eval_unop unop value.it in
  value

and eval_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : Il.Ast.binop)
    (expr_l : Il.Ast.expr) (expr_r : Il.Ast.expr) : Value.t =
  let value_l = eval_expr cursor ctx expr_l in
  let value_r = eval_expr cursor ctx expr_r in
  let value = Numerics.eval_binop binop value_l.it value_r.it in
  value

and eval_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_cond : Il.Ast.expr) (expr_then : Il.Ast.expr)
    (expr_else : Il.Ast.expr) : Value.t =
  let value_cond = eval_expr cursor ctx expr_cond in
  let cond = Value.get_bool value_cond.it in
  let expr = if cond then expr_then else expr_else in
  let value = eval_expr cursor ctx expr in
  value.it

and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : Il.Ast.typ)
    (expr : Il.Ast.expr) : Value.t =
  let value = eval_expr cursor ctx expr in
  let value = Numerics.eval_cast typ.it value.it in
  value

and eval_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : Il.Ast.expr) (value_lo : Il.Ast.value)
    (value_hi : Il.Ast.value) : Value.t =
  let value_base = eval_expr cursor ctx expr_base in
  let value =
    Numerics.eval_bitstring_access value_base.it value_hi.it value_lo.it
  in
  value

and eval_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
    (expr_base : Il.Ast.expr) (member : Il.Ast.member) : Value.t =
  let value_base = eval_expr cursor ctx expr_base in
  let value =
    match value_base.it with
    | StackV (_, _, size) when member.it = "size" -> Value.IntV size
    | _ ->
        Format.asprintf
          "(eval_expr_acc_expr) %a.%a is not local compile-time known"
          (Il.Pp.pp_expr ~level:0) expr_base Il.Pp.pp_member member
        |> error_no_info
  in
  value

and eval_call_method_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t)
    (expr_base : Il.Ast.expr) (member : Il.Ast.member)
    (targs : Il.Ast.targ list) (args : Il.Ast.arg list) : Value.t =
  let value =
    match member.it with
    | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
        assert (targs = [] && args = []);
        let typ_base = expr_base.note.typ in
        Builtins.size typ_base member.it
    | _ ->
        Format.asprintf
          "(eval_call_method_expr) %a.%a is not local compile-time known"
          (Il.Pp.pp_expr ~level:0) expr_base Il.Pp.pp_member member
        |> error_no_info
  in
  value

and eval_call_type_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t) (typ : Il.Ast.typ)
    (member : Il.Ast.member) : Value.t =
  let value =
    match member.it with
    | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
        Builtins.size typ.it member.it
    | _ ->
        Format.asprintf
          "(eval_call_type_expr) %a.%a is not local compile-time known"
          (Il.Pp.pp_typ ~level:0) typ Il.Pp.pp_member member
        |> error_no_info
  in
  value
