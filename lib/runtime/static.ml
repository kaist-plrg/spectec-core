open Syntax
open Ast

(* Environments *)

type env = Env.t
type tenv = Tenv.t

(* Compile-time evaluation of type simplification *)

let rec eval_simplify_typ (tenv : tenv) (typ : Typ.t) : Typ.base =
  match typ with
  | Base base -> (
      match base with
      | Typ.Name { name } -> eval_simplify_typ tenv (Tenv.find name tenv)
      | Typ.NewType { name } -> Tenv.find name tenv |> Typ.extract_base
      | _ -> base)
  | _ ->
      Printf.sprintf "(TODO: eval_simplify_typ) %s" (Typ.print typ) |> failwith

let rec eval_typ (env : env) (tenv : tenv) (typ : Type.t) : Typ.t =
  match typ with
  | Bool _ -> Typ.Base Typ.Bool
  | Integer _ -> Typ.Base Typ.AInt
  | IntType { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      let btyp = Typ.Bit { width } in
      Typ.Base btyp
  | BitType { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      let btyp = Typ.Bit { width } in
      Typ.Base btyp
  | VarBit { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      let btyp = Typ.Bit { width } in
      Typ.Base btyp
  | HeaderStack { header; size; _ } ->
      let vheader = eval_typ env tenv header in
      let bheader = Typ.extract_base vheader in
      let size = eval_expr env tenv size |> Value.extract_bigint in
      let btyp = Typ.Array { typ = bheader; size } in
      Typ.Base btyp
  | String _ -> Typ.Base Typ.String
  | Error _ -> Typ.Base Typ.Error
  | Tuple { args; _ } ->
      let vargs =
        List.map (fun arg -> eval_typ env tenv arg |> Typ.extract_base) args
      in
      let btyp = Typ.Tuple vargs in
      Typ.Base btyp
  | TypeName { name = BareName text; _ } ->
      let var = text.str in
      Tenv.find var tenv
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Tenv.find_toplevel var tenv
  | _ ->
      Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

and eval_base_typ (env : env) (tenv : tenv) (typ : Type.t) : Typ.base =
  eval_typ env tenv typ |> Typ.extract_base

(* Compile-time evaluation of expressions *)

and eval_expr (env : env) (tenv : tenv) (expr : Expression.t) : Value.t =
  match expr with
  | True _ ->
      let bvalue = Value.Bool true in
      Value.Base bvalue
  | False _ ->
      let bvalue = Value.Bool false in
      Value.Base bvalue
  | Int { i; _ } ->
      let value = i.value in
      let bvalue =
        match i.width_signed with
        | Some (width, signed) ->
            if signed then Value.Int { value; width }
            else Value.Bit { value; width }
        | None -> Value.AInt value
      in
      Value.Base bvalue
  | String { text; _ } ->
      let bvalue = Value.String text.str in
      Value.Base bvalue
  | Name { name = BareName text; _ } ->
      let var = text.str in
      Env.find var env
  | Name { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Env.find_toplevel var env
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr env tenv bits in
      let vlo = eval_expr env tenv lo in
      let vhi = eval_expr env tenv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues =
        List.map
          (fun value -> eval_expr env tenv value |> Value.extract_base)
          values
      in
      let bvalue = Value.Tuple vvalues in
      Value.Base bvalue
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr env tenv entry.value |> Value.extract_base in
            (key, value))
          entries
      in
      let bvalue = Value.Struct { entries = ventries } in
      Value.Base bvalue
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr env tenv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr env tenv arg_fst in
      let varg_snd = eval_expr env tenv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ env tenv typ in
      let typ = eval_simplify_typ tenv typ in
      let vexpr = eval_expr env tenv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } ->
      let vexpr = eval_expr env tenv expr |> Value.extract_base in
      let name = name.str in
      let bvalue =
        match vexpr with
        | Value.Header { entries; _ } | Value.Struct { entries } ->
            List.assoc name entries
        | _ ->
            Printf.sprintf "(eval_expr) %s cannot be accessed"
              (Value.print_base vexpr)
            |> failwith
      in
      Value.Base bvalue
  | _ ->
      Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith
