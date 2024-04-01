open Syntax
open Ast
open Runtime
open Envs

(* Compile-time evaluation of type simplification *)

let rec eval_simplify_typ (tdenv : tdenv) (typ : Typ.t) : Typ.t =
  match typ with
  | Typ.Name { name } -> eval_simplify_typ tdenv (TDEnv.find name tdenv)
  | Typ.NewType { name } -> TDEnv.find name tdenv
  | _ -> typ

let rec eval_typ (cenv : cenv) (vsto : vsto) (tdenv : tdenv) (typ : Type.t) :
    Typ.t =
  match typ with
  | Bool _ -> Typ.Bool
  | Integer _ -> Typ.AInt
  | IntType { expr; _ } ->
      let width = eval_expr cenv vsto tdenv expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | BitType { expr; _ } ->
      let width = eval_expr cenv vsto tdenv expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr cenv vsto tdenv expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ cenv vsto tdenv header in
      let size = eval_expr cenv vsto tdenv size |> Ops.extract_bigint in
      Typ.Array { typ = header; size }
  | String _ -> Typ.String
  | Error _ -> Typ.Error
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ cenv vsto tdenv) args in
      Typ.Tuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      TDEnv.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ cenv vsto tdenv base
  | _ ->
      Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

(* Compile-time evaluation of expressions *)

and eval_expr (cenv : cenv) (vsto : vsto) (tdenv : tdenv) (expr : Expression.t)
    : Value.t =
  match expr with
  | True _ -> Value.Bool true
  | False _ -> Value.Bool false
  | Int { i; _ } -> (
      let value = i.value in
      match i.width_signed with
      | Some (width, signed) ->
          if signed then Value.Int { value; width }
          else Value.Bit { value; width }
      | None -> Value.AInt value)
  | String { text; _ } -> Value.String text.str
  | Name { name = BareName text; _ } ->
      let var = text.str in
      find_value cenv vsto var
  | Name { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      find_value_toplevel cenv vsto var
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr cenv vsto tdenv bits in
      let vlo = eval_expr cenv vsto tdenv lo in
      let vhi = eval_expr cenv vsto tdenv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr cenv vsto tdenv) values in
      Value.Tuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr cenv vsto tdenv entry.value in
            (key, value))
          entries
      in
      Value.Struct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr cenv vsto tdenv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr cenv vsto tdenv arg_fst in
      let varg_snd = eval_expr cenv vsto tdenv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ cenv vsto tdenv typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr cenv vsto tdenv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr cenv vsto tdenv expr in
      let name = name.str in
      match vexpr with
      | Value.Header { entries; _ } | Value.Struct { entries } ->
          List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" (Value.print vexpr)
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith
