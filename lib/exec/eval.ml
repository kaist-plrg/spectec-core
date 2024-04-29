open Syntax
open Domain
open Domain.Scope

type benv = Env.t * Env.t * Sto.t

(* Type simplification *)

let rec eval_simplify_typ (tdenv : TDEnv.t) (typ : Type.t) : Type.t =
  match typ with
  | TName { name } -> eval_simplify_typ tdenv (TDEnv.find name tdenv)
  | TNewType { name } -> TDEnv.find name tdenv
  | _ -> typ

let rec eval_typ (tdenv : TDEnv.t) (benv : benv) (typ : Ast.Type.t) : Type.t =
  match typ with
  | Bool _ -> TBool
  | Integer _ -> TAInt
  | IntType { expr; _ } ->
      let width = eval_expr tdenv benv expr |> Ops.extract_bigint in
      TBit { width }
  | BitType { expr; _ } ->
      let width = eval_expr tdenv benv expr |> Ops.extract_bigint in
      TBit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr tdenv benv expr |> Ops.extract_bigint in
      TBit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ tdenv benv header in
      let size = eval_expr tdenv benv size |> Ops.extract_bigint in
      TArray { typ = header; size }
  | String _ -> TString
  | Error _ -> TError
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ tdenv benv) args in
      TTuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Env.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ tdenv benv base
  | _ -> Printf.sprintf "(TODO: eval_typ) %s" "TODO" |> failwith

(* Evaluation of type arguments *)

and eval_targs (tdenv : TDEnv.t) (tdenv_local : TDEnv.t) (benv : benv)
    (tparams : string list) (typs : Ast.Type.t list) : TDEnv.t =
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun tdenv_local tparam typ ->
      let typ = eval_typ tdenv benv typ in
      TDEnv.add tparam typ tdenv_local)
    tdenv_local tparams typs

(* Evaluation of expressions *)

and eval_expr (tdenv : TDEnv.t) (benv : benv) (expr : Ast.Expression.t) :
    Value.t =
  match expr with
  | True _ -> VBool true
  | False _ -> VBool false
  | Int { i; _ } -> (
      let value = i.value in
      match i.width_signed with
      | Some (width, signed) ->
          if signed then VInt { value; width } else VBit { value; width }
      | None -> VAInt value)
  | String { text; _ } -> VString text.str
  | Name { name = BareName name; _ } ->
      let name = name.str in
      let _, value = find_var name benv in
      value
  | Name { name = QualifiedName ([], name); _ } ->
      let name = name.str in
      let _, value = find_var_global name benv in
      value
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr tdenv benv bits in
      let vlo = eval_expr tdenv benv lo in
      let vhi = eval_expr tdenv benv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr tdenv benv) values in
      Value.VTuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : Ast.KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr tdenv benv entry.value in
            (key, value))
          entries
      in
      Value.VStruct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr tdenv benv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr tdenv benv arg_fst in
      let varg_snd = eval_expr tdenv benv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ tdenv benv typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr tdenv benv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr tdenv benv expr in
      let name = name.str in
      match vexpr with
      | VHeader { entries; _ } | VStruct { entries } -> List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" "TODO" |> failwith)
  | _ -> Printf.sprintf "(TODO: eval_expr) %s" "TODO" |> failwith
