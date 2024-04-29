open Syntax
open Domain 
open Domain.Scope

(* Compile-time evaluation of type simplification *)

let rec eval_simplify_typ (tdenv : TDEnv.t) (typ : Type.t) : Type.t =
  match typ with
  | TName { name } -> eval_simplify_typ tdenv (TDEnv.find name tdenv)
  | TNewType { name } -> TDEnv.find name tdenv
  | _ -> typ

let rec eval_typ (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t) (typ : Ast.Type.t) : Type.t
    =
  match typ with
  | Bool _ -> TBool
  | Integer _ -> TAInt
  | IntType { expr; _ } ->
      let width = eval_expr tdenv genv sto expr |> Ops.extract_bigint in
      TBit { width }
  | BitType { expr; _ } ->
      let width = eval_expr tdenv genv sto expr |> Ops.extract_bigint in
      TBit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr tdenv genv sto expr |> Ops.extract_bigint in
      TBit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ tdenv genv sto header in
      let size = eval_expr tdenv genv sto size |> Ops.extract_bigint in
      TArray { typ = header; size }
  | String _ -> TString
  | Error _ -> TError
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ tdenv genv sto) args in
      TTuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      TDEnv.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ tdenv genv sto base
  | _ -> Printf.sprintf "(TODO: eval_typ) %s" "TODO" |> failwith

(* Compile-time evaluation of expressions *)

and eval_expr (tdenv : TDEnv.t) (genv : Env.t) (sto : Sto.t) (expr : Ast.Expression.t) :
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
  (*
  | Name { name = BareName name; _ }
  | Name { name = QualifiedName ([], name); _ } ->
      let name = name.str in
      Scope.find_single name genv sto |> Option.get
  *)
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr tdenv genv sto bits in
      let vlo = eval_expr tdenv genv sto lo in
      let vhi = eval_expr tdenv genv sto hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr tdenv genv sto) values in
      VTuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : Ast.KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr tdenv genv sto entry.value in
            (key, value))
          entries
      in
      VStruct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr tdenv genv sto arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr tdenv genv sto arg_fst in
      let varg_snd = eval_expr tdenv genv sto arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ tdenv genv sto typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr tdenv genv sto expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr tdenv genv sto expr in
      let name = name.str in
      match vexpr with
      | VHeader { entries; _ } | VStruct { entries } -> List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" "TODO" |> failwith)
  | _ -> Printf.sprintf "(TODO: eval_expr) %s" "TODO" |> failwith
