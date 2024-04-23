open Syntax.Ast
open Runtime
open Runtime.Domain
open Utils

(* Compile-time evaluation of type simplification *)

let rec eval_simplify_typ (tdenv : tdenv) (typ : typ) : typ =
  match typ with
  | TName { name } -> eval_simplify_typ tdenv (Env.find name tdenv)
  | TNewType { name } -> Env.find name tdenv
  | _ -> typ

let rec eval_typ (tdenv : tdenv) (ienv : env) (vsto : vsto) (typ : Type.t) : typ
    =
  match typ with
  | Bool _ -> TBool
  | Integer _ -> TAInt
  | IntType { expr; _ } ->
      let width = eval_expr tdenv ienv vsto expr |> Ops.extract_bigint in
      TBit { width }
  | BitType { expr; _ } ->
      let width = eval_expr tdenv ienv vsto expr |> Ops.extract_bigint in
      TBit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr tdenv ienv vsto expr |> Ops.extract_bigint in
      TBit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ tdenv ienv vsto header in
      let size = eval_expr tdenv ienv vsto size |> Ops.extract_bigint in
      TArray { typ = header; size }
  | String _ -> TString
  | Error _ -> TError
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ tdenv ienv vsto) args in
      TTuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Env.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ tdenv ienv vsto base
  | _ -> Printf.sprintf "(TODO: eval_typ) %s" "TODO" |> failwith

(* Compile-time evaluation of expressions *)

and eval_expr (tdenv : tdenv) (ienv : env) (vsto : vsto) (expr : Expression.t) :
    value =
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
  | Name { name = BareName name; _ }
  | Name { name = QualifiedName ([], name); _ } ->
      let name = name.str in
      Scope.find_single name ienv vsto |> Option.get
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr tdenv ienv vsto bits in
      let vlo = eval_expr tdenv ienv vsto lo in
      let vhi = eval_expr tdenv ienv vsto hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr tdenv ienv vsto) values in
      VTuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr tdenv ienv vsto entry.value in
            (key, value))
          entries
      in
      VStruct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr tdenv ienv vsto arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr tdenv ienv vsto arg_fst in
      let varg_snd = eval_expr tdenv ienv vsto arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ tdenv ienv vsto typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr tdenv ienv vsto expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr tdenv ienv vsto expr in
      let name = name.str in
      match vexpr with
      | VHeader { entries; _ } | VStruct { entries } -> List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" "TODO" |> failwith)
  | _ -> Printf.sprintf "(TODO: eval_expr) %s" "TODO" |> failwith
