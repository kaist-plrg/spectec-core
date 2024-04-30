open Syntax
open Runtime
open Runtime.Scope

(* Type simplification *)

let rec eval_simplify_typ (bscope : bscope) (typ : Type.t) : Type.t =
  let tdenv, _, _, _ = bscope in
  match typ with
  | TName { name } -> eval_simplify_typ bscope (TDEnv.find name tdenv)
  | TNewType { name } -> TDEnv.find name tdenv
  | _ -> typ

let rec eval_typ (bscope : bscope) (typ : Ast.Type.t) : Type.t =
  match typ with
  | Bool _ -> TBool
  | Integer _ -> TAInt
  | IntType { expr; _ } ->
      let width = eval_expr bscope expr |> Ops.extract_bigint in
      TBit { width }
  | BitType { expr; _ } ->
      let width = eval_expr bscope expr |> Ops.extract_bigint in
      TBit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr bscope expr |> Ops.extract_bigint in
      TBit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ bscope header in
      let size = eval_expr bscope size |> Ops.extract_bigint in
      TArray { typ = header; size }
  | String _ -> TString
  | Error _ -> TError
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ bscope) args in
      TTuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let tdenv, _, _, _ = bscope in
      let var = text.str in
      TDEnv.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ bscope base
  | _ -> Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

(* Evaluation of type arguments *)

and eval_targs (caller_scope : bscope) (callee_scope : bscope)
  (tparams : string list) (typs : Ast.Type.t list) : bscope =
  assert (List.length tparams = List.length typs);
  let tdenv_callee, lenv_callee, genv_callee, sto_callee = callee_scope in
  let tdenv_callee =
    List.fold_left2
      (fun tdenv_callee tparam typ ->
        let typ = eval_typ caller_scope typ in
        TDEnv.add tparam typ tdenv_callee)
      tdenv_callee tparams typs
  in
  (tdenv_callee, lenv_callee, genv_callee, sto_callee)

(* Evaluation of expressions *)

and eval_expr (bscope : bscope) (expr : Ast.Expression.t) :
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
      let _, value = find_var name bscope in
      value
  | Name { name = QualifiedName ([], name); _ } ->
      let name = name.str in
      let _, value = find_var_global name bscope in
      value
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr bscope bits in
      let vlo = eval_expr bscope lo in
      let vhi = eval_expr bscope hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr bscope) values in
      Value.VTuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : Ast.KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr bscope entry.value in
            (key, value))
          entries
      in
      Value.VStruct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr bscope arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr bscope arg_fst in
      let varg_snd = eval_expr bscope arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ bscope typ in
      let typ = eval_simplify_typ bscope typ in
      let vexpr = eval_expr bscope expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr bscope expr in
      let name = name.str in
      match vexpr with
      | VHeader { entries; _ } | VStruct { entries } -> List.assoc name entries
      | _ ->
          Format.asprintf "(eval_expr) %a cannot be accessed" Value.pp vexpr |> failwith)
  | _ -> Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith
