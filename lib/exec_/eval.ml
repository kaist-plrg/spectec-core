open Syntax.Ast
open Runtime_.Base
open Runtime_.Context

let rec eval_simplify_type (ctx : Ctx.t) (typ : Type.t) : Type.t =
  match typ with
  | NameT name -> Ctx.find_td name ctx |> Option.get |> eval_simplify_type ctx
  | NewT name -> Ctx.find_td name ctx |> Option.get
  | _ -> typ

let rec eval_type (ctx : Ctx.t) (typ : typ) : Type.t =
  match typ with
  | BoolT -> BoolT
  | ErrT -> ErrT
  | StrT -> StrT
  | AIntT -> AIntT
  | IntT width ->
      let width = eval_expr ctx width |> Runtime_.Ops.extract_bigint in
      IntT width
  | BitT width ->
      let width = eval_expr ctx width |> Runtime_.Ops.extract_bigint in
      BitT width
  | VBitT width ->
      let width = eval_expr ctx width |> Runtime_.Ops.extract_bigint in
      VBitT width
  | NameT (Top name) -> Ctx.find_td_glob name ctx |> Option.get
  | NameT (Bare name) -> Ctx.find_td name ctx |> Option.get
  (* (TODO) Handle specialized types *)
  | SpecT (name, _) -> eval_type ctx (NameT name)
  | TupleT typs ->
      let typs = List.map (eval_type ctx) typs in
      TupleT typs
  | _ -> Printf.sprintf "(TODO: eval_typ)" |> failwith

and eval_expr (ctx : Ctx.t) (expr : expr) : Value.t =
  match expr with
  | BoolE b -> BoolV b
  | StrE str -> StrV str
  | NumE (value, width_signed) -> (
      match width_signed with
      | Some (width, signed) ->
          if signed then IntV (width, value) else BitV (width, value)
      | None -> AIntV value)
  | VarE (Top name) -> Ctx.find_var_glob name ctx |> Option.get |> snd
  | VarE (Bare name) -> Ctx.find_var name ctx |> Option.get |> snd
  | ListE values ->
      let values = List.map (eval_expr ctx) values in
      TupleV values
  | RecordE fields ->
      let fields =
        List.map (fun (name, expr) -> (name, eval_expr ctx expr)) fields
      in
      StructV fields
  | UnE (op, arg) ->
      let varg = eval_expr ctx arg in
      Runtime_.Ops.eval_unop op varg
  | BinE (op, arg_fst, arg_snd) ->
      let varg_fst = eval_expr ctx arg_fst in
      let varg_snd = eval_expr ctx arg_snd in
      Runtime_.Ops.eval_binop op varg_fst varg_snd
  | CastE (typ, arg) ->
      let typ = eval_type ctx typ |> eval_simplify_type ctx in
      let varg = eval_expr ctx arg in
      Runtime_.Ops.eval_cast typ varg
  | ExprAccE (expr, name) -> (
      let value = eval_expr ctx expr in
      match value with
      | HeaderV (_, fields) | StructV fields -> List.assoc name fields
      | _ -> assert false)
  | _ -> Printf.sprintf "(TODO: eval_expr)" |> failwith
