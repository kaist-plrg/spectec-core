open Syntax.Ast
open Runtime_.Base
open Runtime_.Context

let rec eval_simplify_type (ictx: ICtx.t) (typ : Type.t) : Type.t =
  match typ with
  | NameT name ->
      ICtx.find_td name ictx
      |> Option.get
      |> eval_simplify_type ictx
  | NewT name -> ICtx.find_td name ictx |> Option.get
  | _ -> typ

let rec eval_type (ictx: ICtx.t) (typ: typ): Type.t =
  match typ with
  | BoolT -> BoolT
  | ErrT -> ErrT
  | StrT -> StrT
  | AIntT -> AIntT
  | IntT width ->
      let width = eval_expr ictx width |> Runtime_.Ops.extract_bigint in
      IntT width
  | BitT width ->
      let width = eval_expr ictx width |> Runtime_.Ops.extract_bigint in
      BitT width
  | VBitT width ->
      let width = eval_expr ictx width |> Runtime_.Ops.extract_bigint in
      VBitT width
  | NameT (Top name) -> ICtx.find_td_glob name ictx |> Option.get
  | NameT (Bare name) -> ICtx.find_td name ictx |> Option.get
  (* (TODO) Handle specialized types *)
  | SpecT (name, _) -> eval_type ictx (NameT name)
  | TupleT typs ->
      let typs = List.map (eval_type ictx) typs in
      TupleT typs
  | _ -> Printf.sprintf "(TODO: eval_typ)" |> failwith

and eval_expr (ictx: ICtx.t) (expr: expr): Value.t =
  match expr with
  | BoolE b -> BoolV b
  | StrE str -> StrV str
  | NumE (value, width_signed) -> (
      match width_signed with
      | Some (width, signed) ->
          if signed then IntV (width, value) else BitV (width, value)
      | None -> AIntV value)
  | UnE (op, arg) ->
      let varg = eval_expr ictx arg in
      Runtime_.Ops.eval_unop op varg
  | BinE (op, arg_fst, arg_snd) ->
      let varg_fst = eval_expr ictx arg_fst in
      let varg_snd = eval_expr ictx arg_snd in
      Runtime_.Ops.eval_binop op varg_fst varg_snd
  | CastE (typ, arg) ->
      let typ =
        eval_type ictx typ
        |> eval_simplify_type ictx
      in
      let varg = eval_expr ictx arg in
      Runtime_.Ops.eval_cast typ varg
  | _ -> Printf.sprintf "(TODO: eval_expr)" |> failwith
