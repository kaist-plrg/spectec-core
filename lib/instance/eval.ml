open Syntax.Ast
open Runtime.Base
open Runtime.Context
open Util.Source

let rec eval_simplify_type (ictx : ICtx.t) (typ : Type.t) : Type.t =
  match typ with
  | NameT name ->
      ICtx.find_td name ictx |> Option.get |> eval_simplify_type ictx
  | NewT name -> ICtx.find_td name ictx |> Option.get
  | _ -> typ

let rec eval_type (ictx : ICtx.t) (typ : typ) : Type.t =
  match typ.it with
  | BoolT -> BoolT
  | ErrT -> ICtx.find_td_glob "error" ictx |> Option.get
  | StrT -> StrT
  | AIntT -> AIntT
  | IntT width ->
      let width = eval_expr ictx width |> Runtime.Ops.extract_bigint in
      IntT width
  | BitT width ->
      let width = eval_expr ictx width |> Runtime.Ops.extract_bigint in
      BitT width
  | VBitT width ->
      let width = eval_expr ictx width |> Runtime.Ops.extract_bigint in
      VBitT width
  | NameT { it = Top id; _ } -> ICtx.find_td_glob id.it ictx |> Option.get
  | NameT { it = Bare id; _ } -> ICtx.find_td id.it ictx |> Option.get
  (* (TODO) Handle specialized types *)
  | SpecT (name, _) -> eval_type ictx (NameT name $ no_info)
  | StackT (typ, size) ->
      let typ = eval_type ictx typ in
      let size = eval_expr ictx size |> Runtime.Ops.extract_bigint in
      StackT (typ, size)
  | TupleT typs ->
      let typs = List.map (eval_type ictx) typs in
      TupleT typs
  | _ ->
      Format.asprintf "(TODO: eval_type) %a" Syntax.Print.print_type typ
      |> failwith

and eval_expr (ictx : ICtx.t) (expr : expr) : Value.t =
  match expr.it with
  | BoolE b -> BoolV b
  | StrE str -> StrV str
  | NumE { it = value, width_signed; _ } -> (
      match width_signed with
      | Some (width, signed) ->
          if signed then IntV (width, value) else BitV (width, value)
      | None -> AIntV value)
  | VarE { it = Top id; _ } ->
      ICtx.find_var_glob id.it ictx |> Option.get |> snd
  | VarE { it = Bare id; _ } -> ICtx.find_var id.it ictx |> Option.get |> snd
  | UnE (op, arg) ->
      let varg = eval_expr ictx arg in
      Runtime.Ops.eval_unop op varg
  | BinE (op, arg_fst, arg_snd) ->
      let varg_fst = eval_expr ictx arg_fst in
      let varg_snd = eval_expr ictx arg_snd in
      Runtime.Ops.eval_binop op varg_fst varg_snd
  | CastE (typ, arg) ->
      let typ = eval_type ictx typ |> eval_simplify_type ictx in
      let varg = eval_expr ictx arg in
      Runtime.Ops.eval_cast typ varg
  | _ ->
      Format.asprintf "(TODO: eval_expr) %a" Syntax.Print.print_expr expr
      |> failwith
