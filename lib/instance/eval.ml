open Syntax.Ast
open Runtime.Context
open Util.Source
module R = Runtime

(* Static evaluation for type simplification,
   Assume: evaluation of bit width should never change the context *)

let rec eval_type (ictx : ICtx.t) (typ : typ) : R.Type.t =
  match typ.it with
  | BoolT -> BoolT
  | AIntT -> AIntT
  | IntT width ->
      let width = eval_expr ictx width |> R.Value.get_num in
      IntT width
  | BitT width ->
      let width = eval_expr ictx width |> R.Value.get_num in
      BitT width
  | VBitT width ->
      let width = eval_expr ictx width |> R.Value.get_num in
      VBitT width
  | StrT -> StrT
  | ErrT -> ErrT
  | NameT { it = Top id; _ } -> ICtx.find_td_glob id.it ictx
  | NameT { it = Bare id; _ } -> ICtx.find_td id.it ictx
  (* (TODO) Handle specialized types *)
  | SpecT (var, _) -> eval_type ictx (NameT var $ no_info)
  | StackT (typ, size) ->
      let typ = eval_type ictx typ in
      let size = eval_expr ictx size |> R.Value.get_num in
      StackT (typ, size)
  | TupleT typs ->
      let typs = List.map (eval_type ictx) typs in
      TupleT typs
  | _ ->
      Format.asprintf "(TODO: eval_type) %a" Syntax.Pp.pp_type typ |> failwith

(* Static evaluation for expressions *)

and eval_expr (ictx : ICtx.t) (expr : expr) : R.Value.t =
  match expr.it with
  | BoolE b -> eval_bool b
  | StrE s -> eval_str s
  | NumE { it = value, encoding; _ } -> eval_num value encoding
  | VarE var -> eval_var ictx var
  | ListE exprs -> eval_list ictx exprs
  | RecordE fields -> eval_record ictx fields
  | UnE (unop, expr) -> eval_unop ictx unop expr
  | BinE (binop, expr_fst, expr_snd) -> eval_binop ictx binop expr_fst expr_snd
  | TernE (cond, expr_tru, expr_fls) -> eval_ternop ictx cond expr_tru expr_fls
  | CastE (typ, expr) -> eval_cast ictx typ expr
  | MaskE _ -> eval_mask ictx
  | RangeE _ -> eval_range ictx
  | ArrAccE (base, idx) -> eval_arr_acc ictx base idx
  | BitAccE (base, idx_lo, idx_hi) -> eval_bitstring_acc ictx base idx_lo idx_hi
  | TypeAccE (var, member) -> eval_type_acc ictx var member
  | ErrAccE member -> eval_error_acc ictx member
  | ExprAccE (base, member) -> eval_expr_acc ictx base member
  | CallE (func, targs, args) -> eval_call_as_expr ictx func targs args
  | InstE _ ->
      Format.eprintf
        "(eval_expr) Instantiation expression should have been evaluated in \
         instantiation.";
      assert false

and eval_bool (b : bool) : R.Value.t = BoolV b
and eval_str (s : string) : R.Value.t = StrV s

and eval_num (value : Bigint.t) (encoding : (Bigint.t * bool) option) :
    R.Value.t =
  match encoding with
  | Some (width, signed) ->
      if signed then R.Value.IntV (width, value) else R.Value.BitV (width, value)
  | None -> AIntV value

and eval_var (ictx : ICtx.t) (var : var) : R.Value.t =
  match var.it with
  | Top id -> ICtx.find_var_glob id.it ictx
  | Bare id -> ICtx.find_var id.it ictx

and eval_list (ictx : ICtx.t) (exprs : expr list) : R.Value.t =
  let values = eval_exprs ictx exprs in
  R.Value.TupleV values

and eval_record (ictx : ICtx.t) (fields : (member * expr) list) : R.Value.t =
  let fields, exprs = List.split fields in
  let values = eval_exprs ictx exprs in
  let fields = List.map2 (fun field value -> (field.it, value)) fields values in
  R.Value.StructV fields

and eval_unop (ictx : ICtx.t) (op : unop) (expr : expr) : R.Value.t =
  let value = eval_expr ictx expr in
  Runtime.Ops.eval_unop op value

and eval_binop (ictx : ICtx.t) (op : binop) (expr_fst : expr) (expr_snd : expr)
    : R.Value.t =
  let values = eval_exprs ictx [ expr_fst; expr_snd ] in
  let value_fst, value_snd = (List.nth values 0, List.nth values 1) in
  Runtime.Ops.eval_binop op value_fst value_snd

and eval_ternop (ictx : ICtx.t) (expr_cond : expr) (expr_tru : expr)
    (expr_fls : expr) : R.Value.t =
  let value_cond = eval_expr ictx expr_cond in
  let cond = match value_cond with BoolV b -> b | _ -> assert false in
  let expr = if cond then expr_tru else expr_fls in
  eval_expr ictx expr

and eval_cast (ictx : ICtx.t) (typ : typ) (expr : expr) : R.Value.t =
  let typ =
    let typ = eval_type ictx typ in
    ICtx.simplify_td typ ictx
  in
  let value = eval_expr ictx expr in
  Runtime.Ops.eval_cast typ value

and eval_mask (_ictx : ICtx.t) : R.Value.t = assert false
and eval_range (_ictx : ICtx.t) : R.Value.t = assert false

and eval_arr_acc (ictx : ICtx.t) (base : expr) (idx : expr) : R.Value.t =
  let values = eval_exprs ictx [ base; idx ] in
  let value_base, value_idx = (List.nth values 0, List.nth values 1) in
  match value_base with
  (* (TODO) Insert bounds checking *)
  | StackV (values, _, _) ->
      let idx = R.Value.get_num value_idx |> Bigint.to_int |> Option.get in
      List.nth values idx
  | _ -> assert false

and eval_bitstring_acc (ictx : ICtx.t) (base : expr) (idx_hi : expr)
    (idx_lo : expr) : R.Value.t =
  let values = eval_exprs ictx [ base; idx_hi; idx_lo ] in
  let value_base, value_hi, value_lo =
    (List.nth values 0, List.nth values 1, List.nth values 2)
  in
  Runtime.Ops.eval_bitstring_access value_base value_hi value_lo

and eval_type_acc (ictx : ICtx.t) (var : var) (member : member) : R.Value.t =
  let typ =
    match var.it with
    | Top id -> ICtx.find_td_glob id.it ictx
    | Bare id -> ICtx.find_td id.it ictx
  in
  match typ with
  | EnumT (id, members) ->
      if List.mem member.it members then EnumFieldV (id, member.it)
      else assert false
  | _ -> assert false

and eval_error_acc (ictx : ICtx.t) (member : member) : R.Value.t =
  let id = "error." ^ member.it in
  match ICtx.find_var_glob_opt id ictx with
  | Some (ErrV _ as value) -> value
  | _ -> assert false

and eval_builtin_stack_acc (_ictx : ICtx.t) (values : R.Value.t list)
    (next : Bigint.t) (_size : Bigint.t) (member : member) =
  match member.it with
  (* hs.next: produces a reference to the element with index hs.nextIndex in the stack.
     May only be used in a parser. If the stack's nextIndex counter is greater than or equal to size,
     then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
     If hs is an l-value, then hs.next is also an l-value. *)
  | "next" ->
      let idx = Bigint.to_int next |> Option.get in
      List.nth values idx
  (* hs.last: produces a reference to the element with index hs.nextIndex - 1 in the stack, if such an element exists.
     May only be used in a parser. If the nextIndex counter is less than 1, or greater than size,
     then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
     Unlike hs.next, the resulting reference is never an l-value. *)
  | "last" ->
      let idx = (Bigint.to_int next |> Option.get) - 1 in
      List.nth values idx
  | _ ->
      Format.eprintf
        "(eval_builtin_stack_acc) %s member access not supported for header \
         stack\n"
        member.it;
      assert false

and eval_expr_acc (ictx : ICtx.t) (base : expr) (member : member) : R.Value.t =
  let value_base = eval_expr ictx base in
  match value_base with
  | HeaderV (_, fields) | StructV fields -> List.assoc member.it fields
  | RefV path -> R.Value.RefV (path @ [ member.it ])
  | StackV (values, next, size) ->
      eval_builtin_stack_acc ictx values next size member
  | _ ->
      Format.eprintf "(TODO: eval_expr_acc) %a.%s\n" R.Value.pp value_base
        member.it;
      assert false

and eval_call_as_expr (_ictx : ICtx.t) (_func : expr) (_targs : typ list)
    (_args : arg list) : R.Value.t =
  assert false

and eval_exprs (ictx : ICtx.t) (exprs : expr list) : R.Value.t list =
  List.map (eval_expr ictx) exprs
