module F = Format
open Domain.Dom
module L = Lang.Ast
open Il.Ast
module Num = Runtime_static.Vdomain.Num
module Value = Runtime_static.Vdomain.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Numerics = Runtime_static.Numerics
module LValue = Runtime_dynamic.Lvalue
module Table = Runtime_dynamic.Table
module State = Runtime_dynamic.State
module Func = Runtime_dynamic.Func
module Obj = Runtime_dynamic.Object
module Envs = Runtime_dynamic.Envs
module TEnv = Envs.TEnv
module SEnv = Envs.SEnv
module FEnv = Envs.FEnv
module Sto = Envs.Sto
open Driver
open Util.Source
open Util.Error

let error_no_info = error_interp_no_info
let error_pass_info = error_interp_pass_info
let check = check_interp

module Make (Arch : ARCH) : INTERP = struct
  (* Global store *)

  let sto = ref Sto.empty
  let init (_sto : Sto.t) : unit = sto := _sto
  let update (oid : OId.t) (obj : Obj.t) : unit = sto := Sto.add oid obj !sto

  (* Call kinds
     (i) InterGlobal: call that inherits only global context
     (ii) InterBlock: call that inherits global context,
                      and the block context of the callee object
     (iii) IntraBlock: call that inherits global context,
                       and the block context of the caller object *)

  type callkind =
    | InterGlobal of {
        fid : FId.t;
        func : Func.t;
        targs : targ list;
        args : arg list;
        args_default : id' list;
      }
    | InterBlock of {
        oid : OId.t;
        fid : FId.t;
        tenv_block : TEnv.t;
        venv_block : VEnv.t;
        func : Func.t;
        targs : targ list;
        args : arg list;
        args_default : id' list;
      }
    | IntraBlock of {
        oid : OId.t;
        fid : FId.t;
        func : Func.t;
        targs : targ list;
        args : arg list;
        args_default : id' list;
      }

  (* L-value evaluation *)

  let rec eval_lvalue_of_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg) :
      Ctx.t * LValue.t option =
    try eval_lvalue_of_arg' cursor ctx arg.it
    with InterpErr _ as err -> error_pass_info arg.at err

  and eval_lvalue_of_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg') :
      Ctx.t * LValue.t option =
    match arg with
    | L.ExprA expr | L.NameA (_, Some expr) ->
        let ctx, lvalue = eval_lvalue_of_expr cursor ctx expr in
        (ctx, Some lvalue)
    | L.NameA (_, None) | L.AnyA -> (ctx, None)

  and eval_lvalue_of_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr) :
      Ctx.t * LValue.t =
    try eval_lvalue_of_expr' cursor ctx expr.it
    with InterpErr _ as err -> error_pass_info expr.at err

  and eval_lvalue_of_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr') :
      Ctx.t * LValue.t =
    match expr with
    | VarE { var } ->
        let lvalue = LValue.VarLV var.it in
        (ctx, lvalue)
    | BitAccE { expr_base; value_lo; value_hi } ->
        let ctx, lvalue_base = eval_lvalue_of_expr cursor ctx expr_base in
        let lvalue = LValue.BitAccLV (lvalue_base, value_lo.it, value_hi.it) in
        (ctx, lvalue)
    | ArrAccE { expr_base; expr_idx } ->
        let ctx, lvalue_base = eval_lvalue_of_expr cursor ctx expr_base in
        let ctx, value_idx = eval_expr cursor ctx expr_idx in
        let lvalue = LValue.ArrAccLV (lvalue_base, value_idx) in
        (ctx, lvalue)
    | ExprAccE { expr_base; member } ->
        let ctx, lvalue_base = eval_lvalue_of_expr cursor ctx expr_base in
        let lvalue = LValue.ExprAccLV (lvalue_base, member.it) in
        (ctx, lvalue)
    | _ ->
        F.asprintf "(eval_lvalue_of_expr') %a cannot be an l-value"
          (Il.Pp.pp_expr' ~level:0) expr
        |> error_no_info

  and eval_lvalue (cursor : Ctx.cursor) (ctx : Ctx.t) (lvalue : LValue.t) :
      Value.t =
    match lvalue with
    | VarLV var -> Ctx.find Ctx.find_value_opt cursor (var $ no_info) ctx
    | BitAccLV (lvalue_base, idx_lo, idx_hi) ->
        let value_base = eval_lvalue cursor ctx lvalue_base in
        Numerics.eval_bitstring_access value_base idx_hi idx_lo
    | ArrAccLV (lvalue_base, idx) -> (
        let value_base = eval_lvalue cursor ctx lvalue_base in
        let idx = idx |> Value.get_num in
        match value_base with
        | TupleV values -> idx |> Bigint.to_int_exn |> List.nth values
        | StackV (values, _, size) when Bigint.(idx >= size) ->
            (* (TODO) What if the stack is of size zero? *)
            let value = List.hd values in
            let value = Value.set_invalid value in
            value
        | StackV (values, _, _) -> idx |> Bigint.to_int_exn |> List.nth values
        | _ ->
            F.asprintf "(eval_lvalue) %a cannot be indexed" (Value.pp ~level:0)
              value_base
            |> error_no_info)
    | ExprAccLV (lvalue_base, member) -> (
        let value_base = eval_lvalue cursor ctx lvalue_base in
        match value_base with
        | StackV (values, idx, size) -> (
            match member with
            | "size" -> Value.FBitV (Bigint.of_int 32, size)
            | "next" -> idx |> Bigint.to_int_exn |> List.nth values
            | "last" ->
                Bigint.(idx - one) |> Bigint.to_int_exn |> List.nth values
            | "lastIndex" -> Value.FBitV (Bigint.of_int 32, Bigint.(idx - one))
            | _ ->
                F.asprintf "(eval_lvalue) invalid member %s for header stack"
                  member
                |> error_no_info)
        | StructV (_, fields) | HeaderV (_, _, fields) | UnionV (_, fields) ->
            List.assoc member fields
        | RefV path -> Value.RefV (path @ [ member ])
        | _ ->
            F.asprintf "(TODO: eval_lvalue) %a" (Value.pp ~level:0) value_base
            |> error_no_info)

  and eval_lvalue_write (cursor : Ctx.cursor) (ctx : Ctx.t) (lvalue : LValue.t)
      (value_rhs : Value.t) : Ctx.t =
    match lvalue with
    | VarLV var ->
        Ctx.update Ctx.update_value_opt cursor (var $ no_info) value_rhs ctx
    | BitAccLV (lvalue_base, value_lo, value_hi) -> (
        let value_base = eval_lvalue cursor ctx lvalue_base in
        match (value_base, value_rhs) with
        | FBitV (width, value), FBitV (_, value_rhs) ->
            let value_lo = value_lo |> Value.get_num in
            let value_hi = value_hi |> Value.get_num in
            let value_rhs = Bigint.(value_rhs lsl to_int_exn value_lo) in
            let mask_hi =
              let mask_hi = Num.power_of_two Bigint.(value_hi + one) in
              Bigint.(mask_hi - one)
            in
            let mask_lo =
              let mask_lo = Num.power_of_two value_lo in
              Bigint.(mask_lo - one)
            in
            let mask = Bigint.(lnot (mask_hi lxor mask_lo)) in
            let value = Bigint.(value land mask lxor value_rhs) in
            let value_base = Value.FBitV (width, value) in
            eval_lvalue_write cursor ctx lvalue_base value_base
        | _ ->
            F.asprintf "(eval_lvalue_write) %a cannot be sliced"
              (Value.pp ~level:0) value_base
            |> error_no_info)
    | ArrAccLV (lvalue_base, idx_target) -> (
        let value_base = eval_lvalue cursor ctx lvalue_base in
        let idx_target = idx_target |> Value.get_num in
        match value_base with
        | StackV (_, _, size) when Bigint.(idx_target >= size) -> ctx
        | StackV (values_stack, idx_stack, size) ->
            let idx_target = idx_target |> Bigint.to_int_exn in
            let values_stack =
              List.mapi
                (fun idx value_stack ->
                  if idx = idx_target then value_rhs else value_stack)
                values_stack
            in
            let value_base = Value.StackV (values_stack, idx_stack, size) in
            eval_lvalue_write cursor ctx lvalue_base value_base
        | _ ->
            F.asprintf "(TODO: eval_lvalue_write) %a" (LValue.pp ~level:0)
              lvalue
            |> error_no_info)
    | ExprAccLV (lvalue_base, member_target) -> (
        let value_base = eval_lvalue cursor ctx lvalue_base in
        match value_base with
        | StackV (values_stack, idx_stack, size) when member_target = "next" ->
            let idx_stack = idx_stack |> Bigint.to_int_exn in
            let values_stack =
              List.mapi
                (fun idx value_stack ->
                  if idx = idx_stack then value_rhs else value_stack)
                values_stack
            in
            let idx_stack = idx_stack + 1 |> Bigint.of_int in
            let value_base = Value.StackV (values_stack, idx_stack, size) in
            eval_lvalue_write cursor ctx lvalue_base value_base
        | StructV _ ->
            let value_base =
              Value.update_struct_field value_base member_target value_rhs
            in
            eval_lvalue_write cursor ctx lvalue_base value_base
        | HeaderV _ ->
            let value_base =
              Value.update_header_field value_base member_target value_rhs
            in
            eval_lvalue_write cursor ctx lvalue_base value_base
        | UnionV _ ->
            let value_base =
              Value.update_union_field value_base member_target value_rhs
            in
            eval_lvalue_write cursor ctx lvalue_base value_base
        | _ ->
            F.asprintf "(TODO: eval_lvalue_write) %a" (LValue.pp ~level:0)
              lvalue
            |> error_no_info)

  (* Argument evaluation *)

  and eval_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg) : Ctx.t * Value.t
      =
    try eval_arg' cursor ctx arg.it
    with InterpErr _ as err -> error_pass_info arg.at err

  and eval_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg') :
      Ctx.t * Value.t =
    match arg with
    | L.ExprA expr | L.NameA (_, Some expr) -> eval_expr cursor ctx expr
    | _ -> F.asprintf "(TODO: eval_arg) %a" Il.Pp.pp_arg' arg |> error_no_info

  (* Keyset expression evaluation *)

  and eval_keyset_match (value_key : Value.t) (value_set : Value.t) : bool =
    match value_set with
    | SetV (`Singleton value) -> Numerics.eval_binop_eq value_key value
    | SetV (`Mask (value_base, value_mask)) ->
        let value_base = Numerics.eval_binop_bitand value_base value_mask in
        let value_key = Numerics.eval_binop_bitand value_key value_mask in
        Numerics.eval_binop_eq value_base value_key
    | SetV (`Range (value_lb, value_ub)) ->
        let lb = Numerics.eval_binop_le value_lb value_key in
        let ub = Numerics.eval_binop_le value_key value_ub in
        lb && ub
    | _ ->
        F.asprintf "(eval_keyset_match) %a must be a set value"
          (Value.pp ~level:0) value_set
        |> error_no_info

  (* Expression evaluation *)

  and eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr) :
      Ctx.t * Value.t =
    try eval_expr' cursor ctx expr.it
    with InterpErr _ as err -> error_pass_info expr.at err

  and eval_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr') :
      Ctx.t * Value.t =
    let wrap_value value = (ctx, value) in
    match expr with
    | ValueE { value } -> value.it |> wrap_value
    | VarE { var } -> eval_var_expr cursor ctx var |> wrap_value
    | SeqE { exprs } -> eval_seq_expr ~default:false cursor ctx exprs
    | SeqDefaultE { exprs } -> eval_seq_expr ~default:true cursor ctx exprs
    | RecordE { fields } -> eval_record_expr ~default:false cursor ctx fields
    | RecordDefaultE { fields } ->
        eval_record_expr ~default:true cursor ctx fields
    | DefaultE -> Value.DefaultV |> wrap_value
    | UnE { unop; expr } -> eval_unop_expr cursor ctx unop expr
    | BinE { binop; expr_l; expr_r } ->
        eval_binop_expr cursor ctx binop expr_l expr_r
    | TernE { expr_cond; expr_then; expr_else } ->
        eval_ternop_expr cursor ctx expr_cond expr_then expr_else
    | CastE { typ; expr } -> eval_cast_expr cursor ctx typ expr
    | MaskE { expr_base; expr_mask } ->
        eval_mask_expr cursor ctx expr_base expr_mask
    | RangeE { expr_lb; expr_ub } -> eval_range_expr cursor ctx expr_lb expr_ub
    | SelectE { exprs_select; cases } ->
        eval_select_expr cursor ctx exprs_select cases
    | ArrAccE { expr_base; expr_idx } ->
        eval_array_acc_expr cursor ctx expr_base expr_idx
    | BitAccE { expr_base; value_lo; value_hi } ->
        eval_bitstring_acc_expr cursor ctx expr_base value_lo value_hi
    | ExprAccE { expr_base; member } ->
        eval_expr_acc_expr cursor ctx expr_base member
    | CallFuncE { var_func; targs; args } ->
        eval_call_func_expr cursor ctx var_func targs args
    | CallMethodE { expr_base; member; targs; args } ->
        eval_call_method_expr cursor ctx expr_base member targs args
    | CallTypeE _ ->
        F.asprintf "(TODO: eval_expr) %a" (Il.Pp.pp_expr' ~level:0) expr
        |> error_no_info
    | InstE _ ->
        F.asprintf
          "(eval_expr) instantiation should have been handled by the \
           instantiation phase"
        |> error_no_info

  and eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (exprs : expr list) :
      Ctx.t * Value.t list =
    List.fold_left
      (fun (ctx, values) expr ->
        let ctx, value = eval_expr cursor ctx expr in
        (ctx, values @ [ value ]))
      (ctx, []) exprs

  and eval_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : var) : Value.t =
    let value = Ctx.find Ctx.find_value_opt cursor var ctx in
    value

  and eval_seq_expr ~(default : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
      (exprs : expr list) : Ctx.t * Value.t =
    let ctx, values = eval_exprs cursor ctx exprs in
    let value =
      if default then Value.SeqDefaultV values else Value.SeqV values
    in
    (ctx, value)

  and eval_record_expr ~(default : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
      (fields : (member * expr) list) : Ctx.t * Value.t =
    let members, exprs = List.split fields in
    let members = List.map it members in
    let ctx, values = eval_exprs cursor ctx exprs in
    let fields = List.combine members values in
    let value =
      if default then Value.RecordDefaultV fields else Value.RecordV fields
    in
    (ctx, value)

  and eval_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : unop)
      (expr : expr) : Ctx.t * Value.t =
    let ctx, value = eval_expr cursor ctx expr in
    let value = Numerics.eval_unop unop value in
    (ctx, value)

  and eval_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : binop)
      (expr_l : expr) (expr_r : expr) : Ctx.t * Value.t =
    match binop.it with
    | L.LAndOp ->
        let ctx, value_l = eval_expr cursor ctx expr_l in
        let cond = Value.get_bool value_l in
        if cond then eval_expr cursor ctx expr_r else (ctx, value_l)
    | L.LOrOp ->
        let ctx, value_l = eval_expr cursor ctx expr_l in
        let cond = Value.get_bool value_l in
        if cond then (ctx, value_l) else eval_expr cursor ctx expr_r
    | _ ->
        let ctx, value_l = eval_expr cursor ctx expr_l in
        let ctx, value_r = eval_expr cursor ctx expr_r in
        let value = Numerics.eval_binop binop value_l value_r in
        (ctx, value)

  and eval_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
      (expr_then : expr) (expr_else : expr) : Ctx.t * Value.t =
    let ctx, value_cond = eval_expr cursor ctx expr_cond in
    let cond = Value.get_bool value_cond in
    let expr = if cond then expr_then else expr_else in
    eval_expr cursor ctx expr

  and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : typ)
      (expr : expr) : Ctx.t * Value.t =
    let ctx, value = eval_expr cursor ctx expr in
    let value = Numerics.eval_cast typ.it value in
    (ctx, value)

  and eval_mask_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (expr_mask : expr) : Ctx.t * Value.t =
    let ctx, value_base = eval_expr cursor ctx expr_base in
    let ctx, value_mask = eval_expr cursor ctx expr_mask in
    let value = Value.SetV (`Mask (value_base, value_mask)) in
    (ctx, value)

  and eval_range_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_lb : expr)
      (expr_ub : expr) : Ctx.t * Value.t =
    let ctx, value_lb = eval_expr cursor ctx expr_lb in
    let ctx, value_ub = eval_expr cursor ctx expr_ub in
    let value = Value.SetV (`Range (value_lb, value_ub)) in
    (ctx, value)

  and eval_select_match_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
      (value_key : Value.t) (keyset : keyset) : Ctx.t * bool =
    match keyset.it with
    | ExprK expr ->
        let ctx, value = eval_expr cursor ctx expr in
        let matched = eval_keyset_match value_key value in
        (ctx, matched)
    | DefaultK | AnyK -> (ctx, true)

  and eval_select_match (cursor : Ctx.cursor) (ctx : Ctx.t)
      (values_key : Value.t list) (case : select_case) :
      Ctx.t * state_label option =
    let keysets, label = case.it in
    let ctx, matched =
      match (values_key, keysets) with
      | [ value_key ], [ keyset ] ->
          eval_select_match_keyset cursor ctx value_key keyset
      | _, [ keyset ] ->
          let value_key = Value.SeqV values_key in
          eval_select_match_keyset cursor ctx value_key keyset
      | values_key, keysets ->
          check
            (List.length values_key = List.length keysets)
            "(eval_select_match) number of select keys must match the number \
             of keysets";
          List.fold_left2
            (fun (ctx, matched) value_key keyset ->
              if not matched then (ctx, matched)
              else eval_select_match_keyset cursor ctx value_key keyset)
            (ctx, true) values_key keysets
    in
    if matched then (ctx, Some label) else (ctx, None)

  and eval_select_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (exprs_select : expr list) (cases : select_case list) : Ctx.t * Value.t =
    let ctx, values_key = eval_exprs cursor ctx exprs_select in
    let ctx, label =
      List.fold_left
        (fun (ctx, label) case ->
          if Option.is_some label then (ctx, label)
          else eval_select_match cursor ctx values_key case)
        (ctx, None) cases
    in
    check (Option.is_some label) "(eval_select_expr) no matching case found";
    let label = Option.get label in
    let value = Value.StateV label.it in
    (ctx, value)

  and eval_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (expr_idx : expr) : Ctx.t * Value.t =
    let ctx, value_base = eval_expr cursor ctx expr_base in
    let ctx, value_idx = eval_expr cursor ctx expr_idx in
    let idx = value_idx |> Value.get_num in
    match value_base with
    | TupleV values ->
        let value = idx |> Bigint.to_int_exn |> List.nth values in
        (ctx, value)
    | StackV (values, _, size) when Bigint.(idx >= size) ->
        (* (TODO) What if the stack is of size zero? *)
        let value = List.hd values in
        let value = Value.set_invalid value in
        (ctx, value)
    | StackV (values, _, _) ->
        let value = idx |> Bigint.to_int_exn |> List.nth values in
        (ctx, value)
    | _ ->
        F.asprintf "(eval_array_expr) %a cannot be indexed" (Value.pp ~level:0)
          value_base
        |> error_no_info

  and eval_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (value_lo : value) (value_hi : value) : Ctx.t * Value.t
      =
    let ctx, value_base = eval_expr cursor ctx expr_base in
    let value =
      Numerics.eval_bitstring_access value_base value_hi.it value_lo.it
    in
    (ctx, value)

  and eval_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (member : member) : Ctx.t * Value.t =
    let ctx, value_base = eval_expr cursor ctx expr_base in
    let value =
      match value_base with
      | StackV (values, idx, size) -> (
          match member.it with
          | "size" -> Value.FBitV (Bigint.of_int 32, size)
          | "next" -> idx |> Bigint.to_int_exn |> List.nth values
          | "last" -> Bigint.(idx - one) |> Bigint.to_int_exn |> List.nth values
          | "lastIndex" -> Value.FBitV (Bigint.of_int 32, Bigint.(idx - one))
          | _ ->
              F.asprintf
                "(eval_expr_acc_expr) invalid member %a for header stack"
                Il.Pp.pp_member member
              |> error_no_info)
      | StructV (_, fields)
      | HeaderV (_, _, fields)
      | UnionV (_, fields)
      | TableStructV (_, fields) ->
          List.assoc member.it fields
      | RefV path -> Value.RefV (path @ [ member.it ])
      | _ ->
          F.asprintf "(TODO: eval_expr_acc_expr) %a" (Value.pp ~level:0)
            value_base
          |> error_no_info
    in
    (ctx, value)

  and eval_call_func_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Value.t =
    let ctx, sign = eval_func_call cursor ctx var_func targs args in
    match sign with
    | Ret (Some value) -> (ctx, value)
    | _ ->
        "(eval_call_func_expr) function call as an expression must return a \
         value" |> error_no_info

  and eval_call_method_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (member : member) (targs : typ list) (args : arg list)
      : Ctx.t * Value.t =
    let ctx, sign = eval_method_call cursor ctx expr_base member targs args in
    match sign with
    | Ret (Some value) -> (ctx, value)
    | _ ->
        "(eval_call_method_expr) method call as an expression must return a \
         value" |> error_no_info

  (* Statement evaluation *)

  and eval_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t) (stmt : stmt)
      : Ctx.t * Sig.t =
    try eval_stmt' cursor ctx sign stmt.it
    with InterpErr _ as err -> error_pass_info stmt.at err

  and eval_stmt' (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (stmt : stmt') : Ctx.t * Sig.t =
    let cont (sign : Sig.t) eval_stmt_cont =
      match sign with Cont -> eval_stmt_cont () | _ -> (ctx, sign)
    in
    match stmt with
    | EmptyS -> cont sign (fun () -> (ctx, Sig.Cont))
    | AssignS { expr_l; expr_r } ->
        cont sign (fun () -> eval_assign_stmt cursor ctx expr_l expr_r)
    | SwitchS { expr_switch; cases } ->
        cont sign (fun () -> eval_switch_stmt cursor ctx expr_switch cases)
    | IfS { expr_cond; stmt_then; stmt_else } ->
        cont sign (fun () ->
            eval_if_stmt cursor ctx expr_cond stmt_then stmt_else)
    | BlockS { block } -> cont sign (fun () -> eval_block_stmt cursor ctx block)
    | ExitS -> cont sign (fun () -> (ctx, Sig.Exit))
    | RetS { expr_ret } ->
        cont sign (fun () -> eval_return_stmt cursor ctx expr_ret)
    | CallFuncS { var_func; targs; args } ->
        cont sign (fun () -> eval_call_func_stmt cursor ctx var_func targs args)
    | CallMethodS { expr_base; member; targs; args } ->
        cont sign (fun () ->
            eval_call_method_stmt cursor ctx expr_base member targs args)
    | CallInstS _ ->
        F.asprintf
          "(eval_stmt) instantiation should have been handled by the \
           instantiation phase"
        |> error_no_info
    | TransS { expr_label } ->
        cont sign (fun () -> eval_trans_stmt cursor ctx expr_label)
    | DeclS { decl } -> cont sign (fun () -> eval_decl_stmt cursor ctx decl)

  and eval_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (stmts : stmt list) : Ctx.t * Sig.t =
    List.fold_left
      (fun (ctx, sign) stmt -> eval_stmt cursor ctx sign stmt)
      (ctx, sign) stmts

  and eval_assign_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_l : expr)
      (expr_r : expr) : Ctx.t * Sig.t =
    let ctx, lvalue = eval_lvalue_of_expr cursor ctx expr_l in
    let ctx, value_r = eval_expr cursor ctx expr_r in
    let ctx = eval_lvalue_write cursor ctx lvalue value_r in
    (ctx, Cont)

  and eval_switch_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_switch : expr)
      (cases : switch_case list) : Ctx.t * Sig.t =
    let typ_switch = expr_switch.note.typ in
    let ctx, value_switch = eval_expr cursor ctx expr_switch in
    match typ_switch with
    | TableEnumT (id_table, _) ->
        let id_table = String.sub id_table 12 (String.length id_table - 12) in
        let id_table = String.sub id_table 0 (String.length id_table - 1) in
        eval_switch_table_stmt cursor ctx id_table value_switch cases
    | _ -> eval_switch_general_stmt cursor ctx value_switch cases

  and eval_switch_table_match_label (id_table : id') (value_switch : Value.t)
      (label : switch_label) : bool =
    match label.it with
    | ExprL { it = VarE { var = { it = Current id_action; _ } }; _ } -> (
        match value_switch with
        | TableEnumFieldV (id_enum, member_enum) ->
            let id_table = "action_list(" ^ id_table ^ ")" in
            id_enum = id_table && member_enum = id_action.it
        | _ -> assert false)
    | ExprL _ ->
        F.asprintf
          "(eval_switch_table_match_label) switch label must be an action name"
        |> error_no_info
    | DefaultL -> true

  and eval_switch_table_match (id_table : id') (fallthrough : bool)
      (value_switch : Value.t) (case : switch_case) : bool * block option =
    match case.it with
    | MatchC (_, block) when fallthrough -> (false, Some block)
    | MatchC (label, block) ->
        let matched =
          eval_switch_table_match_label id_table value_switch label
        in
        if matched then (false, Some block) else (false, None)
    | FallC label ->
        let matched =
          eval_switch_table_match_label id_table value_switch label
        in
        if matched then (true, None) else (false, None)

  and eval_switch_table_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (id_table : id') (value_switch : Value.t) (cases : switch_case list) :
      Ctx.t * Sig.t =
    let _, block =
      List.fold_left
        (fun (fallthrough, block) case ->
          if Option.is_some block then (false, block)
          else eval_switch_table_match id_table fallthrough value_switch case)
        (false, None) cases
    in
    match block with
    | Some block -> eval_block_stmt cursor ctx block
    | None -> (ctx, Cont)

  and eval_switch_general_match_label (cursor : Ctx.cursor) (ctx : Ctx.t)
      (value_switch : Value.t) (label : switch_label) : Ctx.t * bool =
    match label.it with
    | ExprL expr ->
        let ctx, value = eval_expr cursor ctx expr in
        let matched = Numerics.eval_binop_eq value_switch value in
        (ctx, matched)
    | DefaultL -> (ctx, true)

  and eval_switch_general_match (cursor : Ctx.cursor) (ctx : Ctx.t)
      (fallthrough : bool) (value_switch : Value.t) (case : switch_case) :
      Ctx.t * bool * block option =
    match case.it with
    | MatchC (_, block) when fallthrough -> (ctx, false, Some block)
    | MatchC (label, block) ->
        let ctx, matched =
          eval_switch_general_match_label cursor ctx value_switch label
        in
        if matched then (ctx, false, Some block) else (ctx, false, None)
    | FallC label ->
        let ctx, matched =
          eval_switch_general_match_label cursor ctx value_switch label
        in
        if matched then (ctx, true, None) else (ctx, false, None)

  and eval_switch_general_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (value_switch : Value.t) (cases : switch_case list) : Ctx.t * Sig.t =
    let ctx, _, block =
      List.fold_left
        (fun (ctx, fallthrough, block) case ->
          if Option.is_some block then (ctx, false, block)
          else
            eval_switch_general_match cursor ctx fallthrough value_switch case)
        (ctx, false, None) cases
    in
    match block with
    | Some block -> eval_block_stmt cursor ctx block
    | None -> (ctx, Cont)

  and eval_if_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
      (stmt_then : stmt) (stmt_else : stmt) : Ctx.t * Sig.t =
    let ctx, value_cond = eval_expr cursor ctx expr_cond in
    let cond = Value.get_bool value_cond in
    let stmt = if cond then stmt_then else stmt_else in
    eval_stmt cursor ctx Sig.Cont stmt

  and eval_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (block : block) :
      Ctx.t * Sig.t =
    let stmts, _annos = block.it in
    let ctx = Ctx.enter_frame ctx in
    let ctx, sign = eval_stmts cursor ctx Sig.Cont stmts in
    let ctx = Ctx.exit_frame ctx in
    (ctx, sign)

  and eval_return_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_ret : expr option) : Ctx.t * Sig.t =
    let ctx, sign_ret =
      match expr_ret with
      | Some expr_ret ->
          let ctx, value = eval_expr cursor ctx expr_ret in
          (ctx, Sig.Ret (Some value))
      | None -> (ctx, Sig.Ret None)
    in
    (ctx, sign_ret)

  and eval_call_func_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Sig.t =
    let ctx, sign = eval_func_call cursor ctx var_func targs args in
    match sign with
    | Cont | Ret _ | Trans `Accept -> (ctx, Cont)
    | Trans (`Reject value) -> (ctx, Trans (`Reject value))
    | Trans (`State _) ->
        F.asprintf
          "(eval_call_func_stmt) function call should not result in a state \
           transition"
        |> error_no_info
    | Exit -> (ctx, Exit)

  and eval_call_method_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (member : member) (targs : typ list) (args : arg list)
      : Ctx.t * Sig.t =
    let ctx, sign = eval_method_call cursor ctx expr_base member targs args in
    match sign with
    | Cont | Ret _ | Trans `Accept -> (ctx, Cont)
    | Trans (`Reject value) -> (ctx, Trans (`Reject value))
    | Trans (`State _) ->
        F.asprintf
          "(eval_call_method_stmt) method call should not result in a state \
           transition"
        |> error_no_info
    | Exit -> (ctx, Exit)

  and eval_trans_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_label : expr) :
      Ctx.t * Sig.t =
    let ctx, value_label = eval_expr cursor ctx expr_label in
    let label = Value.get_state value_label in
    let sign = Sig.Trans (`State label) in
    (ctx, sign)

  and eval_decl_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl) :
      Ctx.t * Sig.t =
    let ctx = eval_decl cursor ctx decl in
    (ctx, Cont)

  (* Declaration evaluation *)

  and eval_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl) : Ctx.t =
    try eval_decl' cursor ctx decl.it
    with InterpErr _ as err -> error_pass_info decl.at err

  and eval_decl' (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl') : Ctx.t =
    match decl with
    | ConstD { id; typ; value; annos } ->
        eval_const_decl cursor ctx id typ value annos
    | VarD { id; typ; init; annos } ->
        eval_var_decl cursor ctx id typ init annos
    | _ ->
        F.asprintf
          "(eval_decl) %a should have been handled by the instantiation phase"
          (Il.Pp.pp_decl' ~level:0) decl
        |> error_no_info

  and eval_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : decl list) : Ctx.t
      =
    List.fold_left (eval_decl cursor) ctx decls

  and eval_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (_typ : typ)
      (value : value) (_annos : anno list) : Ctx.t =
    Ctx.add_value cursor id.it value.it ctx

  and eval_var_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (typ : typ)
      (init : expr option) (_annos : anno list) : Ctx.t =
    let ctx, value =
      match init with
      | Some expr -> eval_expr cursor ctx expr
      | None ->
          let value =
            Ctx.resolve_typ cursor typ.it ctx |> Numerics.eval_default
          in
          (ctx, value)
    in
    Ctx.add_value cursor id.it value ctx

  (* Parser state machine evaluation *)

  and eval_parser_state_machine (cursor : Ctx.cursor) (ctx : Ctx.t)
      (sign : Sig.t) : Ctx.t * Sig.t =
    assert (cursor = Ctx.Block);
    match sign with
    | Trans (`State "accept") -> (ctx, Trans `Accept)
    | Trans (`State "reject") -> (ctx, Trans (`Reject (Value.ErrV "NoError")))
    | Trans (`State id) ->
        let state = Ctx.find_state cursor id ctx in
        let ctx, sign = eval_parser_state cursor ctx state in
        eval_parser_state_machine cursor ctx sign
    | _ -> (ctx, sign)

  and eval_parser_state (cursor : Ctx.cursor) (ctx : Ctx.t) (state : State.t) :
      Ctx.t * Sig.t =
    assert (cursor = Ctx.Block);
    let ctx, sign =
      let stmt_block = BlockS { block = state } $ no_info in
      eval_stmt Ctx.Local ctx Sig.Cont stmt_block
    in
    match sign with
    | Cont -> (ctx, Sig.Trans (`State "reject"))
    | Trans _ -> (ctx, sign)
    | _ ->
        F.asprintf
          "(eval_parser_state) parser state should result in a state transition"
        |> error_no_info

  (* Table evaluation *)

  and eval_table_match_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
      (table_key : Value.t * match_kind) (keyset : keyset) : Ctx.t * bool =
    let value_key, _match_kind = table_key in
    match keyset.it with
    | ExprK expr ->
        let ctx, value = eval_expr cursor ctx expr in
        let matched = eval_keyset_match value_key value in
        (ctx, matched)
    | DefaultK | AnyK -> (ctx, true)

  and eval_table_match_keysets (cursor : Ctx.cursor) (ctx : Ctx.t)
      (table_keys : (Value.t * match_kind) list) (table_entry : table_entry) :
      Ctx.t * (var * arg list * int option) option =
    let keysets, table_action, priority, _, _ = table_entry.it in
    let priority =
      Option.map
        (fun priority -> priority.it |> Value.get_num |> Bigint.to_int_exn)
        priority
    in
    match (table_keys, keysets) with
    | _, [ { it = L.DefaultK; _ } ] | _, [ { it = L.AnyK; _ } ] ->
        let action =
          let var_action, args_action, _ = table_action.it in
          Some (var_action, args_action, priority)
        in
        (ctx, action)
    | table_keys, keysets ->
        check
          (List.length table_keys = List.length keysets)
          "(eval_table_match_keysets) number of table keys must match the \
           number of keysets";
        let ctx, matched =
          List.fold_left2
            (fun (ctx, matched) table_key keyset ->
              if not matched then (ctx, matched)
              else eval_table_match_keyset cursor ctx table_key keyset)
            (ctx, true) table_keys keysets
        in
        let action =
          if matched then
            let var_action, args_action, _ = table_action.it in
            Some (var_action, args_action, priority)
          else None
        in
        (ctx, action)

  and eval_table_match (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id')
      (largest_priority_wins : bool) (table_keys : (Value.t * match_kind) list)
      (action_default : var * arg list) (table_entries : table_entry list) :
      Ctx.t * Value.t * (var * arg list) =
    let ctx, actions =
      List.fold_left
        (fun (ctx, actions) table_entry ->
          let ctx, action =
            eval_table_match_keysets cursor ctx table_keys table_entry
          in
          let actions = actions @ Option.to_list action in
          (ctx, actions))
        (ctx, []) table_entries
    in
    let action, matched =
      match actions with
      | [] -> (action_default, false)
      | [ (var_action, args_action, _) ] -> ((var_action, args_action), true)
      | _ ->
          let priorities =
            List.map (fun (_, _, priority) -> priority) actions
          in
          check
            (List.for_all Option.is_some priorities)
            "(eval_table_match) cannot tie-break between actions without \
             priorities";
          let actions =
            List.map
              (fun (var_action, args_action, priority) ->
                (var_action, args_action, Option.get priority))
              actions
            |> List.sort (fun (_, _, priority_a) (_, _, priority_b) ->
                   Int.compare priority_a priority_b)
            |> if largest_priority_wins then List.rev else Fun.id
          in
          let var_action, args_action, _ = List.hd actions in
          ((var_action, args_action), true)
    in
    let value =
      let hit = Value.BoolV matched in
      let miss = Value.BoolV (not matched) in
      let action_run =
        let member =
          if matched then
            let var_action, _ = action in
            F.asprintf "%a" Il.Pp.pp_var var_action
          else "NoAction"
        in
        Value.TableEnumFieldV ("action_list(" ^ id ^ ")", member)
      in
      Value.TableStructV
        ( "apply_result(" ^ id ^ ")",
          [ ("hit", hit); ("miss", miss); ("action_run", action_run) ] )
    in
    (ctx, value, action)

  and eval_table (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id')
      (table : Table.t) : Ctx.t * Sig.t =
    (* Evaluate table custom properties *)
    let largest_priority_wins =
      List.find_map
        (fun table_custom ->
          let member, expr, _, _ = table_custom.it in
          if member.it = "largest_priority_wins" then
            eval_expr cursor ctx expr |> snd |> Value.get_bool |> Option.some
          else None)
        table.customs
      |> Option.value ~default:true
    in
    (* Evaluate table keys *)
    let ctx, table_keys =
      List.fold_left
        (fun (ctx, table_keys) table_key ->
          let expr_key, match_kind_key, _annos_key = table_key.it in
          let ctx, value_key = eval_expr cursor ctx expr_key in
          (ctx, table_keys @ [ (value_key, match_kind_key) ]))
        (ctx, []) table.keys
    in
    (* Perform match against table entries *)
    let ctx, value, action =
      let _, table_action_default = table.action_default in
      let var_action_default, args_action_default, _ =
        table_action_default.it
      in
      let action_default = (var_action_default, args_action_default) in
      let _, table_entries = table.entries in
      eval_table_match cursor ctx id largest_priority_wins table_keys
        action_default table_entries
    in
    (* Perform the matched action *)
    let var_action, args_action = action in
    let stmt_call_action =
      CallFuncS { var_func = var_action; targs = []; args = args_action }
      $ no_info
    in
    let ctx, _sign = eval_stmt cursor ctx Sig.Cont stmt_call_action in
    (ctx, Sig.Ret (Some value))

  (* Call evaluation *)

  (* Copy-in/out calling convention *)

  and align_params_with_args (params : param list) (args : arg list)
      (args_default : id' list) :
      param list * arg list * param list * value list =
    let module PMap = Map.Make (String) in
    let params, params_default =
      List.partition
        (fun param ->
          let id, _, _, _, _ = param.it in
          not (List.mem id.it args_default))
        params
    in
    let params_map =
      List.fold_left
        (fun params_map param ->
          let id, _, _, _, _ = param.it in
          PMap.add id.it param params_map)
        PMap.empty params
    in
    let params, args =
      List.fold_left2
        (fun (params, args) param arg ->
          match arg.it with
          | L.ExprA _ | L.AnyA -> (params @ [ param ], args @ [ arg ])
          | L.NameA (id, _) ->
              let param = PMap.find id.it params_map in
              (params @ [ param ], args @ [ arg ]))
        ([], []) params args
    in
    let args_default =
      List.map
        (fun param_default ->
          let _, _, _, value_default, _ = param_default.it in
          Option.get value_default)
        params_default
    in
    (params, args, params_default, args_default)

  and copyin (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (cursor_callee : Ctx.cursor) (ctx_callee : Ctx.t) (params : param list)
      (args : arg list) : Ctx.t * Ctx.t * LValue.t option list =
    let copyin'
        ((ctx_caller, ctx_callee, lvalues) :
          Ctx.t * Ctx.t * LValue.t option list) (param : param) (arg : arg) :
        Ctx.t * Ctx.t * LValue.t option list =
      let id, dir, typ, _, _ = param.it in
      let ctx_caller, value, lvalue =
        match dir.it with
        | No | In ->
            let ctx_caller, value = eval_arg cursor_caller ctx_caller arg in
            (ctx_caller, value, None)
        | InOut ->
            let ctx_caller, lvalue =
              eval_lvalue_of_arg cursor_caller ctx_caller arg
            in
            check (Option.is_some lvalue)
              "(copyin) inout argument cannot be nameless";
            let lvalue = Option.get lvalue in
            let value = eval_lvalue cursor_caller ctx_caller lvalue in
            (ctx_caller, value, Some lvalue)
        | Out ->
            let ctx_caller, lvalue =
              eval_lvalue_of_arg cursor_caller ctx_caller arg
            in
            let value =
              Ctx.resolve_typ cursor_callee typ.it ctx_callee
              |> Numerics.eval_default
            in
            (ctx_caller, value, lvalue)
      in
      let ctx_callee = Ctx.add_value cursor_callee id.it value ctx_callee in
      let lvalues = lvalues @ [ lvalue ] in
      (ctx_caller, ctx_callee, lvalues)
    in
    List.fold_left2 copyin' (ctx_caller, ctx_callee, []) params args

  and copyout (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (cursor_callee : Ctx.cursor) (ctx_callee : Ctx.t) (params : param list)
      (lvalues : LValue.t option list) : Ctx.t =
    let copyout' (ctx_caller : Ctx.t) (param : param) (lvalue : LValue.t option)
        : Ctx.t =
      match lvalue with
      | Some lvalue ->
          let id, _, _, _, _ = param.it in
          let value = Ctx.find_value cursor_callee id.it ctx_callee in
          eval_lvalue_write cursor_caller ctx_caller lvalue value
      | None -> ctx_caller
    in
    List.fold_left2 copyout' ctx_caller params lvalues

  and eval_call (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (callkind : callkind) : Ctx.t * Sig.t =
    match callkind with
    | InterGlobal { fid; func; targs; args; args_default } ->
        eval_call'
          ~pre:(fun (ctx_caller : Ctx.t) -> Ctx.copy Ctx.Global ctx_caller)
          ~post:(fun (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ->
            { ctx_caller with global = ctx_callee.global })
          cursor_caller ctx_caller [] fid func targs args args_default
    | InterBlock
        { oid; fid; tenv_block; venv_block; func; targs; args; args_default } ->
        eval_call'
          ~pre:(fun (ctx_caller : Ctx.t) ->
            let ctx_callee = Ctx.copy Global ctx_caller in
            {
              ctx_callee with
              block =
                { ctx_caller.block with tenv = tenv_block; venv = venv_block };
            })
          ~post:(fun (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ->
            { ctx_caller with global = ctx_callee.global })
          cursor_caller ctx_caller oid fid func targs args args_default
    | IntraBlock { oid; fid; func; targs; args; args_default } ->
        eval_call'
          ~pre:(fun (ctx_caller : Ctx.t) -> Ctx.copy Ctx.Block ctx_caller)
          ~post:(fun (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ->
            { ctx_caller with block = ctx_callee.block })
          cursor_caller ctx_caller oid fid func targs args args_default

  and eval_call' ~pre ~post (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (oid : OId.t) (fid : FId.t) (func : Func.t) (targs : targ list)
      (args : arg list) (args_default : id' list) : Ctx.t * Sig.t =
    match func with
    | BuiltinMethodF (params, lvalue) ->
        eval_builtin_method_call ~pre ~post cursor_caller ctx_caller fid params
          lvalue args args_default
    | ActionF (params, block) ->
        eval_action_call ~pre ~post cursor_caller ctx_caller params targs args
          args_default block
    | FuncF (tparams, params, block) ->
        eval_function_call ~pre ~post cursor_caller ctx_caller tparams params
          targs args args_default block
    | ExternFuncF (tparams, params) ->
        eval_extern_function_call ~pre ~post cursor_caller ctx_caller fid
          tparams params targs args args_default
    | ExternMethodF (tparams, params, None) ->
        eval_extern_method_call ~pre ~post cursor_caller ctx_caller oid fid
          tparams params targs args args_default
    | ExternMethodF (_, _, Some _) ->
        F.asprintf "(TODO: eval_call') %a" (Func.pp ~level:0) func
        |> error_no_info
    | ExternAbstractMethodF _ ->
        F.asprintf "(eval_call') cannot call an abstract method %a" FId.pp fid
        |> error_no_info
    | ParserApplyMethodF (params, decls, senv) ->
        eval_parser_apply_method_call ~pre ~post cursor_caller ctx_caller params
          args args_default decls senv
    | ControlApplyMethodF (params, decls, fenv, block) ->
        eval_control_apply_method_call ~pre ~post cursor_caller ctx_caller
          params args args_default decls fenv block
    | TableApplyMethodF table ->
        eval_table_apply_method_call ~pre ~post cursor_caller ctx_caller oid
          table

  and eval_builtin_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (fid : FId.t) (params : param list)
      (lvalue_base : LValue.t) (args : arg list) (args_default : id' list) :
      Ctx.t * Sig.t =
    let value_base = eval_lvalue cursor_caller ctx_caller lvalue_base in
    let ctx_callee = pre ctx_caller in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign, value_base =
      match (value_base, fid) with
      | StackV (values, idx, size), fid ->
          eval_builtin_stack_method_call Ctx.Local ctx_callee fid values idx
            size
      | HeaderV (id, valid, fields), fid ->
          eval_builtin_header_method_call Ctx.Local ctx_callee fid id valid
            fields
      | UnionV (id, fields), fid ->
          eval_builtin_union_method_call Ctx.Local ctx_callee fid id fields
      | _ ->
          F.asprintf "(eval_builtin_method_call) %a cannot be called on %a"
            FId.pp fid (Value.pp ~level:0) value_base
          |> error_no_info
    in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
    in
    let ctx_caller =
      match value_base with
      | Some value_base ->
          eval_lvalue_write cursor_caller ctx_caller lvalue_base value_base
      | None -> ctx_caller
    in
    (ctx_caller, sign)

  and eval_builtin_stack_method_call (cursor : Ctx.cursor) (ctx : Ctx.t)
      (fid : FId.t) (values : Value.t list) (idx : Bigint.t) (size : Bigint.t) :
      Ctx.t * Sig.t * Value.t option =
    match fid with
    | "push_front", [ ("count", false) ] ->
        let value_count = Ctx.find_value cursor "count" ctx in
        let count = value_count |> Value.get_num in
        let values =
          let count = count |> Bigint.to_int_exn in
          List.init (List.length values) (fun idx ->
              if idx < count then Value.set_invalid (List.nth values idx)
              else List.nth values (idx - count))
        in
        let idx =
          if Bigint.(idx + count > size) then size else Bigint.(idx + count)
        in
        let value_base = Value.StackV (values, idx, size) in
        let sign = Sig.Ret None in
        (ctx, sign, Some value_base)
    | "pop_front", [ ("count", false) ] ->
        let value_count = Ctx.find_value cursor "count" ctx in
        let count = value_count |> Value.get_num in
        let values =
          let count = count |> Bigint.to_int_exn in
          let size = size |> Bigint.to_int_exn in
          List.init (List.length values) (fun idx ->
              if idx + count < size then List.nth values (idx + count)
              else Value.set_invalid (List.nth values idx))
        in
        let idx =
          if Bigint.(idx >= count) then Bigint.(idx - count) else Bigint.zero
        in
        let value_base = Value.StackV (values, idx, size) in
        let sign = Sig.Ret None in
        (ctx, sign, Some value_base)
    | _ ->
        F.asprintf
          "(eval_builtin_stack_method_call) invalid method %a for header stack"
          FId.pp fid
        |> error_no_info

  and eval_builtin_header_method_call (_cursor : Ctx.cursor) (ctx : Ctx.t)
      (fid : FId.t) (id : id') (valid : bool)
      (fields : (member' * Value.t) list) : Ctx.t * Sig.t * Value.t option =
    match fid with
    | "isValid", [] ->
        let value_valid = Value.BoolV valid in
        let sign = Sig.Ret (Some value_valid) in
        (ctx, sign, None)
    | "setValid", [] ->
        let value_base = Value.HeaderV (id, true, fields) in
        let sign = Sig.Ret None in
        (ctx, sign, Some value_base)
    | "setInvalid", [] ->
        let value_base = Value.HeaderV (id, false, fields) in
        let sign = Sig.Ret None in
        (ctx, sign, Some value_base)
    | _ ->
        F.asprintf
          "(eval_builtin_header_method_call) invalid method %a for header"
          FId.pp fid
        |> error_no_info

  and eval_builtin_union_method_call (_cursor : Ctx.cursor) (ctx : Ctx.t)
      (fid : FId.t) (_id : id') (fields : (member' * Value.t) list) :
      Ctx.t * Sig.t * Value.t option =
    match fid with
    | "isValid", [] ->
        let valid = List.map snd fields |> List.exists Value.get_header_valid in
        let value_valid = Value.BoolV valid in
        let sign = Sig.Ret (Some value_valid) in
        (ctx, sign, None)
    | _ ->
        F.asprintf
          "(eval_builtin_union_method_call) invalid method %a for union" FId.pp
          fid
        |> error_no_info

  and eval_action_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (params : param list) (targs : targ list)
      (args : arg list) (args_default : id' list) (block : block) :
      Ctx.t * Sig.t =
    let ctx_callee = pre ctx_caller in
    check (targs = []) "(eval_action_call) action cannot have type arguments";
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign =
      let stmt_block = BlockS { block } $ no_info in
      eval_stmt Ctx.Local ctx_callee Sig.Cont stmt_block
    in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
    in
    (ctx_caller, sign)

  and eval_function_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (tparams : tparam list) (params : param list)
      (targs : typ list) (args : arg list) (args_default : id' list)
      (block : block) : Ctx.t * Sig.t =
    let ctx_callee = pre ctx_caller in
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign =
      let stmt_block = BlockS { block } $ no_info in
      eval_stmt Ctx.Local ctx_callee Sig.Cont stmt_block
    in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
    in
    (ctx_caller, sign)

  and eval_extern_function_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (fid : FId.t) (tparams : tparam list)
      (params : param list) (targs : typ list) (args : arg list)
      (args_default : id' list) : Ctx.t * Sig.t =
    let ctx_callee = pre ctx_caller in
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign = Arch.eval_extern_func_call ctx_callee fid in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
    in
    (ctx_caller, sign)

  and eval_extern_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (oid : OId.t) (fid : FId.t) (tparams : tparam list)
      (params : param list) (targs : typ list) (args : arg list)
      (args_default : id' list) : Ctx.t * Sig.t =
    let ctx_callee = pre ctx_caller in
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign = Arch.eval_extern_method_call ctx_callee oid fid in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
    in
    (ctx_caller, sign)

  and eval_parser_apply_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (params : param list) (args : arg list)
      (args_default : id' list) (decls : decl list) (senv : SEnv.t) :
      Ctx.t * Sig.t =
    let ctx_callee = pre ctx_caller in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Block ctx_callee params args
    in
    let ctx_callee = eval_decls Ctx.Block ctx_callee decls in
    let ctx_callee =
      { ctx_callee with block = { ctx_callee.block with senv } }
    in
    let ctx_callee =
      ctx_callee.block.senv |> SEnv.bindings |> List.map fst
      |> List.fold_left
           (fun ctx_callee id ->
             Ctx.add_value Ctx.Block id (Value.StateV id) ctx_callee)
           ctx_callee
    in
    let ctx_callee, sign =
      eval_parser_state_machine Ctx.Block ctx_callee
        (Sig.Trans (`State "start"))
    in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Block ctx_callee params lvalues
    in
    (ctx_caller, sign)

  and eval_control_apply_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (params : param list) (args : arg list)
      (args_default : id' list) (decls : decl list) (fenv : FEnv.t)
      (block : block) : Ctx.t * Sig.t =
    let ctx_callee = pre ctx_caller in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee, lvalues =
      copyin cursor_caller ctx_caller Ctx.Block ctx_callee params args
    in
    let ctx_callee = eval_decls Ctx.Block ctx_callee decls in
    let ctx_callee =
      { ctx_callee with block = { ctx_callee.block with fenv } }
    in
    let ctx_callee, sign =
      let stmt_block = BlockS { block } $ no_info in
      eval_stmt Ctx.Local ctx_callee Sig.Cont stmt_block
    in
    let ctx_caller = post ctx_caller ctx_callee in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Block ctx_callee params lvalues
    in
    (ctx_caller, sign)

  and eval_table_apply_method_call ~pre ~post (_cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (oid : OId.t) (table : Table.t) : Ctx.t * Sig.t =
    let id = oid |> List.rev |> List.hd in
    let ctx_callee = pre ctx_caller in
    let ctx_callee, sign = eval_table Ctx.Block ctx_callee id table in
    let ctx_caller = post ctx_caller ctx_callee in
    (ctx_caller, sign)

  (* Entry point: function call *)

  and eval_func (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : targ list) (args : arg list) : callkind =
    let (fid, func, args_default), cursor_func =
      let args = FId.to_names args in
      Ctx.find_f_at Ctx.find_func_at_opt cursor var_func args ctx
    in
    match cursor_func with
    | Ctx.Global -> InterGlobal { fid; func; targs; args; args_default }
    | Ctx.Block -> IntraBlock { oid = []; fid; func; targs; args; args_default }
    | Ctx.Local -> assert false

  and eval_func_call (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Sig.t =
    let callkind = eval_func cursor ctx var_func targs args in
    eval_call cursor ctx callkind

  (* Entry point: method call *)

  and eval_method (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (member : member) (targs : targ list) (args : arg list) : Ctx.t * callkind
      =
    let ctx, lvalue_base = eval_lvalue_of_expr cursor ctx expr_base in
    let value_base = eval_lvalue cursor ctx lvalue_base in
    let callkind =
      match (value_base, member.it) with
      | StackV _, "push_front" | StackV _, "pop_front" ->
          let fid = (member.it, [ ("count", false) ]) in
          let params =
            [
              ("count" $ no_info, L.In $ no_info, Types.IntT $ no_info, None, [])
              $ no_info;
            ]
          in
          let func = Func.BuiltinMethodF (params, lvalue_base) in
          InterGlobal { fid; func; targs; args; args_default = [] }
      | HeaderV _, "isValid"
      | HeaderV _, "setValid"
      | HeaderV _, "setInvalid"
      | UnionV _, "isValid" ->
          let fid = (member.it, []) in
          let func = Func.BuiltinMethodF ([], lvalue_base) in
          InterGlobal { fid; func; targs = []; args; args_default = [] }
      | RefV oid, member -> (
          let obj = Sto.find oid !sto in
          match obj with
          | ExternO (_, tenv_block, venv_block, fenv_block) ->
              let fid, func, args_default =
                let args = FId.to_names args in
                FEnv.find_func (member, args) fenv_block
              in
              InterBlock
                {
                  oid;
                  fid;
                  tenv_block;
                  venv_block;
                  func;
                  targs;
                  args;
                  args_default;
                }
          | ParserO (venv_block, params, decls, senv) ->
              let fid, func, args_default =
                let fid = FId.to_fid ("apply" $ no_info) params in
                let func = Func.ParserApplyMethodF (params, decls, senv) in
                let fenv = FEnv.add fid func FEnv.empty in
                let args = FId.to_names args in
                FEnv.find_func (member, args) fenv
              in
              InterBlock
                {
                  oid;
                  fid;
                  tenv_block = TEnv.empty;
                  venv_block;
                  func;
                  targs;
                  args;
                  args_default;
                }
          | ControlO (venv_block, params, decls, fenv, block) ->
              let fid, func, args_default =
                let fid = FId.to_fid ("apply" $ no_info) params in
                let func =
                  Func.ControlApplyMethodF (params, decls, fenv, block)
                in
                let fenv = FEnv.add fid func FEnv.empty in
                let args = FId.to_names args in
                FEnv.find_func (member, args) fenv
              in
              InterBlock
                {
                  oid;
                  fid;
                  tenv_block = TEnv.empty;
                  venv_block;
                  func;
                  targs;
                  args;
                  args_default;
                }
          | TableO (_, table) ->
              let fid, func, args_default =
                let fid = FId.to_fid ("apply" $ no_info) [] in
                let func = Func.TableApplyMethodF table in
                let fenv = FEnv.add fid func FEnv.empty in
                let args = FId.to_names args in
                FEnv.find_func (member, args) fenv
              in
              IntraBlock { oid; fid; func; targs; args; args_default }
          | _ ->
              F.asprintf "(eval_method) method %a not found for object %a"
                Il.Pp.pp_member' member (Obj.pp ~level:0) obj
              |> error_no_info)
      | _ ->
          F.asprintf "(eval_method) method %a not found for value %a"
            Il.Pp.pp_member member (Value.pp ~level:0) value_base
          |> error_no_info
    in
    (ctx, callkind)

  and eval_method_call (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (member : member) (targs : typ list) (args : arg list) : Ctx.t * Sig.t =
    let ctx, callkind = eval_method cursor ctx expr_base member targs args in
    eval_call cursor ctx callkind
end
