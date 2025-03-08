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
module SEnv = Envs.SEnv
module Theta = Envs.Theta
module FEnv = Envs.FEnv
module Sto = Envs.Sto
open Sigs
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
                       the block context of the caller object,
                       and the local context of the callee object *)

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
        theta_block : Theta.t;
        venv_block : VEnv.t;
        func : Func.t;
        targs : targ list;
        args : arg list;
        args_default : id' list;
      }
    | IntraBlock of {
        oid : OId.t;
        fid : FId.t;
        venv_local : VEnv.t;
        func : Func.t;
        targs : targ list;
        args : arg list;
        args_default : id' list;
      }

  (* Continuations *)

  let cont_value (esign : Value.t Sig.t) : Value.t =
    match esign with
    | Cont value -> value
    | _ ->
        F.asprintf
          "(cont_value) expected a value, but got a different continuation"
        |> error_no_info

  let cont_sig (ctx : Ctx.t) (sign : 'a Sig.t)
      (continue : 'a -> Ctx.t * 'b Sig.t) : Ctx.t * 'b Sig.t =
    match sign with
    | Cont cont -> continue cont
    | Reject value -> (ctx, Reject value)
    | Exit -> (ctx, Exit)

  let cont_ssig (ctx : Ctx.t) (ssig : SSig.t)
      (continue : unit -> Ctx.t * SSig.t) : Ctx.t * SSig.t =
    match ssig with Cont -> continue () | _ -> (ctx, ssig)

  let cont_sig_to_ssig (ctx : Ctx.t) (sign : 'a Sig.t)
      (continue : 'a -> Ctx.t * SSig.t) : Ctx.t * SSig.t =
    match sign with
    | Cont cont -> continue cont
    | Reject value -> (ctx, Trans (`Reject value))
    | Exit -> (ctx, Exit)

  (* L-value evaluation *)

  let rec eval_lvalue_of_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg) :
      Ctx.t * LValue.t option Sig.t =
    try eval_lvalue_of_arg' cursor ctx arg.it
    with InterpErr _ as err -> error_pass_info arg.at err

  and eval_lvalue_of_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg') :
      Ctx.t * LValue.t option Sig.t =
    match arg with
    | L.ExprA expr | L.NameA (_, Some expr) ->
        let ctx, lsign = eval_lvalue_of_expr cursor ctx expr in
        cont_sig ctx lsign (fun lvalue -> (ctx, Cont (Some lvalue)))
    | L.NameA (_, None) | L.AnyA -> (ctx, Cont None)

  and eval_lvalue_of_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr) :
      Ctx.t * LValue.t Sig.t =
    try eval_lvalue_of_expr' cursor ctx expr.it
    with InterpErr _ as err -> error_pass_info expr.at err

  and eval_lvalue_of_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr') :
      Ctx.t * LValue.t Sig.t =
    match expr with
    | VarE { var } ->
        let lvalue = LValue.VarLV var.it in
        (ctx, Cont lvalue)
    | BitAccE { expr_base; value_lo; value_hi } ->
        let ctx, lsign = eval_lvalue_of_expr cursor ctx expr_base in
        cont_sig ctx lsign (fun lvalue_base ->
            let lvalue =
              LValue.BitAccLV (lvalue_base, value_lo.it, value_hi.it)
            in
            (ctx, Cont lvalue))
    | ArrAccE { expr_base; expr_idx } ->
        let ctx, lsign = eval_lvalue_of_expr cursor ctx expr_base in
        cont_sig ctx lsign (fun lvalue_base ->
            let ctx, esign = eval_expr cursor ctx expr_idx in
            cont_sig ctx esign (fun value_idx ->
                let lvalue = LValue.ArrAccLV (lvalue_base, value_idx) in
                (ctx, Cont lvalue)))
    | ExprAccE { expr_base; member } ->
        let ctx, lsign = eval_lvalue_of_expr cursor ctx expr_base in
        cont_sig ctx lsign (fun lvalue_base ->
            let lvalue = LValue.ExprAccLV (lvalue_base, member.it) in
            (ctx, Cont lvalue))
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

  and eval_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg) :
      Ctx.t * Value.t Sig.t =
    try eval_arg' cursor ctx arg.it
    with InterpErr _ as err -> error_pass_info arg.at err

  and eval_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg') :
      Ctx.t * Value.t Sig.t =
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
      Ctx.t * Value.t Sig.t =
    try eval_expr' cursor ctx expr.it
    with InterpErr _ as err -> error_pass_info expr.at err

  and eval_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr') :
      Ctx.t * Value.t Sig.t =
    let wrap_value value = (ctx, Sig.Cont value) in
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
      Ctx.t * Value.t list Sig.t =
    List.fold_left
      (fun ((ctx, esign) : Ctx.t * Value.t list Sig.t) (expr : expr) ->
        cont_sig ctx esign (fun values ->
            let ctx, esign = eval_expr cursor ctx expr in
            cont_sig ctx esign (fun value -> (ctx, Cont (values @ [ value ])))))
      (ctx, Cont []) exprs

  and eval_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : var) : Value.t =
    let value = Ctx.find Ctx.find_value_opt cursor var ctx in
    value

  and eval_seq_expr ~(default : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
      (exprs : expr list) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_exprs cursor ctx exprs in
    cont_sig ctx esign (fun values ->
        let value =
          if default then Value.SeqDefaultV values else Value.SeqV values
        in
        (ctx, Cont value))

  and eval_record_expr ~(default : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
      (fields : (member * expr) list) : Ctx.t * Value.t Sig.t =
    let members, exprs = List.split fields in
    let members = List.map it members in
    let ctx, esign = eval_exprs cursor ctx exprs in
    cont_sig ctx esign (fun values ->
        let fields = List.combine members values in
        let value =
          if default then Value.RecordDefaultV fields else Value.RecordV fields
        in
        (ctx, Cont value))

  and eval_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (unop : unop)
      (expr : expr) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr in
    cont_sig ctx esign (fun value ->
        let value = Numerics.eval_unop unop value in
        (ctx, Cont value))

  and eval_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : binop)
      (expr_l : expr) (expr_r : expr) : Ctx.t * Value.t Sig.t =
    match binop.it with
    (* short-circuiting *)
    | L.LAndOp ->
        let ctx, esign = eval_expr cursor ctx expr_l in
        cont_sig ctx esign (fun value_l ->
            let cond = Value.get_bool value_l in
            if cond then eval_expr cursor ctx expr_r else (ctx, esign))
    | L.LOrOp ->
        let ctx, esign = eval_expr cursor ctx expr_l in
        cont_sig ctx esign (fun value_l ->
            let cond = Value.get_bool value_l in
            if cond then (ctx, esign) else eval_expr cursor ctx expr_r)
    (* normal *)
    | _ ->
        let ctx, esign = eval_expr cursor ctx expr_l in
        cont_sig ctx esign (fun value_l ->
            let ctx, esign = eval_expr cursor ctx expr_r in
            cont_sig ctx esign (fun value_r ->
                let value = Numerics.eval_binop binop value_l value_r in
                (ctx, Cont value)))

  and eval_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
      (expr_then : expr) (expr_else : expr) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr_cond in
    cont_sig ctx esign (fun value_cond ->
        let cond = Value.get_bool value_cond in
        let expr = if cond then expr_then else expr_else in
        eval_expr cursor ctx expr)

  and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : typ)
      (expr : expr) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr in
    cont_sig ctx esign (fun value ->
        let value = Numerics.eval_cast typ.it value in
        (ctx, Cont value))

  and eval_mask_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (expr_mask : expr) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr_base in
    cont_sig ctx esign (fun value_base ->
        let ctx, esign = eval_expr cursor ctx expr_mask in
        cont_sig ctx esign (fun value_mask ->
            let value = Value.SetV (`Mask (value_base, value_mask)) in
            (ctx, Cont value)))

  and eval_range_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_lb : expr)
      (expr_ub : expr) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr_lb in
    cont_sig ctx esign (fun value_lb ->
        let ctx, esign = eval_expr cursor ctx expr_ub in
        cont_sig ctx esign (fun value_ub ->
            let value = Value.SetV (`Range (value_lb, value_ub)) in
            (ctx, Cont value)))

  and eval_select_match_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
      (value_key : Value.t) (keyset : keyset) : Ctx.t * bool Sig.t =
    match keyset.it with
    | ExprK expr ->
        let ctx, esign = eval_expr cursor ctx expr in
        cont_sig ctx esign (fun value ->
            let matched = eval_keyset_match value_key value in
            (ctx, Cont matched))
    | DefaultK | AnyK -> (ctx, Cont true)

  and eval_select_match (cursor : Ctx.cursor) (ctx : Ctx.t)
      (values_key : Value.t list) (case : select_case) :
      Ctx.t * state_label option Sig.t =
    let keysets, label = case.it in
    let ctx, msign =
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
            (fun (ctx, msign) value_key keyset ->
              cont_sig ctx msign (fun matched ->
                  if not matched then (ctx, Cont matched)
                  else eval_select_match_keyset cursor ctx value_key keyset))
            (ctx, Cont true) values_key keysets
    in
    cont_sig ctx msign (fun matched ->
        if matched then (ctx, Cont (Some label)) else (ctx, Cont None))

  and eval_select_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (exprs_select : expr list) (cases : select_case list) :
      Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_exprs cursor ctx exprs_select in
    cont_sig ctx esign (fun values_key ->
        let ctx, lsign =
          List.fold_left
            (fun (ctx, lsign) case ->
              cont_sig ctx lsign (fun label ->
                  if Option.is_some label then (ctx, Cont label)
                  else eval_select_match cursor ctx values_key case))
            (ctx, Cont None) cases
        in
        cont_sig ctx lsign (fun label ->
            check (Option.is_some label)
              "(eval_select_expr) no matching case found";
            let label = Option.get label in
            let value = Value.StateV label.it in
            (ctx, Cont value)))

  and eval_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (expr_idx : expr) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr_base in
    cont_sig ctx esign (fun value_base ->
        let ctx, esign = eval_expr cursor ctx expr_idx in
        cont_sig ctx esign (fun value_idx ->
            let idx = value_idx |> Value.get_num in
            match value_base with
            | TupleV values ->
                let value = idx |> Bigint.to_int_exn |> List.nth values in
                (ctx, Cont value)
            | StackV (values, _, size) when Bigint.(idx >= size) ->
                (* (TODO) What if the stack is of size zero? *)
                let value = List.hd values in
                let value = Value.set_invalid value in
                (ctx, Cont value)
            | StackV (values, _, _) ->
                let value = idx |> Bigint.to_int_exn |> List.nth values in
                (ctx, Cont value)
            | _ ->
                F.asprintf "(eval_array_acc_expr) %a cannot be indexed"
                  (Value.pp ~level:0) value_base
                |> error_no_info))

  and eval_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (value_lo : value) (value_hi : value) :
      Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr_base in
    cont_sig ctx esign (fun value_base ->
        let value =
          Numerics.eval_bitstring_access value_base value_hi.it value_lo.it
        in
        (ctx, Cont value))

  and eval_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (member : member) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_expr cursor ctx expr_base in
    cont_sig ctx esign (fun value_base ->
        let value =
          match value_base with
          | StackV (values, idx, size) -> (
              match member.it with
              | "size" -> Value.FBitV (Bigint.of_int 32, size)
              | "next" -> idx |> Bigint.to_int_exn |> List.nth values
              | "last" ->
                  Bigint.(idx - one) |> Bigint.to_int_exn |> List.nth values
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
        (ctx, Cont value))

  and eval_call_func_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_func_call cursor ctx var_func targs args in
    match esign with
    | Ret (Some value) -> (ctx, Cont value)
    | Trans (`Reject value) -> (ctx, Reject value)
    | Exit -> (ctx, Exit)
    | _ ->
        "(eval_call_func_expr) function call as an expression must return a \
         value" |> error_no_info

  and eval_call_method_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (member : member) (targs : typ list) (args : arg list)
      : Ctx.t * Value.t Sig.t =
    let ctx, esign = eval_method_call cursor ctx expr_base member targs args in
    match esign with
    | Ret (Some value) -> (ctx, Cont value)
    | Trans (`Reject value) -> (ctx, Reject value)
    | Exit -> (ctx, Exit)
    | _ ->
        "(eval_call_method_expr) method call as an expression must return a \
         value" |> error_no_info

  (* Statement evaluation *)

  and eval_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (stmt : stmt) :
      Ctx.t * SSig.t =
    try eval_stmt' cursor ctx stmt.it
    with InterpErr _ as err -> error_pass_info stmt.at err

  and eval_stmt' (cursor : Ctx.cursor) (ctx : Ctx.t) (stmt : stmt') :
      Ctx.t * SSig.t =
    match stmt with
    | EmptyS -> (ctx, SSig.Cont)
    | AssignS { expr_l; expr_r } -> eval_assign_stmt cursor ctx expr_l expr_r
    | SwitchS { expr_switch; cases } ->
        eval_switch_stmt cursor ctx expr_switch cases
    | IfS { expr_cond; stmt_then; stmt_else } ->
        eval_if_stmt cursor ctx expr_cond stmt_then stmt_else
    | BlockS { block } -> eval_block_stmt cursor ctx block
    | ExitS -> (ctx, SSig.Exit)
    | RetS { expr_ret } -> eval_return_stmt cursor ctx expr_ret
    | CallFuncS { var_func; targs; args } ->
        eval_call_func_stmt cursor ctx var_func targs args
    | CallMethodS { expr_base; member; targs; args } ->
        eval_call_method_stmt cursor ctx expr_base member targs args
    | CallInstS _ ->
        F.asprintf
          "(eval_stmt) instantiation should have been handled by the \
           instantiation phase"
        |> error_no_info
    | TransS { expr_label } -> eval_trans_stmt cursor ctx expr_label
    | DeclS { decl } -> eval_decl_stmt cursor ctx decl

  and eval_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (stmts : stmt list) :
      Ctx.t * SSig.t =
    List.fold_left
      (fun ((ctx, ssig) : Ctx.t * SSig.t) (stmt : stmt) ->
        cont_ssig ctx ssig (fun () -> eval_stmt cursor ctx stmt))
      (ctx, Cont) stmts

  and eval_assign_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_l : expr)
      (expr_r : expr) : Ctx.t * SSig.t =
    let ctx, lsign = eval_lvalue_of_expr cursor ctx expr_l in
    cont_sig_to_ssig ctx lsign (fun lvalue ->
        let ctx, esign = eval_expr cursor ctx expr_r in
        cont_sig_to_ssig ctx esign (fun value_r ->
            let ctx = eval_lvalue_write cursor ctx lvalue value_r in
            (ctx, Cont)))

  and eval_switch_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_switch : expr)
      (cases : switch_case list) : Ctx.t * SSig.t =
    let typ_switch = expr_switch.note.typ in
    let ctx, esign = eval_expr cursor ctx expr_switch in
    cont_sig_to_ssig ctx esign (fun value_switch ->
        match typ_switch with
        | TableEnumT (id_table, _) ->
            let id_table =
              String.sub id_table 12 (String.length id_table - 12)
            in
            let id_table = String.sub id_table 0 (String.length id_table - 1) in
            eval_switch_table_stmt cursor ctx id_table value_switch cases
        | _ -> eval_switch_general_stmt cursor ctx value_switch cases)

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
      Ctx.t * SSig.t =
    let _, block =
      List.fold_left
        (fun (fallthrough, block) case ->
          if Option.is_some block then (false, block)
          else eval_switch_table_match id_table fallthrough value_switch case)
        (false, None) cases
    in
    match block with
    | Some block -> eval_block ~start:false cursor ctx block
    | None -> (ctx, Cont)

  and eval_switch_general_match_label (cursor : Ctx.cursor) (ctx : Ctx.t)
      (value_switch : Value.t) (label : switch_label) : Ctx.t * bool Sig.t =
    match label.it with
    | ExprL expr ->
        let ctx, esign = eval_expr cursor ctx expr in
        cont_sig ctx esign (fun value ->
            let matched = Numerics.eval_binop_eq value_switch value in
            (ctx, Cont matched))
    | DefaultL -> (ctx, Cont true)

  and eval_switch_general_match (cursor : Ctx.cursor) (ctx : Ctx.t)
      (fallthrough : bool) (value_switch : Value.t) (case : switch_case) :
      Ctx.t * (bool * block option) Sig.t =
    match case.it with
    | MatchC (_, block) when fallthrough -> (ctx, Cont (false, Some block))
    | MatchC (label, block) ->
        let ctx, msign =
          eval_switch_general_match_label cursor ctx value_switch label
        in
        cont_sig ctx msign (fun matched ->
            if matched then (ctx, Cont (false, Some block))
            else (ctx, Cont (false, None)))
    | FallC label ->
        let ctx, msign =
          eval_switch_general_match_label cursor ctx value_switch label
        in
        cont_sig ctx msign (fun matched ->
            if matched then (ctx, Cont (true, None))
            else (ctx, Cont (false, None)))

  and eval_switch_general_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (value_switch : Value.t) (cases : switch_case list) : Ctx.t * SSig.t =
    let ctx, msign =
      List.fold_left
        (fun (ctx, msign) case ->
          cont_sig ctx msign (fun (fallthrough, block) ->
              if Option.is_some block then (ctx, Cont (false, block))
              else
                eval_switch_general_match cursor ctx fallthrough value_switch
                  case))
        (ctx, Cont (false, None))
        cases
    in
    cont_sig_to_ssig ctx msign (fun (_, block) ->
        match block with
        | Some block -> eval_block ~start:false cursor ctx block
        | None -> (ctx, Cont))

  and eval_if_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
      (stmt_then : stmt) (stmt_else : stmt) : Ctx.t * SSig.t =
    let ctx, esign = eval_expr cursor ctx expr_cond in
    cont_sig_to_ssig ctx esign (fun value_cond ->
        let cond = Value.get_bool value_cond in
        let stmt = if cond then stmt_then else stmt_else in
        eval_stmt cursor ctx stmt)

  and eval_block ~(start : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
      (block : block) : Ctx.t * SSig.t =
    try eval_block' ~start cursor ctx block.it
    with InterpErr _ as err -> error_pass_info block.at err

  and eval_block' ~(start : bool) (cursor : Ctx.cursor) (ctx : Ctx.t)
      (block : block') : Ctx.t * SSig.t =
    let stmts, _annos = block in
    let ctx = if start then ctx else Ctx.enter_frame ctx in
    let ctx, ssign = eval_stmts cursor ctx stmts in
    let ctx = if start then ctx else Ctx.exit_frame ctx in
    (ctx, ssign)

  and eval_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (block : block) :
      Ctx.t * SSig.t =
    eval_block ~start:false cursor ctx block

  and eval_return_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_ret : expr option) : Ctx.t * SSig.t =
    match expr_ret with
    | Some expr_ret ->
        let ctx, esign = eval_expr cursor ctx expr_ret in
        cont_sig_to_ssig ctx esign (fun value_ret ->
            (ctx, SSig.Ret (Some value_ret)))
    | None -> (ctx, SSig.Ret None)

  and eval_call_func_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * SSig.t =
    let ctx, ssign = eval_func_call cursor ctx var_func targs args in
    match ssign with
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
      : Ctx.t * SSig.t =
    let ctx, ssign = eval_method_call cursor ctx expr_base member targs args in
    match ssign with
    | Cont | Ret _ | Trans `Accept -> (ctx, Cont)
    | Trans (`Reject value) -> (ctx, Trans (`Reject value))
    | Trans (`State _) ->
        F.asprintf
          "(eval_call_method_stmt) method call should not result in a state \
           transition"
        |> error_no_info
    | Exit -> (ctx, Exit)

  and eval_trans_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_label : expr) :
      Ctx.t * SSig.t =
    let ctx, esign = eval_expr cursor ctx expr_label in
    cont_sig_to_ssig ctx esign (fun value_label ->
        let label = Value.get_state value_label in
        let ssign = SSig.Trans (`State label) in
        (ctx, ssign))

  and eval_decl_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl) :
      Ctx.t * SSig.t =
    let ctx, dsign = eval_decl cursor ctx decl in
    cont_sig_to_ssig ctx dsign (fun _ -> (ctx, Cont))

  (* Declaration evaluation *)

  and eval_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl) :
      Ctx.t * unit Sig.t =
    try eval_decl' cursor ctx decl.it
    with InterpErr _ as err -> error_pass_info decl.at err

  and eval_decl' (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl') :
      Ctx.t * unit Sig.t =
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

  and eval_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : decl list) :
      Ctx.t * unit Sig.t =
    List.fold_left
      (fun ((ctx, dsign) : Ctx.t * unit Sig.t) (decl : decl) ->
        cont_sig ctx dsign (fun _ -> eval_decl cursor ctx decl))
      (ctx, Cont ()) decls

  and eval_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (_typ : typ)
      (value : value) (_annos : anno list) : Ctx.t * unit Sig.t =
    let ctx = Ctx.add_value cursor id.it value.it ctx in
    (ctx, Cont ())

  and eval_var_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (typ : typ)
      (init : expr option) (_annos : anno list) : Ctx.t * unit Sig.t =
    match init with
    | Some expr ->
        let ctx, esign = eval_expr cursor ctx expr in
        cont_sig ctx esign (fun value ->
            let ctx = Ctx.add_value cursor id.it value ctx in
            (ctx, Cont ()))
    | None ->
        let value =
          Ctx.resolve_typ cursor typ.it ctx |> Numerics.eval_default
        in
        let ctx = Ctx.add_value cursor id.it value ctx in
        (ctx, Cont ())

  (* Parser state machine evaluation *)

  and eval_parser_state_machine (cursor : Ctx.cursor) (ctx : Ctx.t)
      (ssign : SSig.t) : Ctx.t * SSig.t =
    assert (cursor = Ctx.Block);
    match ssign with
    | Trans (`State "accept") -> (ctx, Trans `Accept)
    | Trans (`State "reject") -> (ctx, Trans (`Reject (Value.ErrV "NoError")))
    | Trans (`State id) ->
        let state = Ctx.find_state cursor id ctx in
        let ctx, ssign = eval_parser_state cursor ctx state in
        eval_parser_state_machine cursor ctx ssign
    | _ -> (ctx, ssign)

  and eval_parser_state (cursor : Ctx.cursor) (ctx : Ctx.t) (state : State.t) :
      Ctx.t * SSig.t =
    assert (cursor = Ctx.Block);
    let ctx, ssign = eval_block ~start:true Ctx.Local ctx state in
    match ssign with
    | Cont -> (ctx, SSig.Trans (`State "reject"))
    | Trans _ -> (ctx, ssign)
    | _ ->
        F.asprintf
          "(eval_parser_state) parser state should result in a state transition"
        |> error_no_info

  (* Table evaluation *)

  and eval_table_match_keyset (cursor : Ctx.cursor) (ctx : Ctx.t)
      (table_key : Value.t * match_kind) (keyset : keyset) : Ctx.t * bool Sig.t
      =
    let value_key, _match_kind = table_key in
    match keyset.it with
    | ExprK expr ->
        let ctx, esign = eval_expr cursor ctx expr in
        cont_sig ctx esign (fun value ->
            let matched = eval_keyset_match value_key value in
            (ctx, Cont matched))
    | DefaultK | AnyK -> (ctx, Cont true)

  and eval_table_match_keysets (cursor : Ctx.cursor) (ctx : Ctx.t)
      (table_keys : (Value.t * match_kind) list) (table_entry : table_entry) :
      Ctx.t * (var * arg list * int option) option Sig.t =
    let _, keysets, table_action, priority, _ = table_entry.it in
    let priority =
      Option.map
        (fun priority -> priority.it |> Value.get_num |> Bigint.to_int_exn)
        priority
    in
    match (table_keys, keysets) with
    | _, [ { it = L.DefaultK; _ } ] | _, [ { it = L.AnyK; _ } ] ->
        let action =
          let var_action, args_action, _, _, _ = table_action.it in
          Some (var_action, args_action, priority)
        in
        (ctx, Cont action)
    | table_keys, keysets ->
        check
          (List.length table_keys = List.length keysets)
          "(eval_table_match_keysets) number of table keys must match the \
           number of keysets";
        let ctx, msign =
          List.fold_left2
            (fun ((ctx, msign) : Ctx.t * bool Sig.t)
                 (table_key : Value.t * match_kind) (keyst : keyset) ->
              cont_sig ctx msign (fun matched ->
                  if not matched then (ctx, Cont matched)
                  else eval_table_match_keyset cursor ctx table_key keyst))
            (ctx, Cont true) table_keys keysets
        in
        cont_sig ctx msign (fun matched ->
            let action =
              if matched then
                let var_action, args_action, _, _, _ = table_action.it in
                Some (var_action, args_action, priority)
              else None
            in
            (ctx, Cont action))

  and eval_table_match (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id')
      (largest_priority_wins : bool) (table_keys : (Value.t * match_kind) list)
      (action_default : var * arg list) (table_entries : table_entry list) :
      Ctx.t * (Value.t * (var * arg list)) Sig.t =
    let ctx, asign =
      List.fold_left
        (fun ((ctx, asign) : Ctx.t * (var * arg list * int option) list Sig.t)
             (table_entry : table_entry) ->
          cont_sig ctx asign (fun actions ->
              let ctx, asign =
                eval_table_match_keysets cursor ctx table_keys table_entry
              in
              cont_sig ctx asign (fun action ->
                  let actions = actions @ Option.to_list action in
                  (ctx, Cont actions))))
        (ctx, Cont []) table_entries
    in
    cont_sig ctx asign (fun actions ->
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
              let var_action, _ = action in
              match var_action.it with Top id -> id.it | Current id -> id.it
            in
            Value.TableEnumFieldV ("action_list(" ^ id ^ ")", member)
          in
          Value.TableStructV
            ( "apply_result(" ^ id ^ ")",
              [ ("hit", hit); ("miss", miss); ("action_run", action_run) ] )
        in
        (ctx, Cont (value, action)))

  and eval_table (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id')
      (table : Table.t) : Ctx.t * SSig.t =
    (* Evaluate table custom properties *)
    let largest_priority_wins =
      List.find_map
        (fun table_custom ->
          let _, member, expr, _ = table_custom.it in
          if member.it = "largest_priority_wins" then
            eval_expr cursor ctx expr |> snd |> cont_value |> Value.get_bool
            |> Option.some
          else None)
        table.customs
      |> Option.value ~default:true
    in
    (* Evaluate table keys *)
    let exprs_key, match_kinds_key =
      List.map
        (fun table_key ->
          let expr_key, match_kind_key, _annos_key = table_key.it in
          (expr_key, match_kind_key))
        table.keys
      |> List.split
    in
    let ctx, esign = eval_exprs cursor ctx exprs_key in
    cont_sig_to_ssig ctx esign (fun values_key ->
        let table_keys = List.combine values_key match_kinds_key in
        (* Perform match against table entries *)
        let ctx, tsign =
          let _, table_action_default = table.action_default in
          let var_action_default, args_action_default, _, _, _ =
            table_action_default.it
          in
          let action_default = (var_action_default, args_action_default) in
          let _, table_entries = table.entries in
          eval_table_match cursor ctx id largest_priority_wins table_keys
            action_default table_entries
        in
        cont_sig_to_ssig ctx tsign (fun (value, action) ->
            (* Perform the matched action *)
            let var_action, args_action = action in
            let stmt_call_action =
              CallFuncS
                { var_func = var_action; targs = []; args = args_action }
              $ no_info
            in
            let ctx, ssign = eval_stmt cursor ctx stmt_call_action in
            cont_ssig ctx ssign (fun () -> (ctx, SSig.Ret (Some value)))))

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
      (args : arg list) : Ctx.t * (Ctx.t * LValue.t option list) Sig.t =
    let copyin' (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
        (lvalues : LValue.t option list) (param : param) (arg : arg) :
        Ctx.t * (Ctx.t * LValue.t option list) Sig.t =
      let id, dir, typ, _, _ = param.it in
      match dir.it with
      | No | In ->
          let ctx_caller, esign = eval_arg cursor_caller ctx_caller arg in
          cont_sig ctx_caller esign (fun value ->
              let ctx_callee =
                Ctx.add_value cursor_callee id.it value ctx_callee
              in
              let lvalues = lvalues @ [ None ] in
              (ctx_caller, Cont (ctx_callee, lvalues)))
      | InOut ->
          let ctx_caller, asign =
            eval_lvalue_of_arg cursor_caller ctx_caller arg
          in
          cont_sig ctx_caller asign (fun lvalue ->
              check (Option.is_some lvalue)
                "(copyin) inout argument cannot be nameless";
              let lvalue = Option.get lvalue in
              let value = eval_lvalue cursor_caller ctx_caller lvalue in
              let ctx_callee =
                Ctx.add_value cursor_callee id.it value ctx_callee
              in
              let lvalues = lvalues @ [ Some lvalue ] in
              (ctx_caller, Cont (ctx_callee, lvalues)))
      | Out ->
          let ctx_caller, asign =
            eval_lvalue_of_arg cursor_caller ctx_caller arg
          in
          cont_sig ctx_caller asign (fun lvalue ->
              let value =
                Ctx.resolve_typ cursor_callee typ.it ctx_callee
                |> Numerics.eval_default
              in
              let ctx_callee =
                Ctx.add_value cursor_callee id.it value ctx_callee
              in
              let lvalues = lvalues @ [ lvalue ] in
              (ctx_caller, Cont (ctx_callee, lvalues)))
    in
    List.fold_left2
      (fun ((ctx_caller, csign) : Ctx.t * (Ctx.t * LValue.t option list) Sig.t)
           param arg ->
        match csign with
        | Cont (ctx_callee, lvalues) ->
            copyin' ctx_caller ctx_callee lvalues param arg
        | _ -> (ctx_caller, csign))
      (ctx_caller, Cont (ctx_callee, []))
      params args

  and copyin_default (cursor_callee : Ctx.cursor) (ctx_callee : Ctx.t)
      (params_default : param list) (args_default : value list) : Ctx.t =
    List.fold_left2
      (fun ctx_callee param_default value_default ->
        let id, _, _, _, _ = param_default.it in
        Ctx.add_value cursor_callee id.it value_default.it ctx_callee)
      ctx_callee params_default args_default

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
      (callkind : callkind) : Ctx.t * SSig.t =
    match callkind with
    | InterGlobal { fid; func; targs; args; args_default } ->
        eval_call'
          ~pre:(fun (ctx_caller : Ctx.t) -> Ctx.copy Ctx.Global ctx_caller)
          ~post:(fun (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ->
            { ctx_caller with global = ctx_callee.global })
          cursor_caller ctx_caller [] fid func targs args args_default
    | InterBlock
        { oid; fid; theta_block; venv_block; func; targs; args; args_default }
      ->
        eval_call'
          ~pre:(fun (ctx_caller : Ctx.t) ->
            let ctx_callee = Ctx.copy Global ctx_caller in
            {
              ctx_callee with
              block =
                { ctx_caller.block with theta = theta_block; venv = venv_block };
            })
          ~post:(fun (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ->
            { ctx_caller with global = ctx_callee.global })
          cursor_caller ctx_caller oid fid func targs args args_default
    | IntraBlock { oid; fid; venv_local; func; targs; args; args_default } ->
        eval_call'
          ~pre:(fun (ctx_caller : Ctx.t) ->
            let ctx_callee = Ctx.copy Ctx.Block ctx_caller in
            {
              ctx_callee with
              local = { ctx_callee.local with venvs = [ venv_local ] };
            })
          ~post:(fun (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ->
            { ctx_caller with block = ctx_callee.block })
          cursor_caller ctx_caller oid fid func targs args args_default

  and eval_call' ~pre ~post (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (oid : OId.t) (fid : FId.t) (func : Func.t) (targs : targ list)
      (args : arg list) (args_default : id' list) : Ctx.t * SSig.t =
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
      Ctx.t * SSig.t =
    let value_base = eval_lvalue cursor_caller ctx_caller lvalue_base in
    let ctx_callee = pre ctx_caller in
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, ssig, value_base =
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
        (ctx_caller, ssig))

  and eval_builtin_stack_method_call (cursor : Ctx.cursor) (ctx : Ctx.t)
      (fid : FId.t) (values : Value.t list) (idx : Bigint.t) (size : Bigint.t) :
      Ctx.t * SSig.t * Value.t option =
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
        let ssign = SSig.Ret None in
        (ctx, ssign, Some value_base)
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
        let ssign = SSig.Ret None in
        (ctx, ssign, Some value_base)
    | _ ->
        F.asprintf
          "(eval_builtin_stack_method_call) invalid method %a for header stack"
          FId.pp fid
        |> error_no_info

  and eval_builtin_header_method_call (_cursor : Ctx.cursor) (ctx : Ctx.t)
      (fid : FId.t) (id : id') (valid : bool)
      (fields : (member' * Value.t) list) : Ctx.t * SSig.t * Value.t option =
    match fid with
    | "isValid", [] ->
        let value_valid = Value.BoolV valid in
        let ssign = SSig.Ret (Some value_valid) in
        (ctx, ssign, None)
    | "setValid", [] ->
        let value_base = Value.HeaderV (id, true, fields) in
        let ssign = SSig.Ret None in
        (ctx, ssign, Some value_base)
    | "setInvalid", [] ->
        let value_base = Value.HeaderV (id, false, fields) in
        let ssign = SSig.Ret None in
        (ctx, ssign, Some value_base)
    | _ ->
        F.asprintf
          "(eval_builtin_header_method_call) invalid method %a for header"
          FId.pp fid
        |> error_no_info

  and eval_builtin_union_method_call (_cursor : Ctx.cursor) (ctx : Ctx.t)
      (fid : FId.t) (_id : id') (fields : (member' * Value.t) list) :
      Ctx.t * SSig.t * Value.t option =
    match fid with
    | "isValid", [] ->
        let valid = List.map snd fields |> List.exists Value.get_header_valid in
        let value_valid = Value.BoolV valid in
        let ssign = SSig.Ret (Some value_valid) in
        (ctx, ssign, None)
    | _ ->
        F.asprintf
          "(eval_builtin_union_method_call) invalid method %a for union" FId.pp
          fid
        |> error_no_info

  and eval_action_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (params : param list) (targs : targ list)
      (args : arg list) (args_default : id' list) (block : block) :
      Ctx.t * SSig.t =
    let ctx_callee = pre ctx_caller in
    check (targs = []) "(eval_action_call) action cannot have type arguments";
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, ssign =
          eval_block ~start:true Ctx.Local ctx_callee block
        in
        let ctx_caller = post ctx_caller ctx_callee in
        let ctx_caller =
          copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
        in
        (ctx_caller, ssign))

  and eval_function_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (tparams : tparam list) (params : param list)
      (targs : typ list) (args : arg list) (args_default : id' list)
      (block : block) : Ctx.t * SSig.t =
    let ctx_callee = pre ctx_caller in
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, ssign =
          eval_block ~start:true Ctx.Local ctx_callee block
        in
        let ctx_caller = post ctx_caller ctx_callee in
        let ctx_caller =
          copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
        in
        (ctx_caller, ssign))

  and eval_extern_function_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (fid : FId.t) (tparams : tparam list)
      (params : param list) (targs : typ list) (args : arg list)
      (args_default : id' list) : Ctx.t * SSig.t =
    let ctx_callee = pre ctx_caller in
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, ssign = Arch.eval_extern_func_call ctx_callee fid in
        let ctx_caller = post ctx_caller ctx_callee in
        let ctx_caller =
          copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
        in
        (ctx_caller, ssign))

  and eval_extern_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (oid : OId.t) (fid : FId.t) (tparams : tparam list)
      (params : param list) (targs : typ list) (args : arg list)
      (args_default : id' list) : Ctx.t * SSig.t =
    let ctx_callee = pre ctx_caller in
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, ssign =
          Arch.eval_extern_method_call ctx_callee oid fid
        in
        let ctx_caller = post ctx_caller ctx_callee in
        let ctx_caller =
          copyout cursor_caller ctx_caller Ctx.Local ctx_callee params lvalues
        in
        (ctx_caller, ssign))

  and eval_parser_apply_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (params : param list) (args : arg list)
      (args_default : id' list) (decls : decl list) (senv : SEnv.t) :
      Ctx.t * SSig.t =
    let ctx_callee = pre ctx_caller in
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Block ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, dsign = eval_decls Ctx.Block ctx_callee decls in
        cont_sig_to_ssig ctx_callee dsign (fun () ->
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
            let ctx_callee, ssign =
              eval_parser_state_machine Ctx.Block ctx_callee
                (SSig.Trans (`State "start"))
            in
            let ctx_caller = post ctx_caller ctx_callee in
            let ctx_caller =
              copyout cursor_caller ctx_caller Ctx.Block ctx_callee params
                lvalues
            in
            (ctx_caller, ssign)))

  and eval_control_apply_method_call ~pre ~post (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (params : param list) (args : arg list)
      (args_default : id' list) (decls : decl list) (fenv : FEnv.t)
      (block : block) : Ctx.t * SSig.t =
    let ctx_callee = pre ctx_caller in
    let params, args, params_default, args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, csign =
      copyin cursor_caller ctx_caller Ctx.Block ctx_callee params args
    in
    cont_sig_to_ssig ctx_caller csign (fun (ctx_callee, lvalues) ->
        let ctx_callee =
          copyin_default Ctx.Local ctx_callee params_default args_default
        in
        let ctx_callee, dsign = eval_decls Ctx.Block ctx_callee decls in
        cont_sig_to_ssig ctx_callee dsign (fun () ->
            let ctx_callee =
              { ctx_callee with block = { ctx_callee.block with fenv } }
            in
            let ctx_callee, ssign =
              eval_block ~start:true Ctx.Local ctx_callee block
            in
            let ctx_caller = post ctx_caller ctx_callee in
            let ctx_caller =
              copyout cursor_caller ctx_caller Ctx.Block ctx_callee params
                lvalues
            in
            (ctx_caller, ssign)))

  and eval_table_apply_method_call ~pre ~post (_cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (oid : OId.t) (table : Table.t) : Ctx.t * SSig.t =
    let id = oid |> List.rev |> List.hd in
    let ctx_callee = pre ctx_caller in
    let ctx_callee, ssign = eval_table Ctx.Block ctx_callee id table in
    let ctx_caller = post ctx_caller ctx_callee in
    (ctx_caller, ssign)

  (* Entry point: function call *)

  and eval_func (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : targ list) (args : arg list) : callkind =
    let (fid, func, args_default), cursor_func =
      let args = FId.to_names args in
      Ctx.find_f_at Ctx.find_func_at_opt cursor var_func args ctx
    in
    match cursor_func with
    | Ctx.Global -> InterGlobal { fid; func; targs; args; args_default }
    | Ctx.Block ->
        IntraBlock
          {
            oid = [];
            fid;
            venv_local = VEnv.empty;
            func;
            targs;
            args;
            args_default;
          }
    | Ctx.Local -> assert false

  and eval_func_call (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * SSig.t =
    let callkind = eval_func cursor ctx var_func targs args in
    eval_call cursor ctx callkind

  (* Entry point: method call *)

  and eval_method (cursor : Ctx.cursor) (ctx : Ctx.t) (lvalue_base : LValue.t)
      (member : member) (targs : targ list) (args : arg list) : Ctx.t * callkind
      =
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
          | ExternO (_, theta_block, venv_block, fenv_block) ->
              let fid, func, args_default =
                let args = FId.to_names args in
                FEnv.find_func (member, args) fenv_block
              in
              InterBlock
                {
                  oid;
                  fid;
                  theta_block;
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
                  theta_block = Theta.empty;
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
                  theta_block = Theta.empty;
                  venv_block;
                  func;
                  targs;
                  args;
                  args_default;
                }
          | TableO (_, venv_local, table) ->
              let fid, func, args_default =
                let fid = FId.to_fid ("apply" $ no_info) [] in
                let func = Func.TableApplyMethodF table in
                let fenv = FEnv.add fid func FEnv.empty in
                let args = FId.to_names args in
                FEnv.find_func (member, args) fenv
              in
              IntraBlock
                { oid; fid; venv_local; func; targs; args; args_default }
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
      (member : member) (targs : typ list) (args : arg list) : Ctx.t * SSig.t =
    let ctx, lsign = eval_lvalue_of_expr cursor ctx expr_base in
    cont_sig_to_ssig ctx lsign (fun lvalue_base ->
        let ctx, callkind =
          eval_method cursor ctx lvalue_base member targs args
        in
        eval_call cursor ctx callkind)
end
