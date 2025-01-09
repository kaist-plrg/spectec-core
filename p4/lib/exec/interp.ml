open Domain.Dom
open Il.Ast
open Driver
module L = Lang.Ast
module Value = Runtime_static.Value
module Numerics = Runtime_static.Numerics
module Func = Runtime_dynamic.Func
module Obj = Runtime_dynamic.Object
module Envs = Runtime_dynamic.Envs
module TEnv = Envs.TEnv
module FEnv = Envs.FEnv
module Sto = Envs.Sto
open Util.Source
open Util.Error

let error_no_info = error_interp_no_info
let error_pass_info = error_interp_pass_info

module Make (Arch : ARCH) : INTERP = struct
  (* Global store *)

  let sto = ref Envs.Sto.empty
  let init (_sto : Sto.t) : unit = sto := _sto

  (* L-value evaluation *)

  let rec eval_write_lvalue (cursor : Ctx.cursor) (ctx : Ctx.t) (lvalue : expr)
      (value : Value.t) : Ctx.t =
    let eval_write_lvalue_fields (fields : (member' * Value.t) list)
        (member : member) (value : Value.t) =
      List.map
        (fun (member', value') ->
          if member.it = member' then (member', value) else (member', value'))
        fields
    in
    match lvalue.it with
    | VarE { var } -> Ctx.update Ctx.update_value_opt cursor var value ctx
    | ExprAccE { expr_base; member } -> (
        let ctx, value_base = eval_expr cursor ctx expr_base in
        match value_base with
        | StructV fields ->
            let fields = eval_write_lvalue_fields fields member value in
            let value_base = Value.StructV fields in
            eval_write_lvalue cursor ctx expr_base value_base
        | HeaderV (_, fields) ->
            let fields = eval_write_lvalue_fields fields member value in
            let value_base = Value.HeaderV (true, fields) in
            eval_write_lvalue cursor ctx expr_base value_base
        | _ ->
            Format.asprintf "(TODO: eval_write_lvalue) %a" (Value.pp ~level:0)
              value
            |> failwith)
    | _ ->
        Format.asprintf "(TODO: eval_write_lvalue) %a" (Il.Pp.pp_expr ~level:0)
          lvalue
        |> failwith

  (* Argument evaluation *)

  and eval_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg) : Ctx.t * Value.t
      =
    try eval_arg' cursor ctx arg.it
    with InterpErr _ as err -> error_pass_info arg.at err

  and eval_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg') :
      Ctx.t * Value.t =
    match arg with
    | L.ExprA expr | L.NameA (_, Some expr) -> eval_expr cursor ctx expr
    | _ ->
        Format.asprintf "(TODO: eval_arg) %a" Il.Pp.pp_arg' arg |> error_no_info

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
    | ArrAccE { expr_base; expr_idx } ->
        eval_array_acc_expr cursor ctx expr_base expr_idx
    | BitAccE { expr_base; value_lo; value_hi } ->
        eval_bitstring_acc_expr cursor ctx expr_base value_lo value_hi
    | ExprAccE { expr_base; member } ->
        eval_expr_acc_expr cursor ctx expr_base member
    | CallFuncE { var_func; targs; args } ->
        eval_func_call_expr cursor ctx var_func targs args
    | CallMethodE { expr_base; member; targs; args } ->
        eval_method_call_expr cursor ctx expr_base member targs args
    | _ ->
        Format.asprintf "(TODO: eval_expr) %a" (Il.Pp.pp_expr' ~level:0) expr
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
    let ctx, value_l = eval_expr cursor ctx expr_l in
    let ctx, value_r = eval_expr cursor ctx expr_r in
    let value = Numerics.eval_binop binop value_l value_r in
    (ctx, value)

  and eval_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
      (expr_then : expr) (expr_else : expr) : Ctx.t * Value.t =
    let ctx, value_cond = eval_expr cursor ctx expr_cond in
    let value = Value.get_bool value_cond in
    let expr = if value then expr_then else expr_else in
    eval_expr cursor ctx expr

  and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : Il.Ast.typ)
      (expr : Il.Ast.expr) : Ctx.t * Value.t =
    let ctx, value = eval_expr cursor ctx expr in
    let value = Numerics.eval_cast typ.it value in
    (ctx, value)

  and eval_array_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (expr_idx : expr) : Ctx.t * Value.t =
    let ctx, value_base = eval_expr cursor ctx expr_base in
    let ctx, value_idx = eval_expr cursor ctx expr_idx in
    (* (TODO) Insert bounds check *)
    match value_base with
    | TupleV values | StackV (values, _, _) ->
        let idx = Value.get_num value_idx |> Bigint.to_int |> Option.get in
        let value = List.nth values idx in
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
      | StructV fields | HeaderV (_, fields) | UnionV fields ->
          List.assoc member.it fields
      | RefV path -> Value.RefV (path @ [ member.it ])
      | _ -> Format.asprintf "%a" (Value.pp ~level:0) value_base |> failwith
    in
    (ctx, value)

  and eval_func_call_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Value.t =
    let ctx, sign = eval_func_call cursor ctx var_func targs args in
    match sign with
    | Ret (Some value) -> (ctx, value)
    | _ ->
        "(eval_func_call_expr) function call as an expression must return a \
         value" |> error_no_info

  and eval_method_call_expr (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (member : member) (targs : typ list) (args : arg list)
      : Ctx.t * Value.t =
    let ctx, sign = eval_method_call cursor ctx expr_base member targs args in
    match sign with
    | Ret (Some value) -> (ctx, value)
    | _ ->
        "(eval_method_call_expr) method call as an expression must return a \
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
    | IfS { expr_cond; stmt_then; stmt_else } ->
        cont sign (fun () ->
            eval_if_stmt cursor ctx expr_cond stmt_then stmt_else)
    | BlockS { block } -> cont sign (fun () -> eval_block_stmt cursor ctx block)
    | ExitS -> cont sign (fun () -> (ctx, Sig.Exit))
    | RetS { expr_ret } ->
        cont sign (fun () -> eval_return_stmt cursor ctx expr_ret)
    | CallFuncS { var_func; targs; args } ->
        cont sign (fun () -> eval_func_call_stmt cursor ctx var_func targs args)
    | CallMethodS { expr_base; member; targs; args } ->
        cont sign (fun () ->
            eval_method_call_stmt cursor ctx expr_base member targs args)
    | TransS { expr_label } ->
        cont sign (fun () -> eval_trans_stmt cursor ctx expr_label)
    | DeclS { decl } -> cont sign (fun () -> eval_decl_stmt cursor ctx decl)
    | _ -> Format.asprintf "%a" (Il.Pp.pp_stmt' ~level:0) stmt |> error_no_info

  and eval_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (stmts : stmt list) : Ctx.t * Sig.t =
    List.fold_left
      (fun (ctx, sign) stmt -> eval_stmt cursor ctx sign stmt)
      (ctx, sign) stmts

  and eval_assign_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_l : expr)
      (expr_r : expr) : Ctx.t * Sig.t =
    let ctx, value_r = eval_expr cursor ctx expr_r in
    let ctx = eval_write_lvalue cursor ctx expr_l value_r in
    (ctx, Cont)

  and eval_if_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_cond : expr)
      (stmt_then : stmt) (stmt_else : stmt) : Ctx.t * Sig.t =
    let ctx, value_cond = eval_expr cursor ctx expr_cond in
    let value = Value.get_bool value_cond in
    let stmt = if value then stmt_then else stmt_else in
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

  and eval_func_call_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Sig.t =
    eval_func_call cursor ctx var_func targs args

  and eval_method_call_stmt (cursor : Ctx.cursor) (ctx : Ctx.t)
      (expr_base : expr) (member : member) (targs : typ list) (args : arg list)
      : Ctx.t * Sig.t =
    eval_method_call cursor ctx expr_base member targs args

  and eval_trans_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_label : expr) :
      Ctx.t * Sig.t =
    let ctx, value_label = eval_expr cursor ctx expr_label in
    let label = Value.get_state value_label in
    let var_state = L.Current (label $ no_info) $ no_info in
    eval_func_call cursor ctx var_state [] []

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
        Format.asprintf "(TODO: eval_decl) %a" (Il.Pp.pp_decl' ~level:0) decl
        |> failwith

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

  (* Table evaluation *)

  and eval_table (cursor : Ctx.cursor) (ctx : Ctx.t) (table : table) :
      Ctx.t * Sig.t =
    let table_default =
      List.filter_map
        (fun table_property ->
          match table_property with
          | L.DefaultP table_default -> Some table_default
          | _ -> None)
        table
      |> List.hd
    in
    let action_default, _ = table_default.it in
    let var_action, args_action, _annos_action = action_default.it in
    let stmt_call_action =
      CallFuncS { var_func = var_action; targs = []; args = args_action }
      $ action_default.at
    in
    eval_stmt cursor ctx Sig.Cont stmt_call_action

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
      (args : arg list) : Ctx.t * Ctx.t =
    let copyin' ((ctx_caller, ctx_callee) : Ctx.t * Ctx.t) (param : param)
        (arg : arg) : Ctx.t * Ctx.t =
      let id, dir, typ, _, _ = param.it in
      let ctx_caller, value =
        match dir.it with
        | No | In | InOut -> eval_arg cursor_caller ctx_caller arg
        | Out ->
            let value =
              Ctx.resolve_typ cursor_callee typ.it ctx_callee
              |> Numerics.eval_default
            in
            (ctx_caller, value)
      in
      let ctx_callee = Ctx.add_value cursor_callee id.it value ctx_callee in
      (ctx_caller, ctx_callee)
    in
    List.fold_left2 copyin' (ctx_caller, ctx_callee) params args

  and copyout (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (cursor_callee : Ctx.cursor) (ctx_callee : Ctx.t) (params : param list)
      (args : arg list) : Ctx.t =
    let copyout' (ctx_caller : Ctx.t) (param : param) (arg : arg) : Ctx.t =
      let id, dir, _, _, _ = param.it in
      match dir.it with
      | InOut | Out -> (
          let value = Ctx.find_value cursor_callee id.it ctx_callee in
          match arg.it with
          | ExprA expr | NameA (_, Some expr) ->
              eval_write_lvalue cursor_caller ctx_caller expr value
          | _ -> ctx_caller)
      | _ -> ctx_caller
    in
    List.fold_left2 copyout' ctx_caller params args

  (* Inter-block call *)

  and eval_inter_call (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (ctx_callee : Ctx.t) (oid : OId.t) (fid : FId.t) (func : Func.t)
      (targs : typ list) (args : arg list) (args_default : id' list) :
      Ctx.t * Sig.t =
    match func with
    (* Callee enters local layer *)
    | ExternMethodF (tparams, params, None) ->
        eval_inter_extern_method_call cursor_caller ctx_caller ctx_callee oid
          fid tparams params targs args args_default
    (* Callee enters block layer, then into local layer *)
    | ParserApplyMethodF (params, venv, fenv, locals, block) ->
        assert (targs = []);
        let ctx_callee =
          let venv = VEnv.extend_nodup ctx_callee.block.venv venv in
          { ctx_callee with block = { tenv = TEnv.empty; venv; fenv } }
        in
        eval_inter_apply_method_call cursor_caller ctx_caller ctx_callee params
          locals block args args_default
    | ControlApplyMethodF (params, fenv, locals, block) ->
        assert (targs = []);
        let ctx_callee =
          {
            ctx_callee with
            block = { tenv = TEnv.empty; venv = VEnv.empty; fenv };
          }
        in
        eval_inter_apply_method_call cursor_caller ctx_caller ctx_callee params
          locals block args args_default
    | _ ->
        Format.asprintf "(TODO: eval_inter_call) %a" (Func.pp ~level:0) func
        |> failwith

  and eval_inter_extern_method_call (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (oid : OId.t) (fid : FId.t)
      (tparams : tparam list) (params : param list) (targs : typ list)
      (args : arg list) (args_default : id' list) : Ctx.t * Sig.t =
    let ctx_callee = Ctx.add_typs Ctx.Local tparams targs ctx_callee in
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign = Arch.eval_extern ctx_callee oid fid in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    (ctx_caller, sign)

  and eval_inter_apply_method_call (cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (params : param list)
      (locals : decl list) (block : block) (args : arg list)
      (args_default : id' list) : Ctx.t * Sig.t =
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee =
      copyin cursor_caller ctx_caller Ctx.Block ctx_callee params args
    in
    let ctx_callee = eval_decls Ctx.Block ctx_callee locals in
    let ctx_callee, sign =
      let stmt_block = BlockS { block } $ no_info in
      eval_stmt Ctx.Local ctx_callee Sig.Cont stmt_block
    in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    (ctx_caller, sign)

  (* Intra-block call *)

  and eval_intra_call (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (ctx_callee : Ctx.t) (_oid : OId.t) (_fid : FId.t) (func : Func.t)
      (targs : typ list) (args : arg list) (args_default : id' list) :
      Ctx.t * Sig.t =
    match func with
    (* Callee enters local layer *)
    | ActionF (params, block) ->
        assert (targs = []);
        eval_intra_action_call cursor_caller ctx_caller ctx_callee params block
          args args_default
    | ParserStateF block ->
        assert (targs = [] && args = [] && args_default = []);
        eval_intra_parser_state_call cursor_caller ctx_caller ctx_callee block
    | TableApplyMethodF table ->
        assert (targs = [] && args = [] && args_default = []);
        eval_intra_table_apply_method_call cursor_caller ctx_caller ctx_callee
          table
    | _ ->
        Format.asprintf "(TODO: eval_intra_call) %a" (Func.pp ~level:0) func
        |> failwith

  and eval_intra_action_call (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (ctx_callee : Ctx.t) (params : param list) (block : block)
      (args : arg list) (args_default : id' list) : Ctx.t * Sig.t =
    let params, args, _params_default, _args_default =
      align_params_with_args params args args_default
    in
    let ctx_caller, ctx_callee =
      copyin cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    let ctx_callee, sign =
      let stmt_block = BlockS { block } $ no_info in
      eval_stmt Ctx.Local ctx_callee Sig.Cont stmt_block
    in
    let ctx_caller = { ctx_caller with block = ctx_callee.block } in
    let ctx_caller =
      copyout cursor_caller ctx_caller Ctx.Local ctx_callee params args
    in
    (ctx_caller, sign)

  and eval_intra_parser_state_call (_cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (block : block) : Ctx.t * Sig.t
      =
    let ctx_callee, sign =
      let stmt_block = BlockS { block } $ no_info in
      eval_stmt Ctx.Local ctx_callee Sig.Cont stmt_block
    in
    let ctx_callee = Ctx.copy Ctx.Block ctx_callee in
    let ctx_caller = { ctx_caller with block = ctx_callee.block } in
    (ctx_caller, sign)

  and eval_intra_table_apply_method_call (_cursor_caller : Ctx.cursor)
      (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (table : table) : Ctx.t * Sig.t
      =
    let ctx_callee, sign = eval_table Ctx.Block ctx_callee table in
    let ctx_caller = { ctx_caller with block = ctx_callee.block } in
    (ctx_caller, sign)

  (* Entry point: function call *)

  and eval_func_call (cursor : Ctx.cursor) (ctx : Ctx.t) (var_func : var)
      (targs : typ list) (args : arg list) : Ctx.t * Sig.t =
    (* Format.printf "Call %a%a%a\n" Il.Pp.pp_var var_func *)
    (*   (Il.Pp.pp_targs ~level:0) targs Il.Pp.pp_args args; *)
    let (fid, func, args_default), cursor_func =
      let args = FId.to_names args in
      Ctx.find_f_at Ctx.find_func_at_opt cursor var_func args ctx
    in
    match cursor_func with
    | Ctx.Global ->
        let ctx_callee = Ctx.copy Ctx.Global ctx in
        eval_inter_call cursor ctx ctx_callee [] fid func targs args
          args_default
    | Ctx.Block ->
        let ctx_callee = Ctx.copy Ctx.Block ctx in
        eval_intra_call cursor ctx ctx_callee [] fid func targs args
          args_default
    | Ctx.Local -> assert false

  (* Entry point: method call *)

  and eval_method_call (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (member : member) (targs : typ list) (args : arg list) : Ctx.t * Sig.t =
    (* Format.printf "Call %a.%a%a%a\n" (Il.Pp.pp_expr ~level:0) expr_base *)
    (*   Il.Pp.pp_member member (Il.Pp.pp_targs ~level:0) targs Il.Pp.pp_args args; *)
    let ctx, value_base = eval_expr cursor ctx expr_base in
    match value_base with
    | RefV path ->
        let obj = Sto.find path !sto in
        eval_obj_call cursor ctx path obj member targs args
    | _ -> Format.asprintf "%a" (Value.pp ~level:0) value_base |> failwith

  and eval_obj_call (cursor_caller : Ctx.cursor) (ctx_caller : Ctx.t)
      (oid : OId.t) (obj : Obj.t) (member : member) (targs : typ list)
      (args : arg list) : Ctx.t * Sig.t =
    match obj with
    (* Inter-block call *)
    | ExternO (venv, fenv) ->
        let fid, func, args_default =
          let args = FId.to_names args in
          FEnv.find_func (member.it, args) fenv
        in
        let ctx_callee = Ctx.copy Ctx.Global ctx_caller in
        let ctx_callee =
          { ctx_callee with block = { ctx_callee.block with venv } }
        in
        eval_inter_call cursor_caller ctx_caller ctx_callee oid fid func targs
          args args_default
    | ParserO (venv, fenv) | ControlO (venv, fenv) ->
        let fid, func, args_default =
          let args = FId.to_names args in
          FEnv.find_func (member.it, args) fenv
        in
        let ctx_callee = Ctx.copy Ctx.Global ctx_caller in
        let ctx_callee =
          { ctx_callee with block = { ctx_callee.block with venv } }
        in
        eval_inter_call cursor_caller ctx_caller ctx_callee oid fid func targs
          args args_default
    (* Intra-block call *)
    | TableO fenv ->
        let fid, func, args_default =
          let args = FId.to_names args in
          FEnv.find_func (member.it, args) fenv
        in
        let ctx_callee = Ctx.copy Ctx.Block ctx_caller in
        eval_intra_call cursor_caller ctx_caller ctx_callee oid fid func targs
          args args_default
    | _ -> Format.asprintf "%a" (Obj.pp ~level:0) obj |> failwith
end
