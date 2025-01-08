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
        let value_base = eval_expr cursor ctx expr_base in
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

  and eval_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (arg : arg) : Value.t =
    match arg.it with
    | L.ExprA expr | L.NameA (_, Some expr) -> eval_expr cursor ctx expr
    | _ -> Format.asprintf "%a" Il.Pp.pp_arg arg |> failwith

  (* Expression evaluation *)

  and eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr : expr) : Value.t =
    match expr.it with
    | ValueE { value } -> value.it
    | VarE { var } -> eval_var_expr cursor ctx var
    | BinE { binop; expr_l; expr_r } ->
        eval_binop_expr cursor ctx binop expr_l expr_r
    | CastE { typ; expr } -> eval_cast_expr cursor ctx typ expr
    | ExprAccE { expr_base; member } ->
        eval_expr_acc_expr cursor ctx expr_base member
    | _ -> Format.asprintf "%a" (Il.Pp.pp_expr ~level:0) expr |> failwith

  and eval_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (var : var) : Value.t =
    let value = Ctx.find Ctx.find_value_opt cursor var ctx in
    value

  and eval_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (binop : binop)
      (expr_l : expr) (expr_r : expr) : Value.t =
    let value_l = eval_expr cursor ctx expr_l in
    let value_r = eval_expr cursor ctx expr_r in
    let value = Numerics.eval_binop binop value_l value_r in
    value

  and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (typ : Il.Ast.typ)
      (expr : Il.Ast.expr) : Value.t =
    let value = eval_expr cursor ctx expr in
    let value = Numerics.eval_cast typ.it value in
    value

  and eval_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (expr_base : expr)
      (member : member) : Value.t =
    let value_base = eval_expr cursor ctx expr_base in
    match value_base with
    | StructV fields | HeaderV (_, fields) | UnionV fields ->
        List.assoc member.it fields
    | RefV path -> Value.RefV (path @ [ member.it ])
    | _ -> Format.asprintf "%a" (Value.pp ~level:0) value_base |> failwith

  (* Statement evaluation *)

  let rec eval_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (stmt : stmt) : Ctx.t * Sig.t =
    match stmt.it with
    | AssignS { expr_l; expr_r } ->
        eval_assign_stmt cursor ctx sign expr_l expr_r
    | BlockS { block } -> eval_block_stmt cursor ctx sign block
    | CallFuncS { var_func; targs; args } ->
        eval_func_call cursor ctx var_func targs args
    | CallMethodS { expr_base; member; targs; args } ->
        eval_method_call cursor ctx expr_base member targs args
    | TransS { expr_label } -> eval_trans_stmt cursor ctx sign expr_label
    | _ -> Format.asprintf "%a" (Il.Pp.pp_stmt ~level:0) stmt |> failwith

  and eval_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (stmts : stmt list) : Ctx.t * Sig.t =
    List.fold_left
      (fun (ctx, sign) stmt -> eval_stmt cursor ctx sign stmt)
      (ctx, sign) stmts

  and eval_assign_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (expr_l : expr) (expr_r : expr) : Ctx.t * Sig.t =
    let value_r = eval_expr cursor ctx expr_r in
    let ctx = eval_write_lvalue cursor ctx expr_l value_r in
    (ctx, sign)

  and eval_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sign : Sig.t)
      (block : block) : Ctx.t * Sig.t =
    let stmts, _annos = block.it in
    let ctx = Ctx.enter_frame ctx in
    let ctx, sign = eval_stmts cursor ctx sign stmts in
    let ctx = Ctx.exit_frame ctx in
    (ctx, sign)

  and eval_trans_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (_sign : Sig.t)
      (expr_label : expr) : Ctx.t * Sig.t =
    let value_label = eval_expr cursor ctx expr_label in
    let label = Value.get_state value_label in
    let var_state = L.Current (label $ no_info) $ no_info in
    eval_func_call cursor ctx var_state [] []

  (* Declaration evaluation *)

  and eval_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (decl : decl) : Ctx.t =
    match decl.it with
    | ConstD { id; typ; value; annos } ->
        eval_const_decl cursor ctx id typ value annos
    | VarD { id; typ; init; annos } ->
        eval_var_decl cursor ctx id typ init annos
    | _ -> Format.asprintf "%a" (Il.Pp.pp_decl ~level:0) decl |> failwith

  and eval_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (decls : decl list) : Ctx.t
      =
    List.fold_left (eval_decl cursor) ctx decls

  and eval_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (_typ : typ)
      (value : value) (_annos : anno list) : Ctx.t =
    Ctx.add_value cursor id.it value.it ctx

  and eval_var_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (typ : typ)
      (init : expr option) (_annos : anno list) : Ctx.t =
    let value =
      match init with
      | Some expr -> eval_expr cursor ctx expr
      | None -> Ctx.resolve_typ cursor typ.it ctx |> Numerics.eval_default
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
      (args : arg list) : Ctx.t =
    let copyin' (ctx_callee : Ctx.t) (param : param) (arg : arg) : Ctx.t =
      let id, dir, typ, _, _ = param.it in
      let value =
        match dir.it with
        | No | In | InOut -> eval_arg cursor_caller ctx_caller arg
        | Out ->
            Ctx.resolve_typ cursor_callee typ.it ctx_callee
            |> Numerics.eval_default
      in
      Ctx.add_value cursor_callee id.it value ctx_callee
    in
    List.fold_left2 copyin' ctx_callee params args

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
    let ctx_callee =
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
    let ctx_callee =
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
    let ctx_callee =
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
    let value_base = eval_expr cursor ctx expr_base in
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
