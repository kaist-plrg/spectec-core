open Domain.Dom
open Il.Ast
module L = Lang.Ast
module Value = Runtime_static.Value
module Func = Runtime_dynamic.Func
module Cons = Runtime_dynamic.Cons
module Obj = Runtime_dynamic.Object
module Envs = Runtime_dynamic.Envs
module Sto = Envs.Sto
module F = Format
open Util.Source
open Util.Error

let error_info = error_inst_info

(* Instantiation logic (recursive) *)

let align_cparams_with_args (cparams : cparam list) (args : arg list)
    (args_default : id' list) :
    cparam list * arg list * cparam list * value list =
  let module PMap = Map.Make (String) in
  let cparams, cparams_default =
    List.partition
      (fun cparam ->
        let id, _, _, _, _ = cparam.it in
        not (List.mem id.it args_default))
      cparams
  in
  let cparams_map =
    List.fold_left
      (fun cparams_map cparam ->
        let id, _, _, _, _ = cparam.it in
        PMap.add id.it cparam cparams_map)
      PMap.empty cparams
  in
  let cparams, args =
    List.fold_left2
      (fun (cparams, args) cparam arg ->
        match arg.it with
        | L.ExprA _ | L.AnyA -> (cparams @ [ cparam ], args @ [ arg ])
        | L.NameA (id, _) ->
            let cparam = PMap.find id.it cparams_map in
            (cparams @ [ cparam ], args @ [ arg ]))
      ([], []) cparams args
  in
  let args_default =
    List.map
      (fun cparam_default ->
        let _, _, _, value_default, _ = cparam_default.it in
        Option.get value_default)
      cparams_default
  in
  (cparams, args, cparams_default, args_default)

let rec do_instantiate (cursor : Ctx.cursor) (ctx_caller : Ctx.t) (sto : Sto.t)
    (cons : Cons.t) (_targs : typ list) (args : arg list)
    (args_default : id' list) : Sto.t * Obj.t =
  (* Unpack constructor *)
  let _tparams, cparams, do_instantiate_cons =
    match cons with
    | ExternC (tparams, cparams, mthds) ->
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_extern Ctx.Block ctx_callee sto mthds
        in
        (tparams, cparams, do_instantiate_cons)
    | ParserC (tparams, cparams, params, decls, states) ->
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_parser Ctx.Block ctx_callee sto params decls states
        in
        (tparams, cparams, do_instantiate_cons)
    | ControlC (tparams, cparams, params, decls, body) ->
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_control Ctx.Block ctx_callee sto params decls body
        in
        (tparams, cparams, do_instantiate_cons)
    | PackageC (tparams, cparams) ->
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_package Ctx.Block ctx_callee sto
        in
        (tparams, cparams, do_instantiate_cons)
    | TableC table ->
        let do_instantiate_table ctx_callee sto =
          do_instantiate_table Ctx.Block ctx_callee sto table
        in
        ([], [], do_instantiate_table)
  in
  (* Initialize callee context *)
  let ctx_callee =
    let ctx_callee = Ctx.empty in
    { ctx_callee with path = ctx_caller.path; global = ctx_caller.global }
  in
  (* Bind constructor parameters to the callee context *)
  let cparams, args, cparams_default, args_default =
    align_cparams_with_args cparams args args_default
  in
  let ctx_callee, sto =
    List.fold_left2
      (fun (ctx_callee, sto) cparam arg ->
        let id, _, _, _, _ = cparam.it in
        let sto, value =
          let ctx_caller = Ctx.enter_path id.it ctx_caller in
          eval_arg cursor ctx_caller sto arg
        in
        let ctx_callee = Ctx.add_value Ctx.Block id.it value ctx_callee in
        (ctx_callee, sto))
      (ctx_callee, sto) cparams args
  in
  let ctx_callee =
    List.fold_left2
      (fun ctx_callee cparam_default value_default ->
        let id, _, _, _, _ = cparam_default.it in
        Ctx.add_value Ctx.Block id.it value_default.it ctx_callee)
      ctx_callee cparams_default args_default
  in
  (* Instantiate callee *)
  let sto, obj = do_instantiate_cons ctx_callee sto in
  (sto, obj)

and do_instantiate_extern (_cursor : Ctx.cursor) (_ctx : Ctx.t) (_sto : Sto.t)
    (_mthds : mthd list) : Sto.t * Obj.t =
  failwith "(TODO) do_instantiate_extern"

and do_instantiate_parser (_cursor : Ctx.cursor) (_ctx : Ctx.t) (_sto : Sto.t)
    (_params : param list) (_decls : decl list) (_states : parser_state list) :
    Sto.t * Obj.t =
  failwith "(TODO) do_instantiate_parser"

and do_instantiate_control (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (params : param list) (decls : decl list) (body : block) : Sto.t * Obj.t =
  assert (cursor = Ctx.Block);
  let ctx_apply, sto, decls = eval_decls cursor ctx sto decls in
  let ctx_apply, sto, body = eval_block Ctx.Local ctx_apply sto body in
  let func_apply =
    let venv_apply = IdMap.diff ctx_apply.block.venv ctx.block.venv in
    let fenv_apply = FIdMap.diff ctx_apply.block.fenv ctx.block.fenv in
    Func.ControlApplyMethodF (params, venv_apply, fenv_apply, decls, body)
  in
  let ctx_control =
    let fid_apply = FId.to_fid ("apply" $ no_info) params in
    Ctx.add_func_non_overload cursor fid_apply func_apply ctx
  in
  let obj = Obj.ControlO (ctx_control.block.venv, ctx_control.block.fenv) in
  (sto, obj)

and do_instantiate_package (_cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) :
    Sto.t * Obj.t =
  let obj = Obj.PackageO ctx.block.venv in
  (sto, obj)

and do_instantiate_table (_cursor : Ctx.cursor) (_ctx : Ctx.t) (sto : Sto.t)
    (table : table) : Sto.t * Obj.t =
  let obj = Obj.TableO table in
  (sto, obj)

(* Argument evaluation *)

and eval_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (arg : arg) :
    Sto.t * Value.t =
  match arg.it with
  | L.ExprA expr | L.NameA (_, Some expr) -> eval_expr cursor ctx sto expr
  | _ ->
      F.asprintf "(eval_arg) instantiation arguments must not be missing"
      |> error_info arg.at

(* Statement evaluation *)

and eval_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (stmt : stmt) :
    Ctx.t * Sto.t * stmt option =
  let wrap ~at (ctx, sto, stmt) =
    (ctx, sto, Option.map (fun stmt -> stmt $ at) stmt)
  in
  let wrap_some ~at (ctx, sto, stmt) = (ctx, sto, Some (stmt $ at)) in
  let wrap_none (ctx, sto) = (ctx, sto, None) in
  match stmt.it with
  | L.BlockS { block } ->
      eval_block_stmt cursor ctx sto block |> wrap_some ~at:stmt.at
  | L.CallInstS { var_inst; targs; args } ->
      eval_call_inst_stmt cursor ctx sto var_inst targs args |> wrap_none
  | L.DeclS { decl } -> eval_decl_stmt cursor ctx sto decl |> wrap ~at:stmt.at
  | _ -> (ctx, sto, stmt.it) |> wrap_some ~at:stmt.at

and eval_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (stmts : stmt list) : Ctx.t * Sto.t * stmt list =
  List.fold_left
    (fun (ctx, sto, stmts) stmt ->
      let ctx, sto, stmt = eval_stmt cursor ctx sto stmt in
      let stmts =
        match stmt with Some stmt -> stmts @ [ stmt ] | None -> stmts
      in
      (ctx, sto, stmts))
    (ctx, sto, []) stmts

and eval_block (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (block : block)
    : Ctx.t * Sto.t * block =
  let stmts, annos = block.it in
  let ctx = Ctx.enter_frame ctx in
  let ctx, sto, stmts = eval_stmts cursor ctx sto stmts in
  let ctx = Ctx.exit_frame ctx in
  let block = (stmts, annos) $ block.at in
  (ctx, sto, block)

and eval_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (block : block) : Ctx.t * Sto.t * stmt' =
  let ctx, sto, block = eval_block cursor ctx sto block in
  (ctx, sto, L.BlockS { block })

and eval_call_inst_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (var_inst : var) (targs : typ list) (args : arg list) : Ctx.t * Sto.t =
  let cons, args_default =
    let args = FId.to_names args in
    Ctx.find_overloaded Ctx.find_cons_opt cursor var_inst args ctx
  in
  let sto, obj = do_instantiate cursor ctx sto cons targs args args_default in
  let id = F.asprintf "%a" Il.Pp.pp_var var_inst in
  let oid = ctx.path @ [ id ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let ctx = Ctx.add_value cursor id value ctx in
  (ctx, sto)

and eval_decl_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (decl : decl) : Ctx.t * Sto.t * stmt' option =
  let ctx, sto, decl = eval_decl cursor ctx sto decl in
  let stmt = Option.map (fun decl -> L.DeclS { decl }) decl in
  (ctx, sto, stmt)

(* Expression evaluation *)

and eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (expr : expr) :
    Sto.t * Value.t =
  let wrap_value value = (sto, value) in
  match expr.it with
  | ValueE { value } -> value.it |> wrap_value
  | InstE { var_inst; targs; args } ->
      eval_inst_expr cursor ctx sto var_inst targs args
  | _ -> failwith "(TODO) eval_expr"

and eval_inst_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (var_inst : var) (targs : typ list) (args : arg list) : Sto.t * Value.t =
  let cons, args_default =
    let args = FId.to_names args in
    Ctx.find_overloaded Ctx.find_cons_opt cursor var_inst args ctx
  in
  let sto, obj = do_instantiate cursor ctx sto cons targs args args_default in
  let oid = ctx.path in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  (sto, value)

(* Declaration evaluation *)

and eval_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (decl : decl) :
    Ctx.t * Sto.t * decl option =
  let wrap_ctx_some ctx = (ctx, sto, Some decl) in
  let wrap_none (ctx, sto) = (ctx, sto, None) in
  let wrap_ctx_none ctx = (ctx, sto, None) in
  match decl.it with
  | ConstD { id; typ; value; annos } ->
      eval_const_decl cursor ctx id typ value annos |> wrap_ctx_some
  | VarD _ -> ctx |> wrap_ctx_some
  | InstD { id; var_inst; targs; args; init; annos } ->
      eval_inst_decl cursor ctx sto id var_inst targs args init annos
      |> wrap_none
  | ValueSetD _ -> ctx |> wrap_ctx_some
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      eval_parser_decl cursor ctx id tparams params cparams locals states annos
      |> wrap_ctx_none
  | TableD { id; table; annos } ->
      eval_table_decl cursor ctx sto id table annos |> wrap_none
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      eval_control_decl cursor ctx id tparams params cparams locals body annos
      |> wrap_ctx_none
  | ActionD { id; params; body; annos } ->
      eval_action_decl cursor ctx id params body annos |> wrap_ctx_none
  | FuncD { id; typ_ret; tparams; params; body } ->
      eval_func_decl cursor ctx id typ_ret tparams params body |> wrap_ctx_none
  | ExternFuncD { id; typ_ret; tparams; params; annos } ->
      eval_extern_func_decl cursor ctx id typ_ret tparams params annos
      |> wrap_ctx_none
  | ExternObjectD { id; tparams; mthds; annos } ->
      eval_extern_object_decl cursor ctx id tparams mthds annos |> wrap_ctx_none
  | PackageTypeD { id; tparams; cparams; annos } ->
      eval_package_type_decl cursor ctx id tparams cparams annos
      |> wrap_ctx_none

and eval_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (_typ : typ)
    (value : value) (_annos : anno list) : Ctx.t =
  Ctx.add_value cursor id.it value.it ctx

and eval_inst_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (id : id)
    (var_inst : var) (targs : typ list) (args : arg list) (_init : decl list)
    (_annos : anno list) : Ctx.t * Sto.t =
  let cons, args_default =
    let args = FId.to_names args in
    Ctx.find_overloaded Ctx.find_cons_opt cursor var_inst args ctx
  in
  let sto, obj =
    let ctx = Ctx.enter_path id.it ctx in
    do_instantiate cursor ctx sto cons targs args args_default
  in
  let oid = ctx.path @ [ id.it ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let ctx = Ctx.add_value cursor id.it value ctx in
  (ctx, sto)

and eval_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (states : parser_state list) (_annos : anno list) :
    Ctx.t =
  let cid = FId.to_fid id cparams in
  let cons = Cons.ParserC (tparams, cparams, params, locals, states) in
  Ctx.add_cons cursor cid cons ctx

and eval_table_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (id : id)
    (table : table) (_annos : anno list) : Ctx.t * Sto.t =
  let cons = Cons.TableC table in
  let sto, obj =
    let ctx = Ctx.enter_path id.it ctx in
    do_instantiate cursor ctx sto cons [] [] []
  in
  let oid = ctx.path @ [ id.it ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let ctx = Ctx.add_value cursor id.it value ctx in
  (ctx, sto)

and eval_control_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (body : block) (_annos : anno list) : Ctx.t =
  let cid = FId.to_fid id cparams in
  let cons = Cons.ControlC (tparams, cparams, params, locals, body) in
  Ctx.add_cons cursor cid cons ctx

and eval_action_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (params : param list) (body : block) (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ActionF (params, body) in
  Ctx.add_func_non_overload cursor fid func ctx

and eval_func_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (params : param list)
    (body : block) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.FuncF (tparams, params, body) in
  Ctx.add_func_overload cursor fid func ctx

and eval_extern_func_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (params : param list)
    (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ExternFuncF (tparams, params) in
  Ctx.add_func_overload cursor fid func ctx

and eval_extern_object_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (mthds : mthd list) (_annos : anno list) : Ctx.t =
  let conss, mthds =
    List.partition
      (fun mthd -> match mthd.it with L.ExternConsM _ -> true | _ -> false)
      mthds
  in
  List.fold_left
    (fun ctx cons ->
      match cons.it with
      | L.ExternConsM { id = _id; cparams; annos = _annos } ->
          let cid = FId.to_fid id cparams in
          let cons = Cons.ExternC (tparams, cparams, mthds) in
          Ctx.add_cons cursor cid cons ctx
      | _ -> assert false)
    ctx conss

and eval_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (cparams : cparam list) (_annos : anno list) : Ctx.t
    =
  let cid = FId.to_fid id cparams in
  let cons = Cons.PackageC (tparams, cparams) in
  Ctx.add_cons cursor cid cons ctx

and eval_decls (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (decls : decl list) : Ctx.t * Sto.t * decl list =
  List.fold_left
    (fun (ctx, sto, decls) decl ->
      let ctx, sto, decl = eval_decl cursor ctx sto decl in
      let decls =
        match decl with Some decl -> decls @ [ decl ] | None -> decls
      in
      (ctx, sto, decls))
    (ctx, sto, []) decls

let instantiate_program (program : program) : unit =
  let _ctx, _sto, _decls = eval_decls Ctx.Global Ctx.empty Sto.empty program in
  F.printf "%a\n" Sto.pp _sto;
  ()
