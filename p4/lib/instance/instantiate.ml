module F = Format
open Domain.Dom
module L = Lang.Ast
module Ctk = Runtime_static.Ctk
module Value = Runtime_static.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Numerics = Runtime_static.Numerics
module Builtins = Runtime_static.Builtins
open Il.Ast
module Table = Runtime_dynamic.Table
module Func = Runtime_dynamic.Func
module Cons = Runtime_dynamic.Cons
module Obj = Runtime_dynamic.Object
module Envs = Runtime_dynamic.Envs
module VEnv = Envs.VEnv
module FEnv = Envs.FEnv
module CEnv = Envs.CEnv
module Sto = Envs.Sto
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
    | ExternC (id, tparams, cparams, mthds) ->
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_extern Ctx.Block ctx_callee sto id mthds
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
    | TableC (id, table) ->
        let do_instantiate_table ctx_callee sto =
          do_instantiate_table Ctx.Block ctx_callee sto id table
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

and do_instantiate_extern (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (id : id') (mthds : mthd list) : Sto.t * Obj.t =
  assert (cursor = Ctx.Block);
  let ctx = eval_mthds cursor ctx mthds in
  let venv = ctx.block.venv in
  let fenv = ctx.block.fenv in
  let obj = Obj.ExternO (id, venv, fenv) in
  (sto, obj)

and do_instantiate_parser (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (params : param list) (decls : decl list) (states : parser_state list) :
    Sto.t * Obj.t =
  assert (cursor = Ctx.Block);
  let ctx_apply_locals, sto, decls = eval_decls cursor ctx sto decls in
  let ctx_apply_states, sto =
    eval_parser_states cursor ctx_apply_locals sto states
  in
  let venv = ctx.block.venv in
  let senv = ctx_apply_states.block.senv in
  let obj = Obj.ParserO (venv, params, decls, senv) in
  (sto, obj)

and do_instantiate_control (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (params : param list) (decls : decl list) (block : block) : Sto.t * Obj.t =
  assert (cursor = Ctx.Block);
  let ctx_apply_locals, sto, decls = eval_decls cursor ctx sto decls in
  let _ctx_apply_block, sto, block =
    eval_block Ctx.Local ctx_apply_locals sto block
  in
  let venv = ctx.block.venv in
  let fenv = ctx_apply_locals.block.fenv in
  let obj = Obj.ControlO (venv, params, decls, fenv, block) in
  (sto, obj)

and do_instantiate_package (_cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) :
    Sto.t * Obj.t =
  let obj = Obj.PackageO ctx.block.venv in
  (sto, obj)

(* (TODO) Handle custom table properties *)
and do_instantiate_table (_cursor : Ctx.cursor) (_ctx : Ctx.t) (sto : Sto.t)
    (id : id') (table : table) : Sto.t * Obj.t =
  let table = Table.init table in
  let obj = Obj.TableO (id, table) in
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
  match stmt.it with
  | BlockS { block } ->
      eval_block_stmt cursor ctx sto block |> wrap_some ~at:stmt.at
  | CallInstS { typ; var_inst; targs; args } ->
      eval_call_inst_stmt cursor ctx sto typ var_inst targs args
      |> wrap_some ~at:stmt.at
  | DeclS { decl } -> eval_decl_stmt cursor ctx sto decl |> wrap ~at:stmt.at
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
  (ctx, sto, BlockS { block })

and eval_call_inst_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (typ : typ) (var_inst : var) (targs : typ list) (args : arg list) :
    Ctx.t * Sto.t * stmt' =
  let _, cons, _ =
    Ctx.find_overloaded Ctx.find_cons_opt cursor var_inst [] ctx
  in
  let sto, obj = do_instantiate cursor ctx sto cons targs [] [] in
  let id = F.asprintf "%a" Il.Pp.pp_var var_inst in
  let oid = ctx.path @ [ id ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let stmt =
    let expr_inst =
      ValueE { value = value $ no_info }
      $$ (no_info, { typ = typ.it; ctk = Ctk.CTK })
    in
    let decl_inst =
      VarD { id = id $ no_info; typ; init = Some expr_inst; annos = [] }
      $ no_info
    in
    let stmt_decl = DeclS { decl = decl_inst } $ no_info in
    let expr_base =
      VarE { var = L.Current (id $ no_info) $ no_info }
      $$ (no_info, { typ = typ.it; ctk = Ctk.CTK })
    in
    let stmt_call =
      CallMethodS { expr_base; member = "apply" $ no_info; targs; args }
      $ no_info
    in
    let block = ([ stmt_decl; stmt_call ], []) $ no_info in
    BlockS { block }
  in
  (ctx, sto, stmt)

and eval_decl_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (decl : decl) : Ctx.t * Sto.t * stmt' option =
  let ctx, sto, decl = eval_decl cursor ctx sto decl in
  let stmt = Option.map (fun decl -> DeclS { decl }) decl in
  (ctx, sto, stmt)

(* Expression evaluation *)

and eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (expr : expr) :
    Sto.t * Value.t =
  let wrap_value value = (sto, value) in
  match expr.it with
  | ValueE { value } -> value.it |> wrap_value
  | VarE { var } -> eval_var_expr cursor ctx sto var
  | SeqE { exprs } -> eval_seq_expr cursor ctx sto exprs
  | SeqDefaultE { exprs } -> eval_seq_default_expr cursor ctx sto exprs
  | RecordE { fields } -> eval_record_expr cursor ctx sto fields
  | RecordDefaultE { fields } -> eval_record_default_expr cursor ctx sto fields
  | DefaultE -> eval_default_expr cursor ctx sto
  | UnE { unop; expr } -> eval_unop_expr cursor ctx sto unop expr
  | BinE { binop; expr_l; expr_r } ->
      eval_binop_expr cursor ctx sto binop expr_l expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      eval_ternop_expr cursor ctx sto expr_cond expr_then expr_else
  | CastE { typ; expr } -> eval_cast_expr cursor ctx sto typ expr
  | BitAccE { expr_base; value_lo; value_hi } ->
      eval_bitstring_acc_expr cursor ctx sto expr_base value_lo value_hi
  | ExprAccE { expr_base; member } ->
      eval_expr_acc_expr cursor ctx sto expr_base member
  | CallMethodE { expr_base; member; targs; args } ->
      eval_call_method_expr cursor ctx sto expr_base member targs args
  | CallTypeE { typ; member } -> eval_call_type_expr cursor ctx sto typ member
  | InstE { var_inst; targs; args } ->
      eval_inst_expr cursor ctx sto var_inst targs args
  | _ ->
      F.asprintf "(eval_expr) %a is not compile-time known"
        (Il.Pp.pp_expr ~level:0) expr
      |> error_info expr.at

and eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (exprs : expr list) : Sto.t * Value.t list =
  List.fold_left
    (fun (sto, values) expr ->
      let sto, value = eval_expr cursor ctx sto expr in
      (sto, values @ [ value ]))
    (sto, []) exprs

and eval_var_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (var : var)
    : Sto.t * Value.t =
  let value = Ctx.find Ctx.find_value_opt cursor var ctx in
  (sto, value)

and eval_seq_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (exprs : expr list) : Sto.t * Value.t =
  let sto, values = eval_exprs cursor ctx sto exprs in
  let value = Value.SeqV values in
  (sto, value)

and eval_seq_default_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (exprs : expr list) : Sto.t * Value.t =
  let sto, values = eval_exprs cursor ctx sto exprs in
  let value = Value.SeqDefaultV values in
  (sto, value)

and eval_record_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (fields : (member * expr) list) : Sto.t * Value.t =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let sto, values = eval_exprs cursor ctx sto exprs in
  let fields = List.combine members values in
  let value = Value.RecordV fields in
  (sto, value)

and eval_record_default_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (fields : (member * expr) list) : Sto.t * Value.t =
  let members, exprs = List.split fields in
  let members = List.map it members in
  let sto, values = eval_exprs cursor ctx sto exprs in
  let fields = List.combine members values in
  let value = Value.RecordDefaultV fields in
  (sto, value)

and eval_default_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t) (sto : Sto.t) :
    Sto.t * Value.t =
  let value = Value.DefaultV in
  (sto, value)

and eval_unop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (unop : unop) (expr : expr) : Sto.t * Value.t =
  let sto, value = eval_expr cursor ctx sto expr in
  let value = Numerics.eval_unop unop value in
  (sto, value)

and eval_binop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (binop : binop) (expr_l : expr) (expr_r : expr) : Sto.t * Value.t =
  let sto, value_l = eval_expr cursor ctx sto expr_l in
  let sto, value_r = eval_expr cursor ctx sto expr_r in
  let value = Numerics.eval_binop binop value_l value_r in
  (sto, value)

and eval_ternop_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (expr_cond : expr) (expr_then : expr) (expr_else : expr) : Sto.t * Value.t =
  let sto, value_cond = eval_expr cursor ctx sto expr_cond in
  let cond = Value.get_bool value_cond in
  let expr = if cond then expr_then else expr_else in
  let sto, value = eval_expr cursor ctx sto expr in
  (sto, value)

and eval_cast_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (typ : typ)
    (expr : expr) : Sto.t * Value.t =
  let sto, value = eval_expr cursor ctx sto expr in
  let value = Numerics.eval_cast typ.it value in
  (sto, value)

and eval_bitstring_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (expr_base : expr) (value_lo : value) (value_hi : value) : Sto.t * Value.t =
  let sto, value_base = eval_expr cursor ctx sto expr_base in
  let value =
    Numerics.eval_bitstring_access value_base value_lo.it value_hi.it
  in
  (sto, value)

and eval_expr_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (expr_base : expr) (member : member) : Sto.t * Value.t =
  let sto, value_base = eval_expr cursor ctx sto expr_base in
  let value =
    match value_base with
    | StackV (_, _, size) when member.it = "size" -> Value.IntV size
    | _ ->
        F.asprintf "(eval_expr_acc_expr) %a.%a is not compile-time known"
          (Il.Pp.pp_expr ~level:0) expr_base Il.Pp.pp_member member
        |> error_no_info
  in
  (sto, value)

and eval_call_method_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t) (sto : Sto.t)
    (expr_base : expr) (member : member) (targs : typ list) (args : arg list) :
    Sto.t * Value.t =
  let value =
    match member.it with
    | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
        assert (targs = [] && args = []);
        let typ_base = expr_base.note.typ in
        Builtins.size typ_base member.it
    | _ ->
        F.asprintf "(eval_call_method_expr) %a.%a is not compile-time known"
          (Il.Pp.pp_expr ~level:0) expr_base Il.Pp.pp_member member
        |> error_no_info
  in
  (sto, value)

and eval_call_type_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t) (sto : Sto.t)
    (typ : Il.Ast.typ) (member : Il.Ast.member) : Sto.t * Value.t =
  let value =
    match member.it with
    | "minSizeInBits" | "minSizeInBytes" | "maxSizeInBits" | "maxSizeInBytes" ->
        Builtins.size typ.it member.it
    | _ ->
        F.asprintf "(eval_call_typ_expr) %a.%a is not compile-time known"
          (Il.Pp.pp_typ ~level:0) typ Il.Pp.pp_member member
        |> error_no_info
  in
  (sto, value)

(* (TODO) What if nameless instantiation is not passed as a constructor argument?
   Then the reference value will be lost, i.e., not bound in any environment. *)
and eval_inst_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (var_inst : var) (targs : typ list) (args : arg list) : Sto.t * Value.t =
  let _, cons, args_default =
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
  let wrap_some ~at (ctx, sto, decl) = (ctx, sto, Some (decl $ at)) in
  let wrap_ctx_some ctx = (ctx, sto, Some decl) in
  let wrap_ctx_none ctx = (ctx, sto, None) in
  match decl.it with
  | ConstD { id; typ; value; annos } ->
      eval_const_decl cursor ctx id typ value annos |> wrap_ctx_some
  | VarD _ -> (ctx, sto, decl.it) |> wrap_some ~at:decl.at
  | InstD { id; typ; var_inst; targs; args; init; annos } ->
      eval_inst_decl cursor ctx sto id typ var_inst targs args init annos
      |> wrap_some ~at:decl.at
  | ValueSetD _ -> ctx |> wrap_ctx_some
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      eval_parser_decl cursor ctx id tparams params cparams locals states annos
      |> wrap_ctx_none
  | TableD { id; typ; table; annos } ->
      eval_table_decl cursor ctx sto id typ table annos |> wrap_some ~at:decl.at
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

and eval_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (_typ : typ)
    (value : value) (_annos : anno list) : Ctx.t =
  Ctx.add_value cursor id.it value.it ctx

and eval_inst_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (id : id)
    (typ : typ) (var_inst : var) (targs : typ list) (args : arg list)
    (init : decl list) (_annos : anno list) : Ctx.t * Sto.t * decl' =
  let _, cons, args_default =
    let args = FId.to_names args in
    Ctx.find_overloaded Ctx.find_cons_opt cursor var_inst args ctx
  in
  let sto, obj =
    let ctx = Ctx.enter_path id.it ctx in
    do_instantiate cursor ctx sto cons targs args args_default
  in
  let sto, obj =
    match obj with
    | Obj.ExternO (id_obj, venv_obj, fenv_obj) ->
        let ctx_init =
          { Ctx.empty with path = ctx.path @ [ id.it ]; global = ctx.global }
        in
        let ctx_init, sto, _ = eval_decls Ctx.Block ctx_init sto init in
        let venv_obj =
          VEnv.bindings ctx_init.block.venv
          |> List.fold_left
               (fun venv_obj (id, value) -> VEnv.add id value venv_obj)
               venv_obj
        in
        let fenv_obj =
          FEnv.bindings ctx_init.block.fenv
          |> List.fold_left
               (fun fenv_obj (fid, func) ->
                 let func =
                   match func with
                   | Func.FuncF (tparams, params, body) ->
                       Func.ExternMethodF (tparams, params, Some body)
                   | _ -> assert false
                 in
                 FEnv.remove fid fenv_obj |> FEnv.add fid func)
               fenv_obj
        in
        (sto, Obj.ExternO (id_obj, venv_obj, fenv_obj))
    | _ -> (sto, obj)
  in
  let oid = ctx.path @ [ id.it ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let ctx = Ctx.add_value cursor id.it value ctx in
  let decl =
    let expr_inst =
      ValueE { value = value $ no_info }
      $$ (no_info, { typ = typ.it; ctk = Ctk.CTK })
    in
    VarD { id; typ; init = Some expr_inst; annos = [] }
  in
  (ctx, sto, decl)

and eval_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (states : parser_state list) (_annos : anno list) :
    Ctx.t =
  let cid = FId.to_fid id cparams in
  let cons = Cons.ParserC (tparams, cparams, params, locals, states) in
  Ctx.add_cons cursor cid cons ctx

and eval_table_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (id : id)
    (typ : typ) (table : table) (_annos : anno list) : Ctx.t * Sto.t * decl' =
  let cons = Cons.TableC (id.it, table) in
  let sto, obj =
    let ctx = Ctx.enter_path id.it ctx in
    do_instantiate cursor ctx sto cons [] [] []
  in
  let oid = ctx.path @ [ id.it ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let ctx = Ctx.add_value cursor id.it value ctx in
  let decl =
    let expr_inst =
      ValueE { value = value $ no_info }
      $$ (no_info, { typ = typ.it; ctk = Ctk.CTK })
    in
    VarD { id; typ; init = Some expr_inst; annos = [] }
  in
  (ctx, sto, decl)

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
  let conss =
    if conss = [] then
      [ L.ExternConsM { id; cparams = []; annos = [] } $ no_info ]
    else conss
  in
  List.fold_left
    (fun ctx cons ->
      match cons.it with
      | L.ExternConsM { id = _id; cparams; annos = _annos } ->
          let cid = FId.to_fid id cparams in
          let cons = Cons.ExternC (id.it, tparams, cparams, mthds) in
          Ctx.add_cons cursor cid cons ctx
      | _ -> assert false)
    ctx conss

and eval_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (cparams : cparam list) (_annos : anno list) : Ctx.t
    =
  let cid = FId.to_fid id cparams in
  let cons = Cons.PackageC (tparams, cparams) in
  Ctx.add_cons cursor cid cons ctx

(* Parser state evaluation *)

and eval_parser_state_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (state : parser_state) : Ctx.t * Sto.t =
  assert (cursor = Ctx.Block);
  let label, block, _annos = state.it in
  let ctx, sto, block = eval_block Ctx.Local ctx sto block in
  let value_state = Value.StateV label.it in
  let state = block in
  let ctx =
    let id_state = label.it in
    Ctx.add_value cursor id_state value_state ctx
    |> Ctx.add_state cursor id_state state
  in
  (ctx, sto)

and eval_parser_state_builtin_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (label : state_label) : Ctx.t =
  let value_state = Value.StateV label.it in
  let state = ([], []) $ no_info in
  let ctx =
    let id_state = label.it in
    Ctx.add_value cursor id_state value_state ctx
    |> Ctx.add_state cursor id_state state
  in
  ctx

and eval_parser_states (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (states : parser_state list) : Ctx.t * Sto.t =
  assert (cursor = Ctx.Block);
  let ctx = eval_parser_state_builtin_decl cursor ctx ("accept" $ no_info) in
  let ctx = eval_parser_state_builtin_decl cursor ctx ("reject" $ no_info) in
  let ctx, sto =
    List.fold_left
      (fun (ctx, sto) state -> eval_parser_state_decl cursor ctx sto state)
      (ctx, sto) states
  in
  (ctx, sto)

(* Method evaluation *)

and eval_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (mthd : mthd) : Ctx.t =
  assert (cursor = Ctx.Block);
  match mthd.it with
  | ExternAbstractM { id; typ_ret; tparams; params; annos } ->
      eval_extern_abstract_mthd cursor ctx id typ_ret tparams params annos
  | ExternM { id; typ_ret; tparams; params; annos } ->
      eval_extern_mthd cursor ctx id typ_ret tparams params annos
  | _ -> assert false

and eval_mthds (cursor : Ctx.cursor) (ctx : Ctx.t) (mthds : mthd list) : Ctx.t =
  List.fold_left (fun ctx mthd -> eval_mthd cursor ctx mthd) ctx mthds

and eval_extern_abstract_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (params : param list)
    (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ExternAbstractMethodF (tparams, params) in
  Ctx.add_func_overload cursor fid func ctx

and eval_extern_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (params : param list)
    (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ExternMethodF (tparams, params, None) in
  Ctx.add_func_overload cursor fid func ctx

(* Program evaluation *)

let instantiate_program (program : program) : CEnv.t * FEnv.t * VEnv.t * Sto.t =
  Ctx.refresh ();
  let ctx, sto, _decls = eval_decls Ctx.Global Ctx.empty Sto.empty program in
  let cenv = ctx.global.cenv in
  let fenv = ctx.global.fenv in
  let venv = ctx.global.venv in
  (cenv, fenv, venv, sto)
