module F = Format
open Domain.Dom
module L = Lang.Ast
module Ctk = Il.Ctk
module Value = Runtime_value.Value
module Types = Il.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module Numerics = Runtime_static.Numerics
module Builtins = Runtime_static.Builtins
module Envs_static = Runtime_static.Envs
module FDEnv = Envs_static.FDEnv
open Il.Ast
module Table = Runtime_dynamic.Table
module Func = Runtime_dynamic.Func
module Cons = Runtime_dynamic.Cons
module Obj = Runtime_dynamic.Object
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module TDEnv = Envs_dynamic.TDEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto
open Util.Source
open Util.Error

let error_info = error_inst_info
let error_no_info = error_inst_no_info
let error_pass_info = error_inst_pass_info
let check = check_inst

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
    (cons : Cons.t) (targs : typ list) (args : arg list)
    (args_default : id' list) : Sto.t * Obj.t =
  (* Unpack constructor *)
  let cursor_callee, tparams, cparams, do_instantiate_cons =
    match cons with
    | ExternC (id, tparams, cparams, mthds) ->
        let cursor_callee = Ctx.Block in
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_extern Ctx.Block ctx_callee sto id mthds
        in
        (cursor_callee, tparams, cparams, do_instantiate_cons)
    | ParserC (tparams, cparams, params, decls, states) ->
        let cursor_callee = Ctx.Block in
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_parser cursor_callee ctx_callee sto params decls states
        in
        (cursor_callee, tparams, cparams, do_instantiate_cons)
    | ControlC (tparams, cparams, params, decls, body) ->
        let cursor_callee = Ctx.Block in
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_control cursor_callee ctx_callee sto params decls body
        in
        (cursor_callee, tparams, cparams, do_instantiate_cons)
    | PackageC (tparams, cparams) ->
        let cursor_callee = Ctx.Block in
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_package cursor_callee ctx_callee sto
        in
        (cursor_callee, tparams, cparams, do_instantiate_cons)
    | TableC (id, table) ->
        let cursor_callee = Ctx.Local in
        let do_instantiate_cons ctx_callee sto =
          do_instantiate_table Ctx.Local ctx_callee sto id table
        in
        (cursor_callee, [], [], do_instantiate_cons)
  in
  (* Initialize callee context *)
  let ctx_callee =
    match cursor_callee with
    | Global -> Ctx.empty
    | Block -> Ctx.copy Ctx.Global ctx_caller
    | Local -> Ctx.copy Ctx.Block ctx_caller
  in
  (* Bind type arguments to the callee context *)
  let ctx_callee = Ctx.add_tparams cursor_callee tparams targs ctx_callee in
  (* Bind constructor arguments to the callee context *)
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
        let ctx_callee = Ctx.add_value cursor_callee id.it value ctx_callee in
        (ctx_callee, sto))
      (ctx_callee, sto) cparams args
  in
  let ctx_callee =
    List.fold_left2
      (fun ctx_callee cparam_default value_default ->
        let id, _, _, _, _ = cparam_default.it in
        Ctx.add_value cursor_callee id.it value_default.it ctx_callee)
      ctx_callee cparams_default args_default
  in
  (* Instantiate callee *)
  let sto, obj = do_instantiate_cons ctx_callee sto in
  (sto, obj)

and do_instantiate_extern (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (id : id') (mthds : mthd list) : Sto.t * Obj.t =
  assert (cursor = Ctx.Block);
  let ctx = eval_mthds cursor ctx mthds in
  let theta = ctx.block.theta in
  let venv = ctx.block.venv in
  let fenv = ctx.block.fenv in
  let obj = Obj.ExternO (id, theta, venv, fenv) in
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
    eval_block ~start:true Ctx.Local ctx_apply_locals sto block
  in
  let venv = ctx.block.venv in
  let fenv = ctx_apply_locals.block.fenv in
  let obj = Obj.ControlO (venv, params, decls, fenv, block) in
  (sto, obj)

and do_instantiate_package (_cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) :
    Sto.t * Obj.t =
  let theta = ctx.block.theta in
  let venv = ctx.block.venv in
  let obj = Obj.PackageO (theta, venv) in
  (sto, obj)

and do_instantiate_table (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (id : id') (table : table) : Sto.t * Obj.t =
  assert (cursor = Ctx.Local);
  let ctx_table, sto, table =
    let ctx = Ctx.enter_frame ctx in
    eval_table Ctx.Local ctx sto table
  in
  let venv = List.hd ctx_table.local.venvs in
  let table = Table.init table in
  let obj = Obj.TableO (id, venv, table) in
  (sto, obj)

(* Argument evaluation *)

and eval_arg (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (arg : arg) :
    Sto.t * Value.t =
  try eval_arg' cursor ctx sto arg.it
  with InstErr _ as err -> error_pass_info arg.at err

and eval_arg' (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (arg : arg') :
    Sto.t * Value.t =
  match arg with
  | L.ExprA expr | L.NameA (_, Some expr) -> eval_expr cursor ctx sto expr
  | _ ->
      F.asprintf "(eval_arg) instantiation arguments must not be missing"
      |> error_no_info

(* Expression evaluation *)

and eval_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (expr : expr) :
    Sto.t * Value.t =
  try eval_expr' cursor ctx sto expr.it
  with InstErr _ as err -> error_pass_info expr.at err

and eval_expr' (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (expr : expr')
    : Sto.t * Value.t =
  match expr with
  | ValueE { value } -> (sto, value.it)
  | BoolE { boolean } -> (sto, Value.BoolV boolean)
  | StrE { text } -> (sto, Value.StrV text.it)
  | NumE { num } -> eval_num_expr cursor ctx sto num
  | VarE { var } -> eval_var_expr cursor ctx sto var
  | SeqE { exprs } -> eval_seq_expr cursor ctx sto exprs
  | SeqDefaultE { exprs } -> eval_seq_default_expr cursor ctx sto exprs
  | RecordE { fields } -> eval_record_expr cursor ctx sto fields
  | RecordDefaultE { fields } -> eval_record_default_expr cursor ctx sto fields
  | DefaultE -> (sto, Value.DefaultV)
  | InvalidE -> (sto, Value.InvalidV)
  | UnE { unop; expr } -> eval_unop_expr cursor ctx sto unop expr
  | BinE { binop; expr_l; expr_r } ->
      eval_binop_expr cursor ctx sto binop expr_l expr_r
  | TernE { expr_cond; expr_then; expr_else } ->
      eval_ternop_expr cursor ctx sto expr_cond expr_then expr_else
  | CastE { typ; expr } -> eval_cast_expr cursor ctx sto typ expr
  | BitAccE { expr_base; value_lo; value_hi } ->
      eval_bitstring_acc_expr cursor ctx sto expr_base value_lo value_hi
  | ErrAccE { member } -> eval_error_acc_expr cursor ctx sto member
  | TypeAccE { var_base; member } ->
      eval_type_acc_expr cursor ctx sto var_base member
  | ExprAccE { expr_base; member } ->
      eval_expr_acc_expr cursor ctx sto expr_base member
  | CallMethodE { expr_base; member; targs; args } ->
      eval_call_method_expr cursor ctx sto expr_base member targs args
  | CallTypeE { typ; member } -> eval_call_type_expr cursor ctx sto typ member
  | InstE { var_inst; targs; targs_hidden; args } ->
      eval_inst_expr cursor ctx sto var_inst (targs @ targs_hidden) args
  | _ ->
      F.asprintf "(eval_expr') %a is not compile-time known"
        (Il.Pp.pp_expr' ~level:0) expr
      |> error_no_info

and eval_exprs (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (exprs : expr list) : Sto.t * Value.t list =
  List.fold_left
    (fun (sto, values) expr ->
      let sto, value = eval_expr cursor ctx sto expr in
      (sto, values @ [ value ]))
    (sto, []) exprs

and eval_num_expr (_cursor : Ctx.cursor) (_ctx : Ctx.t) (sto : Sto.t)
    (num : Il.Ast.num) : Sto.t * Value.t =
  let value =
    match num.it with
    | value, Some (width, signed) ->
        if signed then Runtime_value.Num.int_of_raw_int value width
        else Runtime_value.Num.bit_of_raw_int value width
    | value, None -> Value.IntV value
  in
  (sto, value)

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

and eval_error_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (member : Il.Ast.member) : Sto.t * Value.t =
  let value_error = Ctx.find_value_opt cursor ("error." ^ member.it) ctx in
  let value_error = Option.get value_error in
  (sto, value_error)

and eval_type_acc_expr (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (var_base : Il.Ast.var) (member : Il.Ast.member) : Sto.t * Value.t =
  let td_base = Ctx.find_opt Ctx.find_typdef_opt cursor var_base ctx in
  let td_base = Option.get td_base in
  let value =
    let typ_base =
      match td_base with
      | MonoD typ_base -> typ_base
      | _ ->
          F.asprintf "(eval_type_acc_expr) Cannot access a generic type %a"
            (TypeDef.pp ~level:0) td_base
          |> error_no_info
    in
    match Type.canon typ_base with
    | EnumT (id, _) -> Value.EnumFieldV (id, member.it)
    | SEnumT (id, _, fields) ->
        let value_inner = List.assoc member.it fields in
        Value.SEnumFieldV (id, member.it, value_inner)
    | _ ->
        F.asprintf "(eval_type_acc_expr) %a cannot be accessed\n"
          (TypeDef.pp ~level:0) td_base
        |> error_no_info
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

(* Statement evaluation *)

and eval_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (stmt : stmt) :
    Ctx.t * Sto.t * stmt =
  try
    let ctx, sto, stmt_post = eval_stmt' cursor ctx sto stmt.it in
    (ctx, sto, stmt_post $ stmt.at)
  with InstErr _ as err -> error_pass_info stmt.at err

and eval_stmt' (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (stmt : stmt')
    : Ctx.t * Sto.t * stmt' =
  match stmt with
  | EmptyS | AssignS _ -> (ctx, sto, stmt)
  | SwitchS { expr_switch; cases } ->
      eval_switch_stmt cursor ctx sto expr_switch cases
  | IfS { expr_cond; stmt_then; stmt_else } ->
      eval_if_stmt cursor ctx sto expr_cond stmt_then stmt_else
  | BlockS { block } -> eval_block_stmt cursor ctx sto block
  | ExitS | RetS _ | CallFuncS _ | CallMethodS _ -> (ctx, sto, stmt)
  | CallInstS { typ; var_inst; targs; args } ->
      eval_call_inst_stmt cursor ctx sto typ var_inst targs args
  | TransS _ -> (ctx, sto, stmt)
  | DeclS { decl } -> eval_decl_stmt cursor ctx sto decl

and eval_stmts (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (stmts : stmt list) : Ctx.t * Sto.t * stmt list =
  List.fold_left
    (fun (ctx, sto, stmts) stmt ->
      let ctx, sto, stmt = eval_stmt cursor ctx sto stmt in
      (ctx, sto, stmts @ [ stmt ]))
    (ctx, sto, []) stmts

and eval_switch_case (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (case : switch_case) : Ctx.t * Sto.t * switch_case =
  match case.it with
  | MatchC (label, block) ->
      let ctx, sto, block = eval_block ~start:false cursor ctx sto block in
      let case = Lang.Ast.MatchC (label, block) $ case.at in
      (ctx, sto, case)
  | FallC _ -> (ctx, sto, case)

and eval_switch_cases (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (cases : switch_case list) : Ctx.t * Sto.t * switch_case list =
  List.fold_left
    (fun (ctx, sto, cases) case ->
      let ctx, sto, case = eval_switch_case cursor ctx sto case in
      (ctx, sto, cases @ [ case ]))
    (ctx, sto, []) cases

and eval_switch_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (expr_switch : expr) (cases : switch_case list) : Ctx.t * Sto.t * stmt' =
  let ctx, sto, cases = eval_switch_cases cursor ctx sto cases in
  let stmt = SwitchS { expr_switch; cases } in
  (ctx, sto, stmt)

and eval_if_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (expr_cond : expr) (stmt_then : stmt) (stmt_else : stmt) :
    Ctx.t * Sto.t * stmt' =
  let _ctx, sto, stmt_then = eval_stmt cursor ctx sto stmt_then in
  let _ctx, sto, stmt_else = eval_stmt cursor ctx sto stmt_else in
  let stmt = IfS { expr_cond; stmt_then; stmt_else } in
  (ctx, sto, stmt)

and eval_block ~(start : bool) (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (block : block) : Ctx.t * Sto.t * block =
  let stmts, annos = block.it in
  let ctx = if start then ctx else Ctx.enter_frame ctx in
  let ctx, sto, stmts = eval_stmts cursor ctx sto stmts in
  let ctx = if start then ctx else Ctx.exit_frame ctx in
  let block = (stmts, annos) $ block.at in
  (ctx, sto, block)

and eval_block_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (block : block) : Ctx.t * Sto.t * stmt' =
  let ctx, sto, block = eval_block ~start:false cursor ctx sto block in
  (ctx, sto, BlockS { block })

(* (15.1) Direct type invocation

   p.apply();

   is translated to a block statement,

   { type_of_p p_0 = ref (ctx.path @ [ "p_0" ]);
     p_0.apply(); } *)

and eval_call_inst_stmt (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (typ : typ) (var_inst : var) (targs : typ list) (args : arg list) :
    Ctx.t * Sto.t * stmt' =
  let _, cons, _ =
    Ctx.find_overloaded Ctx.find_cons_opt cursor var_inst [] ctx
  in
  let sto, obj = do_instantiate cursor ctx sto cons targs [] [] in
  let id = F.asprintf "%a_%d" Il.Pp.pp_var var_inst (Ctx.fresh ()) in
  let oid = ctx.path @ [ id ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let stmt =
    let expr_inst =
      ValueE
        {
          value =
            value
            $$ (no_info, InstE { var_inst; targs; targs_hidden = []; args });
        }
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
    (decl : decl) : Ctx.t * Sto.t * stmt' =
  let ctx, sto, decl = eval_decl cursor ctx sto decl in
  let stmt = match decl with Some decl -> DeclS { decl } | None -> EmptyS in
  (ctx, sto, stmt)

(* Declaration evaluation *)

and eval_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (decl : decl) :
    Ctx.t * Sto.t * decl option =
  try
    let ctx, sto, decl_post = eval_decl' cursor ctx sto decl.it in
    let decl_post =
      Option.map (fun decl_post -> decl_post $ decl.at) decl_post
    in
    (ctx, sto, decl_post)
  with InstErr _ as err -> error_pass_info decl.at err

and eval_decl' (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (decl : decl')
    : Ctx.t * Sto.t * decl' option =
  let wrap (ctx, sto, decl) =
    if cursor = Ctx.Global then (ctx, sto, None) else (ctx, sto, Some decl)
  in
  let wrap_some ctx = (ctx, sto, Some decl) in
  let wrap_none ctx = (ctx, sto, None) in
  match decl with
  (* Constant, variable, error, match_kind, and instance declarations *)
  | ConstD { id; typ; value; annos } ->
      eval_const_decl cursor ctx id typ value annos
      |> if cursor = Ctx.Global then wrap_none else wrap_some
  | VarD _ -> ctx |> if cursor = Ctx.Global then wrap_none else wrap_some
  | ErrD { members } -> eval_error_decl cursor ctx members |> wrap_none
  | MatchKindD { members } ->
      eval_match_kind_decl cursor ctx members |> wrap_none
  | InstD { id; typ; var_inst; targs; targs_hidden; args; init; annos } ->
      eval_inst_decl cursor ctx sto id typ var_inst (targs @ targs_hidden) args
        init annos
      |> wrap
  (* Derived type declarations *)
  | StructD { id; tparams; tparams_hidden; fields; annos } ->
      eval_struct_decl cursor ctx id tparams tparams_hidden fields annos
      |> wrap_none
  | HeaderD { id; tparams; tparams_hidden; fields; annos } ->
      eval_header_decl cursor ctx id tparams tparams_hidden fields annos
      |> wrap_none
  | UnionD { id; tparams; tparams_hidden; fields; annos } ->
      eval_union_decl cursor ctx id tparams tparams_hidden fields annos
      |> wrap_none
  | EnumD { id; members; annos } ->
      eval_enum_decl cursor ctx id members annos |> wrap_none
  | SEnumD { id; typ; fields; annos } ->
      eval_senum_decl cursor ctx id typ fields annos |> wrap_none
  | NewTypeD { id; typdef; annos } ->
      eval_newtype_decl cursor ctx id typdef annos |> wrap_none
  | TypeDefD { id; typdef; annos } ->
      eval_typedef_decl cursor ctx id typdef annos |> wrap_none
  (* Function declarations *)
  | ActionD { id; params; body; annos } ->
      eval_action_decl cursor ctx id params body annos |> wrap_none
  | FuncD { id; typ_ret; tparams; tparams_hidden; params; body } ->
      eval_func_decl cursor ctx id typ_ret tparams tparams_hidden params body
      |> wrap_none
  | ExternFuncD { id; typ_ret; tparams; tparams_hidden; params; annos } ->
      eval_extern_func_decl cursor ctx id typ_ret tparams tparams_hidden params
        annos
      |> wrap_none
  (* Object declarations *)
  (* Extern *)
  | ExternObjectD { id; tparams; mthds; annos } ->
      eval_extern_object_decl cursor ctx id tparams mthds annos |> wrap_none
  (* Parser *)
  | ValueSetD _ -> ctx |> wrap_some
  | ParserTypeD { id; tparams; tparams_hidden; params; annos } ->
      eval_parser_type_decl cursor ctx id tparams tparams_hidden params annos
      |> wrap_none
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      eval_parser_decl cursor ctx id tparams params cparams locals states annos
      |> wrap_none
  (* Control *)
  | TableD { id; typ; table; annos } ->
      eval_table_decl cursor ctx sto id typ table annos |> wrap
  | ControlTypeD { id; tparams; tparams_hidden; params; annos } ->
      eval_control_type_decl cursor ctx id tparams tparams_hidden params annos
      |> wrap_none
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      eval_control_decl cursor ctx id tparams params cparams locals body annos
      |> wrap_none
  (* Package *)
  | PackageTypeD { id; tparams; tparams_hidden; cparams; annos } ->
      eval_package_type_decl cursor ctx id tparams tparams_hidden cparams annos
      |> wrap_none

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

(* (11.1) Constants *)

and eval_const_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (_typ : typ)
    (value : value) (_annos : anno list) : Ctx.t =
  Ctx.add_value cursor id.it value.it ctx

(* (7.1.2) The error type *)

and eval_error_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (members : member list)
    : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_error_decl) error declaration must be global";
  let members = List.map it members in
  let ids = List.map (fun member -> "error." ^ member) members in
  let values = List.map (fun member -> Value.ErrV member) members in
  Ctx.add_values cursor ids values ctx

(* (7.1.3) The match kind type *)

and eval_match_kind_decl (cursor : Ctx.cursor) (ctx : Ctx.t)
    (members : member list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_match_kind_decl) match_kind declaration must be global";
  let members = List.map it members in
  let values = List.map (fun member -> Value.MatchKindV member) members in
  Ctx.add_values cursor members values ctx

(* (8.21) Constructor invocations
   (10.3.1) Instantiating objects with abstract methods

   type_of_inst id(constructor args);

   is translated to a variable declaration,

   type_of_inst id = ref (ctx.path @ [ id ]); *)

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
    | Obj.ExternO (id_obj, tenv_obj, venv_obj, fenv_obj) ->
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
        (sto, Obj.ExternO (id_obj, tenv_obj, venv_obj, fenv_obj))
    | _ -> (sto, obj)
  in
  let oid = ctx.path @ [ id.it ] in
  let sto = Sto.add oid obj sto in
  let value = Value.RefV oid in
  let ctx = Ctx.add_value cursor id.it value ctx in
  let decl =
    let expr_inst =
      ValueE
        {
          value =
            value
            $$ (no_info, InstE { var_inst; targs; targs_hidden = []; args });
        }
      $$ (no_info, { typ = typ.it; ctk = Ctk.CTK })
    in
    VarD { id; typ; init = Some expr_inst; annos = [] }
  in
  (ctx, sto, decl)

(* (7.2.5) Struct types *)

and eval_struct_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (tparams_hidden : tparam list)
    (fields : (member * typ * anno list) list) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_struct_decl) struct declarations must be global";
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let members, typs, _annoss =
      List.fold_left
        (fun (members, typs, annoss) (member, typ, annos) ->
          (members @ [ member.it ], typs @ [ typ.it ], annoss @ [ annos ]))
        ([], [], []) fields
    in
    let fields = List.combine members typs in
    let typ_struct = Types.StructT (id.it, fields) in
    Types.PolyD (tparams, tparams_hidden, typ_struct)
  in
  Ctx.add_typdef cursor id.it td ctx

(* (7.2.2) Header types *)

and eval_header_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (tparams_hidden : tparam list)
    (fields : (member * typ * anno list) list) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_header_decl) header declarations must be global";
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let members, typs, _annoss =
      List.fold_left
        (fun (members, typs, annoss) (member, typ, annos) ->
          (members @ [ member.it ], typs @ [ typ.it ], annoss @ [ annos ]))
        ([], [], []) fields
    in
    let fields = List.combine members typs in
    let typ_struct = Types.HeaderT (id.it, fields) in
    Types.PolyD (tparams, tparams_hidden, typ_struct)
  in
  Ctx.add_typdef cursor id.it td ctx

(* (7.2.4) Header unions *)

and eval_union_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (tparams_hidden : tparam list)
    (fields : (member * typ * anno list) list) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_header_decl) union declarations must be global";
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let members, typs, _annoss =
      List.fold_left
        (fun (members, typs, annoss) (member, typ, annos) ->
          (members @ [ member.it ], typs @ [ typ.it ], annoss @ [ annos ]))
        ([], [], []) fields
    in
    let fields = List.combine members typs in
    let typ_struct = Types.UnionT (id.it, fields) in
    Types.PolyD (tparams, tparams_hidden, typ_struct)
  in
  Ctx.add_typdef cursor id.it td ctx

(* (7.2.1) Enumeration types *)

and eval_enum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (members : member list) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_enum_decl) enum declarations must be global";
  let members = List.map it members in
  let ids = List.map (fun member -> id.it ^ "." ^ member) members in
  let values =
    List.map (fun member -> Value.EnumFieldV (id.it, member)) members
  in
  let ctx = Ctx.add_values cursor ids values ctx in
  let td =
    let typ_enum = Types.EnumT (id.it, members) in
    Types.MonoD typ_enum
  in
  Ctx.add_typdef cursor id.it td ctx

(* (7.2.1) Enumeration types *)

and eval_senum_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id) (typ : typ)
    (fields : (member * value) list) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_senum_decl) serializable enum declarations must be global";
  let members, values = List.split fields in
  let members = List.map it members in
  let values = List.map it values in
  let ids = List.map (fun member -> id.it ^ "." ^ member) members in
  let values =
    List.map2
      (fun member value -> Value.SEnumFieldV (id.it, member, value))
      members values
  in
  let ctx = Ctx.add_values cursor ids values ctx in
  let td =
    let fields = List.combine members values in
    let typ_senum = Types.SEnumT (id.it, typ.it, fields) in
    Types.MonoD typ_senum
  in
  Ctx.add_typdef cursor id.it td ctx

(* (7.6) Introducing new types *)

and eval_newtype_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (typdef : (typ, decl) Lang.Ast.alt) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_newtype_decl) new type declarations must be global";
  let typ =
    match typdef with
    | Left typ -> typ.it
    | Right decl -> (
        let ctx', _, _ = eval_decl cursor ctx Sto.empty decl in
        let tid_newtype =
          TIdSet.diff
            (TDEnv.keys ctx'.global.tdenv |> TIdSet.of_list)
            (TDEnv.keys ctx.global.tdenv |> TIdSet.of_list)
        in
        assert (TIdSet.cardinal tid_newtype = 1);
        let tid_newtype = TIdSet.choose tid_newtype in
        let td_newtype = Ctx.find_typdef cursor tid_newtype ctx' in
        match td_newtype with
        | Types.MonoD typ -> typ
        | Types.PolyD td_poly -> Types.SpecT (td_poly, []))
  in
  let td =
    let typ_new = Types.NewT (id.it, typ) in
    Types.MonoD typ_new
  in
  Ctx.add_typdef cursor id.it td ctx

(* (7.5) typedef *)

and eval_typedef_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (typdef : (typ, decl) Lang.Ast.alt) (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_typedef_decl) typedef declarations must be global";
  let typ =
    match typdef with
    | Left typ -> typ.it
    | Right decl -> (
        let ctx', _, _ = eval_decl cursor ctx Sto.empty decl in
        let tid_newtype =
          TIdSet.diff
            (TDEnv.keys ctx'.global.tdenv |> TIdSet.of_list)
            (TDEnv.keys ctx.global.tdenv |> TIdSet.of_list)
        in
        assert (TIdSet.cardinal tid_newtype = 1);
        let tid_newtype = TIdSet.choose tid_newtype in
        let td_newtype = Ctx.find_typdef cursor tid_newtype ctx' in
        match td_newtype with
        | Types.MonoD typ -> typ
        | Types.PolyD td_poly -> Types.SpecT (td_poly, []))
  in
  let td =
    let typ_def = Types.DefT (typ, id.it) in
    Types.MonoD typ_def
  in
  Ctx.add_typdef cursor id.it td ctx

(* (14.1) Actions *)

and eval_action_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (params : param list) (body : block) (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ActionF (params, body) in
  Ctx.add_func_non_overload cursor fid func ctx

(* (10) Function declarations *)

and eval_func_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (tparams_hidden : tparam list)
    (params : param list) (body : block) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.FuncF (tparams @ tparams_hidden, params, body) in
  Ctx.add_func_overload cursor fid func ctx

(* (7.2.10.1) Extern functions *)

and eval_extern_func_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (tparams_hidden : tparam list)
    (params : param list) (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ExternFuncF (tparams @ tparams_hidden, params) in
  Ctx.add_func_overload cursor fid func ctx

(* (7.2.10.2) Extern objects *)

and eval_extern_object_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (mthds : mthd list) (_annos : anno list) : Ctx.t =
  let conss, mthds =
    List.partition
      (fun mthd -> match mthd.it with ExternConsM _ -> true | _ -> false)
      mthds
  in
  let td =
    let fdenv =
      List.fold_left
        (fun fdenv mthd ->
          match mthd.it with
          | ExternAbstractM { id; typ_ret; tparams; tparams_hidden; params; _ }
            ->
              let fid = FId.to_fid id params in
              let tparams = List.map it tparams in
              let tparams_hidden = List.map it tparams_hidden in
              let params = List.map it params in
              let ft = Types.ExternAbstractMethodT (params, typ_ret.it) in
              let fd = Types.PolyFD (tparams, tparams_hidden, ft) in
              FDEnv.add fid fd fdenv
          | ExternM { id; typ_ret; tparams; tparams_hidden; params; _ } ->
              let fid = FId.to_fid id params in
              let tparams = List.map it tparams in
              let tparams_hidden = List.map it tparams_hidden in
              let params = List.map it params in
              let ft = Types.ExternMethodT (params, typ_ret.it) in
              let fd = Types.PolyFD (tparams, tparams_hidden, ft) in
              FDEnv.add fid fd fdenv
          | _ -> assert false)
        FEnv.empty mthds
    in
    let tparams = List.map it tparams in
    let typ_extern = Types.ExternT (id.it, fdenv) in
    Types.PolyD (tparams, [], typ_extern)
  in
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let conss =
    if conss = [] then
      [
        ExternConsM { id; cparams = []; tparams_hidden = []; annos = [] }
        $ no_info;
      ]
    else conss
  in
  List.fold_left
    (fun ctx cons ->
      match cons.it with
      | ExternConsM { cparams; tparams_hidden; _ } ->
          let cid = FId.to_fid id cparams in
          let cons =
            Cons.ExternC (id.it, tparams @ tparams_hidden, cparams, mthds)
          in
          Ctx.add_cons cursor cid cons ctx
      | _ -> assert false)
    ctx conss

(* (7.2.12) Parser and control blocks types *)

and eval_parser_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (tparams_hidden : tparam list) (params : param list)
    (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_parser_type_decl) parser type declarations must be global";
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let params = List.map it params in
    (* unsure if the id here is id *)
    let typ_parser = Types.ParserT (id.it, params) in
    Types.PolyD (tparams, tparams_hidden, typ_parser)
  in
  Ctx.add_typdef cursor id.it td ctx

(* (13.2) Parser declarations *)

and eval_parser_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (states : parser_state list) (_annos : anno list) :
    Ctx.t =
  let cid = FId.to_fid id cparams in
  let cons = Cons.ParserC (tparams, cparams, params, locals, states) in
  Ctx.add_cons cursor cid cons ctx

(* (14.2) Tables

   table type_of_table id { ... }

   is translated to a variable declaration,

   type_of_table id = ref (ctx.path @ [ id ]); *)

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
      (* unused note *)
      ValueE
        {
          value =
            value
            $$ ( no_info,
                 InstE
                   {
                     var_inst = L.Top id $ no_info;
                     targs = [];
                     targs_hidden = [];
                     args = [];
                   } );
        }
      $$ (no_info, { typ = typ.it; ctk = Ctk.CTK })
    in
    VarD { id; typ; init = Some expr_inst; annos = [] }
  in
  (ctx, sto, decl)

(* (7.2.12.2) Control type declarations *)

and eval_control_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (tparams_hidden : tparam list) (params : param list)
    (_annos : anno list) : Ctx.t =
  check (cursor = Ctx.Global)
    "(eval_control_type_decl) control type declarations must be global";
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let params = List.map it params in
    let typ_parser = Types.ControlT (id.it, params) in
    Types.PolyD (tparams, tparams_hidden, typ_parser)
  in
  Ctx.add_typdef cursor id.it td ctx

(* (14) Control blocks *)

and eval_control_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (params : param list) (cparams : cparam list)
    (locals : decl list) (body : block) (_annos : anno list) : Ctx.t =
  let cid = FId.to_fid id cparams in
  let cons = Cons.ControlC (tparams, cparams, params, locals, body) in
  Ctx.add_cons cursor cid cons ctx

(* (7.2.13) Package types *)

and eval_package_type_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (tparams : tparam list) (tparams_hidden : tparam list)
    (cparams : cparam list) (_annos : anno list) : Ctx.t =
  let td =
    let tparams = List.map it tparams in
    let tparams_hidden = List.map it tparams_hidden in
    let typ_package =
      let typs_inner =
        List.map it cparams |> List.map (fun (_, _, typ, _, _) -> typ.it)
      in
      Types.PackageT (id.it, typs_inner)
    in
    Types.PolyD (tparams, tparams_hidden, typ_package)
  in
  let ctx = Ctx.add_typdef cursor id.it td ctx in
  let cid = FId.to_fid id cparams in
  let cons = Cons.PackageC (tparams @ tparams_hidden, cparams) in
  Ctx.add_cons cursor cid cons ctx

(* Parser state evaluation *)

and eval_parser_state_decl (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (state : parser_state) : Ctx.t * Sto.t =
  assert (cursor = Ctx.Block);
  let label, block, _annos = state.it in
  let ctx, sto, block = eval_block ~start:true Ctx.Local ctx sto block in
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

(* Table evaluation *)

and eval_table_custom (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (table_custom : table_custom) : Ctx.t * Sto.t * table_custom =
  let table_custom_const, member, expr, annos = table_custom.it in
  let sto, value =
    let ctx = Ctx.enter_path member.it ctx in
    eval_expr cursor ctx sto expr
  in
  let expr =
    Il.Ast.ValueE { value = value $$ (no_info, expr.it) }
    $$ (no_info, { typ = expr.note.typ; ctk = expr.note.ctk })
  in
  let table_custom =
    (table_custom_const, member, expr, annos) $ table_custom.at
  in
  (ctx, sto, table_custom)

and eval_table_property (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (table_property : table_property) : Ctx.t * Sto.t * table_property =
  match table_property with
  | CustomP table_custom ->
      let ctx, sto, table_custom =
        eval_table_custom cursor ctx sto table_custom
      in
      (ctx, sto, Lang.Ast.CustomP table_custom)
  | _ -> (ctx, sto, table_property)

and eval_table_properties (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t)
    (table_properties : table_property list) :
    Ctx.t * Sto.t * table_property list =
  List.fold_left
    (fun (ctx, sto, table_properties) table_property ->
      let ctx, sto, table_property =
        eval_table_property cursor ctx sto table_property
      in
      (ctx, sto, table_properties @ [ table_property ]))
    (ctx, sto, []) table_properties

and eval_table (cursor : Ctx.cursor) (ctx : Ctx.t) (sto : Sto.t) (table : table)
    : Ctx.t * Sto.t * table =
  assert (cursor = Ctx.Local);
  eval_table_properties cursor ctx sto table

(* Method evaluation *)

and eval_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (mthd : mthd) : Ctx.t =
  assert (cursor = Ctx.Block);
  match mthd.it with
  | ExternAbstractM { id; typ_ret; tparams; tparams_hidden; params; annos } ->
      eval_extern_abstract_mthd cursor ctx id typ_ret tparams tparams_hidden
        params annos
  | ExternM { id; typ_ret; tparams; tparams_hidden; params; annos } ->
      eval_extern_mthd cursor ctx id typ_ret tparams tparams_hidden params annos
  | _ ->
      F.asprintf
        "(eval_mthd) %a is a constructor and should have been handled prior to \
         instantiation"
        (Il.Pp.pp_mthd ~level:0) mthd
      |> error_info mthd.at

and eval_mthds (cursor : Ctx.cursor) (ctx : Ctx.t) (mthds : mthd list) : Ctx.t =
  List.fold_left (fun ctx mthd -> eval_mthd cursor ctx mthd) ctx mthds

and eval_extern_abstract_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (tparams_hidden : tparam list)
    (params : param list) (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ExternAbstractMethodF (tparams @ tparams_hidden, params) in
  Ctx.add_func_overload cursor fid func ctx

and eval_extern_mthd (cursor : Ctx.cursor) (ctx : Ctx.t) (id : id)
    (_typ_ret : typ) (tparams : tparam list) (tparams_hidden : tparam list)
    (params : param list) (_annos : anno list) : Ctx.t =
  let fid = FId.to_fid id params in
  let func = Func.ExternMethodF (tparams @ tparams_hidden, params, None) in
  Ctx.add_func_overload cursor fid func ctx

(* Program evaluation:
   Instantiates the program, creating a global environment and a store. *)

let instantiate_program (program : program) :
    CEnv.t * TDEnv.t * FEnv.t * VEnv.t * Sto.t =
  Ctx.refresh ();
  let decls = program in
  let ctx, sto, decls = eval_decls Ctx.Global Ctx.empty Sto.empty decls in
  check (decls = [])
    "(instantiate_program) some declarations were not handled by instantiation";
  let cenv = ctx.global.cenv in
  let tdenv = ctx.global.tdenv in
  let fenv = ctx.global.fenv in
  let venv = ctx.global.venv in
  (cenv, tdenv, fenv, venv, sto)
