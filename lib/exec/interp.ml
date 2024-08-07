open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Context
open Runtime.Signal
open Runtime.Ops
open Util.Source
open Driver

module Make (Arch : ARCH) : INTERP = struct
  (* Global store *)

  let sto = ref Sto.empty
  let init _sto = sto := _sto

  (* Interpreter for type simplification,
     Assume: evaluation of bit width should never change the context *)

  let rec interp_type (ctx : Ctx.t) (typ : typ) : Type.t =
    match typ.it with
    | BoolT -> BoolT
    | AIntT -> AIntT
    | IntT width ->
        let width = interp_expr ctx width |> snd |> Value.get_num in
        IntT width
    | BitT width ->
        let width = interp_expr ctx width |> snd |> Value.get_num in
        BitT width
    | VBitT width ->
        let width = interp_expr ctx width |> snd |> Value.get_num in
        VBitT width
    | StrT -> StrT
    | ErrT -> Ctx.find_td_glob "error" ctx
    | NameT { it = Top id; _ } -> Ctx.find_td_glob id.it ctx
    | NameT { it = Bare id; _ } -> Ctx.find_td id.it ctx
    (* (TODO) Handle specialized types *)
    | SpecT (var, _) -> interp_type ctx (NameT var $ no_info)
    | StackT (typ, size) ->
        let typ = interp_type ctx typ in
        let size = interp_expr ctx size |> snd |> Value.get_num in
        StackT (typ, size)
    | TupleT typs ->
        let typs = List.map (interp_type ctx) typs in
        TupleT typs
    | _ ->
        Format.asprintf "(TODO: eval_type) %a" Syntax.Pp.pp_type typ |> failwith

  (* Interpreter for expressions *)

  and interp_expr (ctx : Ctx.t) (expr : expr) : Ctx.t * Value.t =
    match expr.it with
    | BoolE b -> interp_bool ctx b
    | StrE s -> interp_str ctx s
    | NumE { it = value, encoding; _ } -> interp_num ctx value encoding
    | VarE var -> interp_var ctx var
    | ListE exprs -> interp_list ctx exprs
    | RecordE fields -> interp_record ctx fields
    | UnE (unop, expr) -> interp_unop ctx unop expr
    | BinE (binop, expr_fst, expr_snd) ->
        interp_binop ctx binop expr_fst expr_snd
    | TernE (cond, expr_tru, expr_fls) ->
        interp_ternop ctx cond expr_tru expr_fls
    | CastE (typ, expr) -> interp_cast ctx typ expr
    | MaskE _ -> interp_mask ctx
    | RangeE _ -> interp_range ctx
    | ArrAccE (base, idx) -> interp_arr_acc ctx base idx
    | BitAccE (base, idx_lo, idx_hi) ->
        interp_bitstring_acc ctx base idx_lo idx_hi
    | TypeAccE (var, member) -> interp_type_acc ctx var member
    | ErrAccE member -> interp_error_acc ctx member
    | ExprAccE (base, member) -> interp_expr_acc ctx base member
    | CallE (func, targs, args) -> interp_call_as_expr ctx func targs args
    | InstE _ ->
        Format.asprintf
          "(interp_expr) Instantiation expression should have been evaluated \
           in instantiation."
        |> failwith

  and interp_bool (ctx : Ctx.t) (b : bool) : Ctx.t * Value.t = (ctx, BoolV b)
  and interp_str (ctx : Ctx.t) (s : string) : Ctx.t * Value.t = (ctx, StrV s)

  and interp_num (ctx : Ctx.t) (value : Bigint.t)
      (encoding : (Bigint.t * bool) option) : Ctx.t * Value.t =
    let value =
      match encoding with
      | Some (width, signed) ->
          if signed then Value.IntV (width, value) else Value.BitV (width, value)
      | None -> AIntV value
    in
    (ctx, value)

  and interp_var (ctx : Ctx.t) (var : var) : Ctx.t * Value.t =
    match var.it with
    | Top id ->
        let value = Ctx.find_var_glob id.it ctx in
        (ctx, value)
    | Bare id ->
        let value = Ctx.find_var id.it ctx in
        (ctx, value)

  and interp_list (ctx : Ctx.t) (exprs : expr list) : Ctx.t * Value.t =
    let ctx, values = interp_exprs ctx exprs in
    let value = Value.TupleV values in
    (ctx, value)

  and interp_record (ctx : Ctx.t) (fields : (member * expr) list) :
      Ctx.t * Value.t =
    let fields, exprs = List.split fields in
    let ctx, values = exprs |> interp_exprs ctx in
    let fields =
      List.map2 (fun field value -> (field.it, value)) fields values
    in
    let value = Value.StructV fields in
    (ctx, value)

  and interp_unop (ctx : Ctx.t) (op : unop) (expr : expr) : Ctx.t * Value.t =
    let ctx, value = interp_expr ctx expr in
    let value = Runtime.Ops.eval_unop op value in
    (ctx, value)

  and interp_binop (ctx : Ctx.t) (op : binop) (expr_fst : expr)
      (expr_snd : expr) : Ctx.t * Value.t =
    let ctx, values = interp_exprs ctx [ expr_fst; expr_snd ] in
    let value_fst, value_snd = (List.nth values 0, List.nth values 1) in
    let value = Runtime.Ops.eval_binop op value_fst value_snd in
    (ctx, value)

  and interp_ternop (ctx : Ctx.t) (expr_cond : expr) (expr_tru : expr)
      (expr_fls : expr) : Ctx.t * Value.t =
    let ctx, value_cond = interp_expr ctx expr_cond in
    let cond = Value.get_bool value_cond in
    let expr = if cond then expr_tru else expr_fls in
    interp_expr ctx expr

  and interp_cast (ctx : Ctx.t) (typ : typ) (expr : expr) : Ctx.t * Value.t =
    let typ =
      let typ = interp_type ctx typ in
      Ctx.simplify_td typ ctx
    in
    let ctx, value = interp_expr ctx expr in
    let value = Runtime.Ops.eval_cast typ value in
    (ctx, value)

  and interp_mask (_ctx : Ctx.t) : Ctx.t * Value.t =
    Format.sprintf "(TODO: interp_mask)" |> failwith

  and interp_range (_ctx : Ctx.t) : Ctx.t * Value.t =
    Format.sprintf "(TODO: interp_range)" |> failwith

  and interp_arr_acc (ctx : Ctx.t) (base : expr) (idx : expr) : Ctx.t * Value.t
      =
    let ctx, values = interp_exprs ctx [ base; idx ] in
    let value_base, value_idx = (List.nth values 0, List.nth values 1) in
    match value_base with
    (* (TODO) Insert bounds checking *)
    | StackV (values, _, _) ->
        let idx = Value.get_num value_idx |> Bigint.to_int |> Option.get in
        let value = List.nth values idx in
        (ctx, value)
    | _ ->
        Format.asprintf "(interp_arr_acc) %a is not a header stack." Value.pp
          value_base
        |> failwith

  and interp_bitstring_acc (ctx : Ctx.t) (base : expr) (idx_hi : expr)
      (idx_lo : expr) : Ctx.t * Value.t =
    let ctx, values = interp_exprs ctx [ base; idx_hi; idx_lo ] in
    let value_base, value_hi, value_lo =
      (List.nth values 0, List.nth values 1, List.nth values 2)
    in
    let value =
      Runtime.Ops.eval_bitstring_access value_base value_hi value_lo
    in
    (ctx, value)

  and interp_type_acc (ctx : Ctx.t) (var : var) (member : member) :
      Ctx.t * Value.t =
    let typ =
      match var.it with
      | Top id -> Ctx.find_td_glob id.it ctx
      | Bare id -> Ctx.find_td id.it ctx
    in
    match typ with
    | EnumT (id, members) when List.mem member.it members ->
        (ctx, EnumFieldV (id, member.it))
    | _ ->
        Format.asprintf "(TODO: interp_type_acc) Cannot access member %s of %a"
          member.it Type.pp typ
        |> failwith

  and interp_error_acc (ctx : Ctx.t) (member : member) : Ctx.t * Value.t =
    let id = "error." ^ member.it in
    match Ctx.find_var_glob_opt id ctx with
    | Some (ErrV _ as value) -> (ctx, value)
    | _ ->
        Format.asprintf "(interp_error_acc) Cannot access member %s of error"
          member.it
        |> failwith

  and interp_builtin_stack_acc (ctx : Ctx.t) (values : Value.t list)
      (next : Bigint.t) (_size : Bigint.t) (member : member) =
    match member.it with
    (* hs.next: produces a reference to the element with index hs.nextIndex in the stack.
       May only be used in a parser. If the stack's nextIndex counter is greater than or equal to size,
       then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
       If hs is an l-value, then hs.next is also an l-value. *)
    | "next" ->
        let idx = Bigint.to_int next |> Option.get in
        let value = List.nth values idx in
        (ctx, value)
    (* hs.last: produces a reference to the element with index hs.nextIndex - 1 in the stack, if such an element exists.
       May only be used in a parser. If the nextIndex counter is less than 1, or greater than size,
       then evaluating this expression results in a transition to reject and sets the error to error.StackOutOfBounds.
       Unlike hs.next, the resulting reference is never an l-value. *)
    | "last" ->
        let idx = (Bigint.to_int next |> Option.get) - 1 in
        let value = List.nth values idx in
        (ctx, value)
    | _ ->
        Format.asprintf
          "(interp_builtin_stack_acc) %s member access not supported for \
           header stack\n"
          member.it
        |> failwith

  and interp_expr_acc (ctx : Ctx.t) (base : expr) (member : member) :
      Ctx.t * Value.t =
    let ctx, value_base = interp_expr ctx base in
    match value_base with
    | HeaderV (_, fields) | StructV fields ->
        let value = List.assoc member.it fields in
        (ctx, value)
    | RefV path ->
        let value = Value.RefV (path @ [ member.it ]) in
        (ctx, value)
    | StackV (values, next, size) ->
        interp_builtin_stack_acc ctx values next size member
    | _ ->
        Format.asprintf "(TODO: interp_expr_acc) %a.%s\n" Value.pp value_base
          member.it
        |> failwith

  and interp_call_as_expr (ctx : Ctx.t) (func : expr) (targs : typ list)
      (args : arg list) : Ctx.t * Value.t =
    let sign, ctx = interp_call ctx func targs args in
    let value =
      match (sign : Sig.t) with
      | Ret value -> Option.get value
      | _ ->
          Format.asprintf
            "(interp_call_as_expr) No return signal from function.\n"
          |> failwith
    in
    (ctx, value)

  and interp_exprs (ctx : Ctx.t) (exprs : expr list) : Ctx.t * Value.t list =
    List.fold_left
      (fun (ctx, values) expr ->
        let ctx, value = interp_expr ctx expr in
        (ctx, values @ [ value ]))
      (ctx, []) exprs

  (* Interpreter for statements *)

  (* lvalue
     : prefixedNonTypeName | lvalue "." member
     | lvalue "[" expression "]" | lvalue "[" expression ":" expression "]" *)
  and interp_write (ctx : Ctx.t) (lvalue : expr) (value : Value.t) =
    match lvalue.it with
    | VarE { it = Bare id; _ } ->
        (* let typ = Ctx.find_var id.it ctx |> fst in *)
        (* (TODO) casts must be explicitized after type checking *)
        (* let value = Runtime.Ops.eval_cast typ value in *)
        Ctx.update_var id.it value ctx
    | ArrAccE (base, idx) -> (
        let ctx, value_base = interp_expr ctx base in
        let ctx, value_idx = interp_expr ctx idx in
        match value_base with
        | StackV (values, next, size) ->
            let idx = Value.get_num value_idx |> Bigint.to_int |> Option.get in
            let values =
              List.mapi
                (fun idx' value' -> if idx' = idx then value else value')
                values
            in
            interp_write ctx base (StackV (values, next, size))
        | _ ->
            Format.asprintf "(interp_write) %a is not a header stack."
              Syntax.Pp.pp_expr base
            |> failwith)
    | ExprAccE (base, member) -> (
        let ctx, value_base = interp_expr ctx base in
        let update_field fields member =
          List.map
            (fun (m, v) -> if m = member.it then (m, value) else (m, v))
            fields
        in
        match value_base with
        | StructV fields ->
            let fields = update_field fields member in
            interp_write ctx base (StructV fields)
        | HeaderV (valid, fields) ->
            let fields = update_field fields member in
            interp_write ctx base (HeaderV (valid, fields))
        | StackV (values, next, size) when member.it = "next" ->
            let idx = Bigint.to_int next |> Option.get in
            let values =
              List.mapi (fun i v -> if i = idx then value else v) values
            in
            (* (TODO) In order to enforce,
               "It is automatically advanced on each successful call to extract. (13.9)"
               Yet doesn't seem to be in the right place *)
            let next = Bigint.(next + one) in
            interp_write ctx base (StackV (values, next, size))
        | _ ->
            Format.asprintf "(TODO: interp_write) %a" Syntax.Pp.pp_expr lvalue
            |> failwith)
    | _ ->
        Format.asprintf "(TODO: interp_write) %a" Syntax.Pp.pp_expr lvalue
        |> failwith

  and interp_stmt (sign : Sig.t) (ctx : Ctx.t) (stmt : stmt) =
    match stmt.it with
    | EmptyI -> interp_empty sign ctx
    | AssignI (lhs, rhs) -> interp_assign sign ctx lhs rhs
    | IfI (cond, tru, fls) -> interp_if sign ctx cond tru fls
    | BlockI block -> interp_block sign ctx block
    | CallI (func, targs, args) -> interp_call_as_stmt sign ctx func targs args
    | TransI next -> interp_trans sign ctx next
    | SelectI (exprs, cases) -> interp_select sign ctx exprs cases
    | DeclI decl -> interp_decl sign ctx decl
    | SwitchI (expr, cases) -> interp_switch sign ctx expr cases
    | ExitI -> interp_exit sign ctx
    | RetI expr -> interp_return sign ctx expr

  and interp_empty (sign : Sig.t) (ctx : Ctx.t) = (sign, ctx)

  and interp_assign (sign : Sig.t) (ctx : Ctx.t) (lhs : expr) (rhs : expr) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let ctx, value = interp_expr ctx rhs in
        let ctx = interp_write ctx lhs value in
        (sign, ctx)

  and interp_if (sign : Sig.t) (ctx : Ctx.t) (cond : expr) (tru : stmt)
      (fls : stmt) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let ctx, cond = interp_expr ctx cond in
        let cond = Runtime.Ops.eval_cast Type.BoolT cond in
        let cond = match cond with BoolV b -> b | _ -> assert false in
        let branch = if cond then tru else fls in
        interp_stmt sign ctx branch

  and interp_block (sign : Sig.t) (ctx : Ctx.t) (block : block) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let ctx = Ctx.enter_frame ctx in
        let sign, ctx =
          List.fold_left
            (fun (sign, ctx) stmt -> interp_stmt sign ctx stmt)
            (sign, ctx) block.it
        in
        let ctx = Ctx.exit_frame ctx in
        (sign, ctx)

  and interp_call_as_stmt (sign : Sig.t) (ctx : Ctx.t) (func : expr)
      (targs : typ list) (args : arg list) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let sign, ctx = interp_call ctx func targs args in
        let sign = match sign with Ret _ -> Sig.Cont | _ -> sign in
        (sign, ctx)

  (* (TODO) For state transitions, do not change object visibility,
     treating them as real "transitions", because states can be mutually recursive *)
  (* exit statements are not allowed within parsers or functions. (12.5) *)
  and interp_trans (sign : Sig.t) (ctx : Ctx.t) (next : label) =
    match sign with
    | Ret _ | Exit ->
        Format.asprintf "(interp_trans) Exit unallowed within parser.\n"
        |> failwith
    | Cont ->
        (* (TODO) better handling of accept/reject *)
        if next.it = "accept" || next.it = "reject" then (sign, ctx)
        else
          let state_next = Ctx.find_func (next.it, []) ctx in
          let body =
            match state_next with StateF { body } -> body | _ -> assert false
          in
          let ctx_next =
            let env_loc = (TDEnv.empty, []) in
            { ctx with env_loc }
          in
          let sign, ctx_next = interp_block sign ctx_next body in
          assert (sign = Cont);
          let ctx = { ctx with env_obj = ctx_next.env_obj } in
          (sign, ctx)

  (* exit statements are not allowed within parsers or functions. (12.5) *)
  (* assume: evaluation of match case should never change the context *)
  and interp_select (sign : Sig.t) (ctx : Ctx.t) (exprs : expr list)
      (cases : select_case list) =
    match sign with
    | Ret _ | Exit ->
        Format.asprintf "(interp_select) Exit unallowed within parser.\n"
        |> failwith
    | Cont ->
        (* (TODO) how to properly cast the select-ed value against the case value(s)? *)
        let ctx, values =
          List.fold_left
            (fun (ctx, values) expr ->
              let ctx, value = interp_expr ctx expr in
              (ctx, value :: values))
            (ctx, []) exprs
        in
        let select_cases (next_found : label option) (case : select_case) =
          match next_found with
          | Some _ -> next_found
          | None ->
              let mtchs, next = case.it in
              let select_mtch (mtch : mtch) (value : Value.t) =
                match mtch.it with
                | DefaultM | AnyM -> true
                | ExprM expr -> interp_expr ctx expr |> snd = value
              in
              if List.for_all2 select_mtch mtchs values then Some next else None
        in
        let next = List.fold_left select_cases None cases |> Option.get in
        interp_trans sign ctx next

  and interp_decl (sign : Sig.t) (ctx : Ctx.t) (decl : decl) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont -> (
        match decl.it with
        | VarD { id; typ; init = None } ->
            let typ = interp_type ctx typ in
            let value = Runtime.Ops.eval_default_value typ in
            let ctx = Ctx.add_var_loc id.it value ctx in
            (sign, ctx)
        | VarD { id; typ; init = Some value } ->
            let typ = interp_type ctx typ in
            let ctx, value = interp_expr ctx value in
            let value = Runtime.Ops.eval_cast typ value in
            let ctx = Ctx.add_var_loc id.it value ctx in
            (sign, ctx)
        | _ ->
            Format.asprintf "(TODO: interp_decl) %a" Syntax.Pp.pp_decl (0, decl)
            |> failwith)

  (* (TODO) assume switch matches on table apply result only,
     since case with expression is not supported in Petr4 parser *)
  and interp_switch (sign : Sig.t) (ctx : Ctx.t) (expr : expr)
      (cases : switch_case list) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let ctx, value = interp_expr ctx expr in
        let value = Value.get_enum value |> snd in
        let switch_cases (block_found : bool * block option)
            (case : switch_case) =
          let case, block = case.it in
          match block_found with
          (* match complete *)
          | true, Some _ -> block_found
          (* during fallthrough *)
          | true, None -> (
              match case.it with
              | CaseC _ -> (true, Some block)
              | FallC _ -> (true, None)
              | DefaultC -> (true, Some block))
          (* match not found *)
          | false, _ -> (
              match case.it with
              | CaseC case when case = value -> (true, Some block)
              | FallC case when case = value -> (true, None)
              | DefaultC -> (true, Some block)
              | _ -> block_found)
        in
        let _, block = List.fold_left switch_cases (false, None) cases in
        let block =
          match block with None -> [] $ no_info | Some block -> block
        in
        interp_block sign ctx block

  and interp_exit (sign : Sig.t) (ctx : Ctx.t) =
    match sign with Ret _ | Exit -> (sign, ctx) | Cont -> (Exit, ctx)

  and interp_return (sign : Sig.t) (ctx : Ctx.t) (expr : expr option) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let ctx, value =
          match expr with
          | Some expr ->
              let ctx, value = interp_expr ctx expr in
              (ctx, Some value)
          | None -> (ctx, None)
        in
        (Ret value, ctx)

  (* Call semantics *)

  (* align parameters by argument order *)
  and align_params_with_args (params : param list) (args : arg list) =
    (* (TODO) assume there is no default argument *)
    assert (List.length params = List.length args);
    Instance.Instantiate.check_args args;
    let module PMap = Map.Make (String) in
    let params_map =
      List.fold_left
        (fun params_map param ->
          let id, _, _, _ = param.it in
          PMap.add id.it param params_map)
        PMap.empty params
    in
    List.fold_left2
      (fun (params, args) param arg ->
        match arg.it with
        | ExprA arg -> (params @ [ param ], args @ [ arg ])
        | NameA (id, arg) ->
            let param = PMap.find id.it params_map in
            (params @ [ param ], args @ [ arg ])
        | _ ->
            Format.asprintf "(TODO: align_params_with_args) %a" Syntax.Pp.pp_arg
              arg
            |> failwith)
      ([], []) params args

  (* adder determines where to add the argument, either object or local scope *)
  and copyin adder (ctx_callee : Ctx.t) (params : param list)
      (values : Value.t list) =
    let copyin' (ctx_callee : Ctx.t) (param : param) (value : Value.t) =
      let id, dir, typ, _ = param.it in
      (* (TODO) Is it correct to evaluate the type at callee? *)
      match dir.it with
      | No | In | InOut ->
          (* let typ = interp_type ctx_callee typ in *)
          (* let value = Runtime.Ops.eval_cast typ value in *)
          adder id.it value ctx_callee
      | Out ->
          let typ = interp_type ctx_callee typ in
          let value = Runtime.Ops.eval_default_value typ in
          adder id.it value ctx_callee
    in
    List.fold_left2 copyin' ctx_callee params values

  and copyout (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (params : param list)
      (exprs : expr list) =
    let copyout' (ctx_caller : Ctx.t) (param : param) (expr : expr) =
      let id, dir, _, _ = param.it in
      match dir.it with
      | InOut | Out ->
          let value = Ctx.find_var id.it ctx_callee in
          interp_write ctx_caller expr value
      | _ -> ctx_caller
    in
    List.fold_left2 copyout' ctx_caller params exprs

  (* adder determines where to add the type argument, either object or local scope *)
  and interp_targs adder (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (tparams : tparam list) (targs : typ list) =
    assert (List.length tparams = List.length targs);
    List.fold_left2
      (fun ctx_callee tparam targ ->
        let targ = interp_type ctx_caller targ in
        adder tparam.it targ ctx_callee)
      ctx_callee tparams targs

  and interp_args (ctx_caller : Ctx.t) (exprs : expr list) =
    interp_exprs ctx_caller exprs

  and interp_extern_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (tparams : tparam list) (params : param list) (targs : typ list)
      (args : arg list) =
    (* Align the parameters with the arguments *)
    let params, args = align_params_with_args params args in
    (* Evaluate the arguments *)
    let ctx_caller, values = interp_args ctx_caller args in
    (* Enter the function frame, bind type parameters,
       and copy-in to the local environment *)
    let ctx_callee = Ctx.enter_frame ctx_callee in
    let ctx_callee =
      interp_targs Ctx.add_td_loc ctx_caller ctx_callee tparams targs
    in
    let ctx_callee = copyin Ctx.add_var_loc ctx_callee params values in
    (* Execute the body *)
    let sign, ctx_callee = Arch.interp_extern ctx_callee in
    (* Copy-out from the local environment *)
    let ctx_caller = copyout ctx_caller ctx_callee params args in
    (sign, ctx_caller, ctx_callee)

  and interp_extern_method_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (tparams : tparam list) (params : param list) (targs : typ list)
      (args : arg list) =
    (* Enter the function frame *)
    let ctx_callee = Ctx.enter_frame ctx_callee in
    (* Align the parameters with the arguments *)
    let params, args = align_params_with_args params args in
    (* Evaluate the arguments *)
    let ctx_caller, values = interp_args ctx_caller args in
    (* Bind the type parameters *)
    let ctx_callee =
      interp_targs Ctx.add_td_loc ctx_caller ctx_callee tparams targs
    in
    (* Copy-in to the local environment *)
    let ctx_callee = copyin Ctx.add_var_loc ctx_callee params values in
    (* Execute the body *)
    let sign, ctx_callee = Arch.interp_extern ctx_callee in
    (* Copy-out from the local environment *)
    let ctx_caller = copyout ctx_caller ctx_callee params args in
    (sign, ctx_caller, ctx_callee)

  and interp_method_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (_tparams : tparam list) (params : param list) (_targs : typ list)
      (args : arg list) (body : block) =
    (* Align the parameters with the arguments *)
    let params, args = align_params_with_args params args in
    (* Evaluate the arguments *)
    let ctx_caller, values = interp_args ctx_caller args in
    (* Copy-in to the object environment *)
    let ctx_callee = copyin Ctx.add_var_obj ctx_callee params values in
    (* Execute the body *)
    let sign, ctx_callee = interp_block Sig.Cont ctx_callee body in
    (* Copy-out from the object environment *)
    let ctx_caller = copyout ctx_caller ctx_callee params args in
    (sign, ctx_caller, ctx_callee)

  and interp_action_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (params : param list) (args : arg list) (body : block) =
    (* Align the parameters with the arguments *)
    let params, args = align_params_with_args params args in
    (* Evaluate the arguments *)
    let ctx_caller, values = interp_args ctx_caller args in
    (* Enter the function frame and copy-in to the local environment *)
    let ctx_callee = Ctx.enter_frame ctx_callee in
    let ctx_callee = copyin Ctx.add_var_loc ctx_callee params values in
    (* Execute the body *)
    let sign, ctx_callee = interp_block Sig.Cont ctx_callee body in
    (* Copy-out from the local environment *)
    let ctx_caller = copyout ctx_caller ctx_callee params args in
    (sign, ctx_caller, ctx_callee)

  and interp_table_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (table : table)
      =
    (* Evaluate the keys *)
    let keys, actions, entries, default, custom = table in
    let exprs, mtchs = List.map it keys |> List.split in
    let ctx_caller, values = interp_exprs ctx_caller exprs in
    let keys = List.combine values mtchs in
    (* Invoke the match-action table to get an action *)
    let action, value =
      match_action ctx_callee keys actions entries default custom
    in
    let hit = Value.access_field "hit" value |> Value.get_bool in
    if not hit then
      let sign = Sig.Ret (Some value) in
      (sign, ctx_caller, ctx_callee)
    else
      let action, args = Option.get action |> it in
      (* Find and call the action *)
      let sign, ctx_callee = interp_func_call ctx_callee action [] args in
      let sign =
        match (sign : Sig.t) with
        | Cont -> Sig.Ret (Some value)
        | Ret _ ->
            Format.asprintf "(interp_table_app) Action should not return.\n"
            |> failwith
        | Exit -> sign
      in
      (sign, ctx_caller, ctx_callee)

  and interp_inter_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (func : Func.t)
      (targs : typ list) (args : arg list) =
    (* Restrict the callee context and call the function *)
    (* Difference between calling an extern method and a parser/control method
       is that parameters are passed to the method-local scope for the former
       and for the latter, it is the object-local scope *)
    let sign, ctx_caller, _ctx_callee =
      match func with
      | ExternF { vis_glob; tparams; params } ->
          let ctx_callee = { ctx_callee with vis_glob } in
          interp_extern_app ctx_caller ctx_callee tparams params targs args
      | ExternMethodF { vis_obj; tparams; params } ->
          let ctx_callee = { ctx_callee with vis_obj } in
          interp_extern_method_app ctx_caller ctx_callee tparams params targs
            args
      | MethodF { vis_obj; tparams; params; body } ->
          let ctx_callee = { ctx_callee with vis_obj } in
          interp_method_app ctx_caller ctx_callee tparams params targs args body
      | ActionF { vis; params; body } ->
          let ctx_callee = { ctx_callee with vis_glob = vis } in
          interp_action_app ctx_caller ctx_callee params args body
      | _ ->
          Format.asprintf "(interp_inter_app) %a is not intra-callable\n"
            Func.pp func
          |> failwith
    in
    (* Account for global mutations, which shouldn't happen *)
    (sign, ctx_caller)

  and interp_intra_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ctx_table
      (func : Func.t) (_targs : typ list) (args : arg list) =
    (* Restrict the callee context and call the function *)
    let sign, ctx_caller, ctx_callee =
      match func with
      | TableF { vis_obj } ->
          let ctx_callee = { ctx_callee with vis_obj } in
          let table = Option.get ctx_table in
          interp_table_app ctx_caller ctx_callee table
      | ActionF { vis; params; body } ->
          let ctx_callee = { ctx_callee with vis_obj = vis } in
          interp_action_app ctx_caller ctx_callee params args body
      | _ ->
          Format.asprintf "(interp_intra_app) %a is not inter-callable\n"
            Func.pp func
          |> failwith
    in
    (* Account for object-local mutations *)
    let ctx_caller = { ctx_caller with env_obj = ctx_callee.env_obj } in
    (sign, ctx_caller)

  and interp_object_call (ctx : Ctx.t) (path : Path.t) (obj : Object.t)
      (fid : id) (targs : typ list) (args : arg list) =
    (* Resolve callee object's scope and find the callee method *)
    match obj with
    | ExternO { vis_glob; env_obj } ->
        let ctx_callee =
          Ctx.init (path, (fid.it, [])) ctx.env_glob env_obj (TDEnv.empty, [])
        in
        let ctx_callee = { ctx_callee with vis_glob } in
        let func = Ctx.find_func_obj (fid.it, args) ctx_callee in
        let cid = (path, (fid.it, Func.get_params func)) in
        let ctx_callee = Ctx.set_id cid ctx_callee in
        interp_inter_app ctx ctx_callee func targs args
    | ParserO { vis_glob; env_obj; mthd } | ControlO { vis_glob; env_obj; mthd }
      ->
        assert (fid.it = "apply");
        let ctx_callee =
          Ctx.init (path, (fid.it, [])) ctx.env_glob env_obj (TDEnv.empty, [])
        in
        let ctx_callee = { ctx_callee with vis_glob } in
        let func = mthd in
        interp_inter_app ctx ctx_callee func targs args
    | TableO { table; mthd } ->
        assert (fid.it = "apply" && targs = [] && args = []);
        let ctx_callee =
          Ctx.init (path, (fid.it, [])) ctx.env_glob ctx.env_obj env_stack_empty
        in
        let ctx_callee = { ctx_callee with vis_glob = ctx.vis_glob } in
        let ctx_table = Some table in
        let func = mthd in
        interp_intra_app ctx ctx_callee ctx_table func targs args
    | _ ->
        Format.asprintf "(interp_object_call) %a is not a callable object\n"
          Object.pp obj
        |> failwith

  (* In addition, headers support the following methods: ... (8.17) *)
  and interp_builtin_header_call (ctx : Ctx.t) (base : expr) (_valid : bool)
      (fields : (member' * Value.t) list) (fid : id) (_targs : typ list)
      (_args : arg list) =
    match fid.it with
    | "isValid" ->
        let value = Some (Value.BoolV true) in
        let sign = Sig.Ret value in
        (sign, ctx)
    | "setValid" ->
        let value = Value.HeaderV (true, fields) in
        let ctx = interp_write ctx base value in
        let sign = Sig.Ret None in
        (sign, ctx)
    | "setInvalid" ->
        let value = Value.HeaderV (false, fields) in
        let ctx = interp_write ctx base value in
        let sign = Sig.Ret None in
        (sign, ctx)
    | _ ->
        Format.asprintf
          "(interp_builtin_header_call) %s call not supported for header\n"
          fid.it
        |> failwith

  (* Finally, P4 offers the following computations that
     can be used to manipulate the elements at the
     front and back of the stack: ... (8.18) *)
  and interp_builtin_stack_call (ctx : Ctx.t) (base : expr)
      (values : Value.t list) (idx : Bigint.t) (size : Bigint.t) (fid : id)
      (_targs : typ list) (args : arg list) =
    match fid.it with
    | "pop_front" ->
        assert (List.length args = 1);
        let ctx, count =
          (match (List.hd args).it with
          | ExprA expr -> expr
          | _ -> assert false)
          |> interp_expr ctx
        in
        let count = Value.get_num count |> Bigint.to_int |> Option.get in
        let values =
          let values = Array.of_list values in
          let size = Bigint.to_int size |> Option.get in
          Array.sub values count (size - count) |> Array.to_list
        in
        (* (TODO) How to fill the rest with default values? Should know the types *)
        let idx =
          let idx = Bigint.to_int idx |> Option.get in
          Bigint.of_int (idx + count)
        in
        let value = Value.StackV (values, idx, size) in
        let ctx = interp_write ctx base value in
        let sign = Sig.Ret None in
        (sign, ctx)
    | _ ->
        Format.asprintf
          "(interp_builtin_stack_call) %s call not supported for header stack\n"
          fid.it
        |> failwith

  and interp_method_call (ctx : Ctx.t) (base : expr) (fid : id)
      (targs : typ list) (args : arg list) =
    let ctx, value_base = interp_expr ctx base in
    match value_base with
    (* Calling a built-in method of a non-object value *)
    | HeaderV (valid, fields) ->
        interp_builtin_header_call ctx base valid fields fid targs args
    | StackV (values, idx, size) ->
        interp_builtin_stack_call ctx base values idx size fid targs args
    (* Calling a method of an actual (user-defined) object *)
    | RefV path ->
        let obj = Sto.find path !sto in
        interp_object_call ctx path obj fid targs args
    | _ ->
        Format.asprintf "(TODO: interp_method_call) Call %s of %a\n" fid.it
          Value.pp value_base
        |> failwith

  and interp_func_call (ctx : Ctx.t) (fvar : var) (targs : typ list)
      (args : arg list) : Sig.t * Ctx.t =
    let interp_inter_func_call (fid : id) =
      let ctx_callee =
        Ctx.init ([], (fid.it, [])) ctx.env_glob env_empty env_stack_empty
      in
      let func = Ctx.find_func_glob (fid.it, args) ctx in
      let cid = ([], (fid.it, Func.get_params func)) in
      let ctx_callee = Ctx.set_id cid ctx_callee in
      interp_inter_app ctx ctx_callee func targs args
    in
    let interp_intra_func_call (fid : id) =
      let ctx_callee =
        Ctx.init ([], (fid.it, [])) ctx.env_glob ctx.env_obj env_stack_empty
      in
      let ctx_callee = { ctx_callee with vis_glob = ctx.vis_glob } in
      let func = Ctx.find_func_obj (fid.it, args) ctx in
      let cid = ([], (fid.it, Func.get_params func)) in
      let ctx_callee = Ctx.set_id cid ctx_callee in
      interp_intra_app ctx ctx_callee None func targs args
    in
    match fvar.it with
    | Top fid -> interp_inter_func_call fid
    | Bare fid when Option.is_some (Ctx.find_func_obj_opt (fid.it, args) ctx) ->
        interp_intra_func_call fid
    | Bare fid -> interp_inter_func_call fid

  and interp_call (ctx : Ctx.t) (func : expr) (targs : typ list)
      (args : arg list) =
    match func.it with
    (* method call *)
    | ExprAccE (expr, fid) -> interp_method_call ctx expr fid targs args
    (* function call *)
    | VarE fvar -> interp_func_call ctx fvar targs args
    | _ ->
        Format.asprintf "(interp_call) %a is not a function" Syntax.Pp.pp_expr
          func
        |> failwith

  (* Logic for match-action table *)
  and eval_binop_eq_default (ctx : Ctx.t) (value : Value.t): bool =
    match value with
    | BoolV b -> not b
    | AIntV value
    | BitV (_, value)
    | IntV (_, value)
    | VBitV (_, _, value) ->
        Bigint.(value = zero)
    | StrV value -> value = ""
    | StackV (values, _, _)
    | TupleV values ->
        List.for_all (fun value -> eval_binop_eq_default ctx value) values
    | StructV entries
    | UnionV entries ->
        let values = List.map (fun (_,v) -> v) entries in
        List.for_all (fun value -> eval_binop_eq_default ctx value) values
    | HeaderV (valid, entries) ->
        let values = List.map (fun (_,v) -> v) entries in
        not valid && List.for_all (fun value -> eval_binop_eq_default ctx value) values
    | EnumFieldV (id, member) ->
      let typ = Ctx.find_td id ctx in
      begin match typ with
      | EnumT (_id, members) ->
        let member' = List.hd members in
        member = member'
      | _ -> failwith "1"
      end
    | SEnumFieldV (_, _, value) -> 
        let value = Value.get_num value in
        Bigint.(value = zero)
    | _ -> failwith "1"

  and get_prefix (mask : Value.t) (prefix : int) : int =
    match mask with
    | BitV (width, value) ->
        if Bigint.(width = zero) then prefix 
        else
          let two = Bigint.(one + one) in
          let width' = Bigint.(width - one) in
          let value' = Bigint.(value / two) in
          let mask' = bit_of_raw_int value' width' in
          if Bigint.(value % two = zero) then
            if prefix <> 0 then failwith "invalid lpm mask"
            else get_prefix mask' prefix
          else get_prefix mask' (prefix+1)
    | _ -> failwith "wrong type for lpm mask"

  and mask_values (ctx : Ctx.t) (key_value : Value.t) (expr : expr) : Value.t * Value.t =
    let width = Value.get_width key_value in
    let limit = Value.AIntV (power_of_two width) in
    match expr.it with
    | MaskE (expr, mask) ->
        (* Get masked values *)
        let expr_value = interp_expr ctx expr |> snd in
        let mask_value = interp_expr ctx mask |> snd in
        (* If expr | mask value has 1s outside of field bit width *)
        let is_expr_limit = eval_binop_ge expr_value limit |> Value.get_bool in
        let is_mask_limit = eval_binop_ge mask_value limit |> Value.get_bool in
        if is_expr_limit || is_mask_limit then failwith "has 1s outside of field bit width"
        else
           (* Should convert to bit *)
          let mask_value = Value.get_num mask_value in
          let mask_value = bit_of_raw_int mask_value width in
          (* Mask value *)
          let masked_expr_value = eval_binop_bitand expr_value mask_value in
          let masked_key_value = eval_binop_bitand key_value mask_value in
          (masked_expr_value, masked_key_value)
    | _ ->
        let expr_value = interp_expr ctx expr |> snd in
        (* If expr value has 1s outside of field bit width *)
        let is_expr_limit = eval_binop_ge expr_value limit |> Value.get_bool in
        if is_expr_limit then failwith "has 1s outside of field bit width"
        else (expr_value, key_value)

  and get_prior_lpm (ctx : Ctx.t) (expr : expr) (width : int) : int option =
    match expr.it with
    | MaskE (_, mask) ->
        let width = Bigint.of_int width in
        (* Get masked values *)
        let mask_value = interp_expr ctx mask |> snd |> Value.get_num in
        (* Mask should convert to bit *)
        let mask_value = bit_of_raw_int mask_value width in
        let prefix = get_prefix mask_value 0 in
        Some prefix
    | _ -> Some width

  (* Match *)
  (* TODO : OPTIONAL and RANGE *)
  (* TODO : What about un init enum? *)
  and check_match (ctx : Ctx.t) (ent_list : mtch list)
      (key_list : (Value.t * mtch_kind) list) : bool =
    let check_match' is_match ent key =
      (* If it doesn't match then skip rest *)
      if not is_match then is_match
      else
        let key_value, key_mtch_kind = key in
        match ent.it with
        (* Wildcard *)
        | AnyM -> is_match
        | ExprM expr ->
          (* Seperate between exact and other *)
            if key_mtch_kind.it = "exact" then 
              let ent_value = interp_expr ctx expr |> snd in
              eval_binop_eq ent_value key_value
            else 
              let ent_value, key_value = mask_values ctx key_value expr in
              eval_binop_eq ent_value key_value
        | DefaultM -> eval_binop_eq_default ctx key_value
    in
    (* If entry list is just underline('_') return true *)
    match ent_list with 
      | [{ it = AnyM; _ }] -> true
      | _ -> List.fold_left2 check_match' true ent_list key_list

  and set_priors (ctx : Ctx.t) (keys : (Value.t * mtch_kind) list) (entries : table_entry list)
     (prior_delta : int) (largest_priority_wins : bool) : int option list =
    let length = List.length entries in
    let is_ternary (_key_value, key_kind) = key_kind.it = "ternary" in
    let is_lpm ind (key_value, key_kind) = 
      let width = key_value |> Value.get_width |> Bigint.to_int |> Option.get in
      if key_kind.it = "lpm" then (ind, width) else (-1, 0) 
    in
    (* If there are least one ternary then need priority *)
    let need_prior = List.exists is_ternary keys in
    (* If match kind is lpm, then save index and type's width.
       Index is for entry's lpm key, width is for maximum prefix length. *)
    let lpms = List.mapi is_lpm keys in
    (* Collect only lpm  *)
    let lpms = List.filter (fun (ind, _) -> ind >= 0) lpms in
    let cnt_lpms = List.length lpms in
    (* no_spec is bool value for whether developer specifies priority. If false 
       and other entry has specified priority, it cause error. *)
    let no_spec = true in
    (* Function that compute priority. Logic is described in (14.2.1.4) Entry priorities *)
    let set_basic_priors prior _ent = 
      let is_spec = false in
      if is_spec then failwith "spec priority error"
      else
        let prior = Option.get prior in
        let prior = Some (prior + prior_delta) in
        prior, prior
    in      
    let set_priors' prior _ent = 
      (* (TODO) Should get is_specified, prior from ent. It may be from optEntryPriority*)
      let is_spec = false in
      let prior = Option.get prior in
      if is_spec then
        let spec_prior = Some 0 in
        let spec_prior' = Option.get spec_prior in
        if (largest_priority_wins && spec_prior' > prior) ||
           (not largest_priority_wins && spec_prior' < prior) then
          let _ = Printf.printf "Warning entries_out_of_priority_order" in
          spec_prior, spec_prior
        else spec_prior, spec_prior
      else 
        if largest_priority_wins then 
          let prior = Some (prior - prior_delta) in
          prior, prior
        else
          let prior = Some (prior + prior_delta) in
          prior, prior
    in
    (* Function that extract priority (prefix) at lpm *)
    let extract_lpms ent =
      let lpm_ind, width = List.hd lpms in
      let ent_lists, _ = ent.it in
      let ent_list = List.nth ent_lists lpm_ind in
      match ent_list.it with
      | AnyM -> Some 0
      | DefaultM -> Some width
      | ExprM expr -> get_prior_lpm ctx expr width
    in
    if cnt_lpms > 1 then failwith "lpms should be most one"
    else if need_prior && no_spec then 
      let basic_priors = List.fold_left_map set_basic_priors (Some 1) entries |> snd in
      if largest_priority_wins then List.rev basic_priors
      else basic_priors
    else if need_prior then 
      let first_prior = Some 1 in
      List.fold_left_map set_priors' first_prior entries |> snd
    else if cnt_lpms = 1 then List.map extract_lpms entries
    else List.init length (fun _ -> None)

  and match_action (ctx : Ctx.t) (keys : (Value.t * mtch_kind) list)
      (actions : table_action list) (entries : table_entry list)
      (default : table_default option) (_custom : table_custom list) =
    let path, _ = ctx.id in
    let id = List.rev path |> List.hd in
    (* Determine the action to be run *)
    (* Decl the default_action*)
    let default_action = match default with
      | Some { it = action, _; _ } -> Some action
      | None -> None
    in
    (* Compute priorities and save in list *)
    (* If no_ternary, it means there are only exacts and lpm*)
    let priors = set_priors ctx keys entries 1 true in

    let find_action (prior, action) entry new_prior=
      let matches, entry_action = entry.it in
      let new_action = Some entry_action in
      (* If current action is None, it means no matched action yet *)
      let is_action_none = Option.is_none action in
      (* If prior is None, it means there are only exacts *)
      let is_prior_none = Option.is_none prior in
      (* Check match *)
      let is_match = check_match ctx matches keys in
      (* Compare priorities *)
      let cmp_prior = Option.compare Int.compare prior new_prior in
      (* If first matched *)
      if is_match && is_action_none then (new_prior, new_action)
      (* If match one more in only exact field, it may causes error *)
      else if is_match && is_prior_none then failwith "no dup in only exacts"
      (* If new_priority is larger, then replace the action *)
      else if is_match && cmp_prior < 0 then (new_prior, new_action)
      (* Else not changed *)
      else (prior, action) 
    in

    let _, action =
      List.fold_left2 find_action (None, None) entries priors
    in
    let action = if Option.is_none action then default_action else action in
    (* Calling an apply method on a table instance returns a value with
       a struct type with three fields. This structure is synthesized
       by the compiler automatically. (14.2.2) *)
    let value =
      let hit = Value.BoolV (Option.is_some action) in
      let miss = Value.BoolV (Option.is_none action) in
      (* For enum values without an underlying type the default value is
         the first value that appears in the enum type declaration. (7.3) *)
      let action_run =
        let action_run =
          Option.value ~default:(List.hd actions) action
          |> it |> fst
          |> Format.asprintf "%a" Syntax.Pp.pp_var
        in
        Value.EnumFieldV ("action_list(" ^ id ^ ")", action_run)
      in
      Value.StructV [ ("hit", hit); ("miss", miss); ("action_run", action_run) ]
    in
    (action, value)
end
