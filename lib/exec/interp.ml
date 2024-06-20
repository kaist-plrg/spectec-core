open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Context
open Runtime.Signal
open Driver

module Make (Arch : ARCH) : INTERP = struct
  (* Global store *)

  let sto = ref Sto.empty
  let init _sto = sto := _sto

  (* Helper to access aggregate value *)

  let fetch_struct_field (value : Value.t) (field : field) =
    match value with
    | StructV fields -> List.assoc field fields
    | _ -> assert false

  let fetch_enum_field (value : Value.t) =
    match value with EnumFieldV field -> field | _ -> assert false

  (* Interpreter for type simplification,
     Assume: evaluation of bit width should never change the context *)

  let rec interp_type (ctx : Ctx.t) (typ : typ) : Type.t =
    match typ with
    | BoolT -> BoolT
    | ErrT -> Ctx.find_td_glob "error" ctx |> Option.get
    | StrT -> StrT
    | AIntT -> AIntT
    | IntT width ->
        let width =
          interp_expr ctx width |> snd |> Runtime.Ops.extract_bigint
        in
        IntT width
    | BitT width ->
        let width =
          interp_expr ctx width |> snd |> Runtime.Ops.extract_bigint
        in
        BitT width
    | VBitT width ->
        let width =
          interp_expr ctx width |> snd |> Runtime.Ops.extract_bigint
        in
        VBitT width
    | NameT (Top id) -> Ctx.find_td_glob id ctx |> Option.get
    | NameT (Bare id) -> Ctx.find_td id ctx |> Option.get
    (* (TODO) Handle specialized types *)
    | SpecT (var, _) -> interp_type ctx (NameT var)
    | StackT (typ, size) ->
        let typ = interp_type ctx typ in
        let size = interp_expr ctx size |> snd |> Runtime.Ops.extract_bigint in
        StackT (typ, size)
    | TupleT typs ->
        let typs = List.map (interp_type ctx) typs in
        TupleT typs
    | _ ->
        Format.asprintf "(TODO: eval_type) %a" Syntax.Print.print_type typ
        |> failwith

  (* Interpreter for expressions *)

  and interp_expr (ctx : Ctx.t) (expr : expr) : Ctx.t * Value.t =
    match expr with
    | BoolE b -> interp_bool ctx b
    | StrE s -> interp_str ctx s
    | NumE (value, encoding) -> interp_num ctx value encoding
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
    | TypeAccE (var, field) -> interp_type_acc ctx var field
    | ErrAccE field -> interp_error_acc ctx field
    | ExprAccE (base, field) -> interp_expr_acc ctx base field
    | CallE (func, targs, args) -> interp_call_as_expr ctx func targs args
    | InstE _ ->
        Format.eprintf
          "(interp_expr) Instantiation expression should have been evaluated \
           in instantiation.";
        assert false

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
    match var with
    | Top id ->
        let value = Ctx.find_var_glob id ctx |> Option.get |> snd in
        (ctx, value)
    | Bare id ->
        let value = Ctx.find_var id ctx |> Option.get |> snd in
        (ctx, value)

  and interp_list (ctx : Ctx.t) (exprs : expr list) : Ctx.t * Value.t =
    let ctx, values = interp_exprs ctx exprs in
    let value = Value.TupleV values in
    (ctx, value)

  and interp_record (ctx : Ctx.t) (fields : (field * expr) list) :
      Ctx.t * Value.t =
    let fields, exprs = List.split fields in
    let ctx, values = exprs |> interp_exprs ctx in
    let fields = List.map2 (fun field value -> (field, value)) fields values in
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
    let cond = match value_cond with BoolV b -> b | _ -> assert false in
    let expr = if cond then expr_tru else expr_fls in
    interp_expr ctx expr

  and interp_cast (ctx : Ctx.t) (typ : typ) (expr : expr) : Ctx.t * Value.t =
    let typ = interp_type ctx typ |> Eval.eval_simplify_type ctx in
    let ctx, value = interp_expr ctx expr in
    let value = Runtime.Ops.eval_cast typ value in
    (ctx, value)

  and interp_mask (_ctx : Ctx.t) : Ctx.t * Value.t = assert false
  and interp_range (_ctx : Ctx.t) : Ctx.t * Value.t = assert false

  and interp_arr_acc (ctx : Ctx.t) (base : expr) (idx : expr) : Ctx.t * Value.t
      =
    let ctx, values = interp_exprs ctx [ base; idx ] in
    let value_base, value_idx = (List.nth values 0, List.nth values 1) in
    match value_base with
    (* (TODO) Insert bounds checking *)
    | StackV (values, _, _) ->
        let idx = Eval.unpack_value value_idx |> Bigint.to_int |> Option.get in
        let value = List.nth values idx in
        (ctx, value)
    | _ -> assert false

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

  and interp_type_acc (ctx : Ctx.t) (var : var) (field : field) :
      Ctx.t * Value.t =
    let typ =
      match var with
      | Top id -> Ctx.find_td_glob id ctx |> Option.get
      | Bare id -> Ctx.find_td id ctx |> Option.get
    in
    match typ with
    | EnumT fields ->
        if List.mem field fields then (ctx, EnumFieldV field) else assert false
    | _ -> assert false

  and interp_error_acc (ctx : Ctx.t) (field : field) : Ctx.t * Value.t =
    let typ = Ctx.find_td_glob "error" ctx |> Option.get in
    match typ with
    | ErrT fields ->
        if List.mem field fields then (ctx, ErrV field) else assert false
    | _ -> assert false

  and interp_builtin_stack_acc (ctx : Ctx.t) (values : Value.t list)
      (next : Bigint.t) (_size : Bigint.t) (field : field) =
    match field with
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
        Format.eprintf
          "(interp_builtin_stack_acc) %s member access not supported for \
           header stack\n"
          field;
        assert false

  and interp_expr_acc (ctx : Ctx.t) (base : expr) (field : field) :
      Ctx.t * Value.t =
    let ctx, value_base = interp_expr ctx base in
    match value_base with
    | HeaderV (_, fields) | StructV fields ->
        let value = List.assoc field fields in
        (ctx, value)
    | RefV path ->
        let value = Value.RefV (path @ [ field ]) in
        (ctx, value)
    | StackV (values, next, size) ->
        interp_builtin_stack_acc ctx values next size field
    | _ ->
        Format.eprintf "(TODO: interp_expr_acc) %a.%s\n" Value.pp value_base
          field;
        assert false

  and interp_call_as_expr (ctx : Ctx.t) (func : expr) (targs : typ list)
      (args : arg list) : Ctx.t * Value.t =
    let sign, ctx = interp_call ctx func targs args in
    let value =
      match (sign : Sig.t) with
      | Ret value -> Option.get value
      | _ -> assert false
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
    match lvalue with
    | VarE (Bare id) ->
        let typ = Ctx.find_var id ctx |> Option.get |> fst in
        let value = Runtime.Ops.eval_cast typ value in
        Ctx.update_var id typ value ctx
    | ExprAccE (base, field) -> (
        let ctx, value_base = interp_expr ctx base in
        let update_field fields field =
          List.map
            (fun (k, v) -> if k = field then (k, value) else (k, v))
            fields
        in
        match value_base with
        | StructV fields ->
            let fields = update_field fields field in
            interp_write ctx base (StructV fields)
        | HeaderV (valid, fields) ->
            let fields = update_field fields field in
            interp_write ctx base (HeaderV (valid, fields))
        | StackV (values, next, size) when field = "next" ->
            let idx = Bigint.to_int next |> Option.get in
            let values =
              List.mapi (fun i v -> if i = idx then value else v) values
            in
            (* (TODO) In order to enforce,
               "It is automatically advanced on each successful call to extract. (13.9)"
               Yet doesn't seem to be in the right place *)
            let next = Bigint.(next + one) in
            interp_write ctx base (StackV (values, next, size))
        | _ -> assert false)
    | _ ->
        Format.eprintf "(TODO: interp_write) %a" Syntax.Print.print_expr lvalue;
        assert false

  and interp_stmt (sign : Sig.t) (ctx : Ctx.t) (stmt : stmt) =
    match stmt with
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
            (sign, ctx) block
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
        Format.eprintf "(interp_trans) Exit unallowed within parser.\n";
        assert false
    | Cont ->
        (* (TODO) better handling of accept/reject *)
        if next = "accept" || next = "reject" then (sign, ctx)
        else
          let state_next = Ctx.find_func next ctx |> Option.get in
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
        Format.eprintf "(interp_select) Exit unallowed within parser.\n";
        assert false
    | Cont ->
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
              let mtchs, next = case in
              let select_mtch (mtch : mtch) (value : Value.t) =
                match mtch with
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
        match decl with
        | VarD { name = id; typ; init = None } ->
            let typ = interp_type ctx typ in
            let value = Runtime.Ops.eval_default_value typ in
            let ctx = Ctx.add_var_loc id typ value ctx in
            (sign, ctx)
        | VarD { name = id; typ; init = Some value } ->
            let typ = interp_type ctx typ in
            let ctx, value = interp_expr ctx value in
            let ctx = Ctx.add_var_loc id typ value ctx in
            (sign, ctx)
        | _ ->
            Format.eprintf "(TODO: interp_decl) %a" Syntax.Print.print_decl
              (0, decl);
            assert false)

  (* (TODO) assume switch matches on table apply result only,
     since case with expression is not supported in Petr4 parser *)
  and interp_switch (sign : Sig.t) (ctx : Ctx.t) (expr : expr)
      (cases : switch_case list) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont ->
        let ctx, value = interp_expr ctx expr in
        let value = fetch_enum_field value in
        let switch_cases (block_found : bool * block option)
            (case : switch_case) =
          let case, block = case in
          match block_found with
          (* match complete *)
          | true, Some _ -> block_found
          (* during fallthrough *)
          | true, None -> (
              match case with
              | CaseC _ -> (true, Some block)
              | FallC _ -> (true, None)
              | DefaultC -> (true, Some block))
          (* match not found *)
          | false, _ -> (
              match case with
              | CaseC case when case = value -> (true, Some block)
              | FallC case when case = value -> (true, None)
              | DefaultC -> (true, Some block)
              | _ -> block_found)
        in
        let _, block = List.fold_left switch_cases (false, None) cases in
        let block = match block with None -> [] | Some block -> block in
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
          let pname, _, _, _ = param in
          PMap.add pname param params_map)
        PMap.empty params
    in
    List.fold_left2
      (fun (params, args) param arg ->
        match arg with
        | ExprA arg -> (params @ [ param ], args @ [ arg ])
        | NameA (id, arg) ->
            let param = PMap.find id params_map in
            (params @ [ param ], args @ [ arg ])
        | _ ->
            Format.eprintf "(TODO: align_params_with_args) %a"
              Syntax.Print.print_arg arg;
            assert false)
      ([], []) params args

  (* adder determines where to add the argument, either object or local scope *)
  and copyin adder (ctx_callee : Ctx.t) (params : param list)
      (values : Value.t list) =
    let copyin' (ctx_callee : Ctx.t) (param : param) (value : Value.t) =
      let id, dir, typ, _ = param in
      (* (TODO) Is it correct to evaluate the type at callee? *)
      match dir with
      | No | In | InOut ->
          let typ = interp_type ctx_callee typ in
          adder id typ value ctx_callee
      | Out ->
          let typ = interp_type ctx_callee typ in
          let value = Runtime.Ops.eval_default_value typ in
          adder id typ value ctx_callee
    in
    List.fold_left2 copyin' ctx_callee params values

  and copyout (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (params : param list)
      (exprs : expr list) =
    let copyout' (ctx_caller : Ctx.t) (param : param) (expr : expr) =
      let id, dir, _, _ = param in
      match dir with
      | InOut | Out ->
          let value = Ctx.find_var id ctx_callee |> Option.get |> snd in
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
        adder tparam targ ctx_callee)
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
    let sign, ctx_callee = Arch.interp_extern Sig.Cont ctx_callee in
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
    let sign, ctx_callee = Arch.interp_extern Sig.Cont ctx_callee in
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

  and interp_table_app (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) ctx_table =
    let key, actions, entries, default, custom = ctx_table in
    (* Invoke the match-action table to get an action *)
    let ctx_caller, key =
      List.fold_left
        (fun (ctx_caller, key) (expr, mtch) ->
          let ctx_caller, value = interp_expr ctx_caller expr in
          (ctx_caller, key @ [ (value, mtch) ]))
        (ctx_caller, []) key
    in
    let action, value =
      Control.match_action ctx_callee key actions entries default custom
    in
    let hit =
      match fetch_struct_field value "hit" with
      | BoolV b -> b
      | _ -> assert false
    in
    if not hit then
      let sign = Sig.Ret (Some value) in
      (sign, ctx_caller, ctx_callee)
    else
      let action, args = Option.get action in
      (* Find and call the action *)
      let sign, ctx_callee = interp_func_call ctx_callee action [] args in
      let sign =
        match (sign : Sig.t) with
        | Cont -> Sig.Ret (Some value)
        | Ret _ ->
            Format.eprintf "(interp_table_call) Action should not return.\n";
            assert false
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
      | _ -> assert false
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
          let ctx_table = Option.get ctx_table in
          interp_table_app ctx_caller ctx_callee ctx_table
      | ActionF { vis; params; body } ->
          let ctx_callee = { ctx_callee with vis_obj = vis } in
          interp_action_app ctx_caller ctx_callee params args body
      | _ -> assert false
    in
    (* Account for object-local mutations *)
    let ctx_caller = { ctx_caller with env_obj = ctx_callee.env_obj } in
    (sign, ctx_caller)

  and interp_object_call (ctx : Ctx.t) (path : Path.t) (obj : Object.t)
      (fid : Var.t) (targs : typ list) (args : arg list) =
    (* Resolve callee object's scope and find the callee method *)
    match obj with
    | ExternO { vis_glob; env_obj } ->
        let ctx_callee =
          Ctx.init (path, fid) ctx.env_glob env_obj (TDEnv.empty, [])
        in
        let ctx_callee = { ctx_callee with vis_glob } in
        let func = Ctx.find_func_obj fid ctx_callee |> Option.get in
        interp_inter_app ctx ctx_callee func targs args
    | ParserO { vis_glob; env_obj; mthd } | ControlO { vis_glob; env_obj; mthd }
      ->
        assert (fid = "apply");
        let ctx_callee =
          Ctx.init (path, fid) ctx.env_glob env_obj (TDEnv.empty, [])
        in
        let ctx_callee = { ctx_callee with vis_glob } in
        let func = mthd in
        interp_inter_app ctx ctx_callee func targs args
    | TableO { key; actions; entries; default; custom; mthd } ->
        assert (fid = "apply" && targs = [] && args = []);
        let ctx_callee =
          Ctx.init (path, fid) ctx.env_glob ctx.env_obj env_stack_empty
        in
        let ctx_callee = { ctx_callee with vis_glob = ctx.vis_glob } in
        let ctx_table = Some (key, actions, entries, default, custom) in
        let func = mthd in
        interp_intra_app ctx ctx_callee ctx_table func targs args
    | _ -> assert false

  (* In addition, headers support the following methods: ... (8.17) *)
  and interp_builtin_header_call (ctx : Ctx.t) (base : expr) (_valid : bool)
      (fields : (field * Value.t) list) (fid : string) (_targs : typ list)
      (_args : arg list) =
    match fid with
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
        Format.eprintf
          "(interp_builtin_header_call) %s call not supported for header\n" fid;
        assert false

  (* Finally, P4 offers the following computations that
     can be used to manipulate the elements at the
     front and back of the stack: ... (8.18) *)
  and interp_builtin_stack_call (ctx : Ctx.t) (base : expr)
      (values : Value.t list) (idx : Bigint.t) (size : Bigint.t) (fid : string)
      (_targs : typ list) (args : arg list) =
    match fid with
    | "pop_front" ->
        assert (List.length args = 1);
        let ctx, count =
          (match List.hd args with ExprA expr -> expr | _ -> assert false)
          |> interp_expr ctx
        in
        let count = Eval.unpack_value count |> Bigint.to_int |> Option.get in
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
        Format.eprintf
          "(interp_builtin_stack_call) %s call not supported for header stack\n"
          fid;
        assert false

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
        let obj = Sto.find path !sto |> Option.get in
        interp_object_call ctx path obj fid targs args
    | _ ->
        Format.eprintf "(TODO: interp_method_call) %a with %s\n" Value.pp
          value_base fid;
        assert false

  and interp_func_call (ctx : Ctx.t) (fid : var) (targs : typ list)
      (args : arg list) : Sig.t * Ctx.t =
    let interp_inter_func_call (fid : Var.t) =
      let ctx_callee =
        Ctx.init ([], fid) ctx.env_glob env_empty env_stack_empty
      in
      let func = Ctx.find_func_glob fid ctx |> Option.get in
      interp_inter_app ctx ctx_callee func targs args
    in
    let interp_intra_func_call (fid : Var.t) =
      let ctx_callee =
        Ctx.init ([], fid) ctx.env_glob ctx.env_obj env_stack_empty
      in
      let ctx_callee = { ctx_callee with vis_glob = ctx.vis_glob } in
      let func = Ctx.find_func_obj fid ctx |> Option.get in
      interp_intra_app ctx ctx_callee None func targs args
    in
    match fid with
    | Top fid -> interp_inter_func_call fid
    | Bare fid ->
        if Option.is_some (Ctx.find_func_obj fid ctx) then
          interp_intra_func_call fid
        else interp_inter_func_call fid

  and interp_call (ctx : Ctx.t) (func : expr) (targs : typ list)
      (args : arg list) =
    match func with
    (* method call *)
    | ExprAccE (expr, fid) -> interp_method_call ctx expr fid targs args
    (* function call *)
    | VarE fid -> interp_func_call ctx fid targs args
    | _ -> assert false
end
