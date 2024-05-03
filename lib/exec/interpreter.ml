open Syntax
open Syntax.Ast

(* A hack to avoid module name conflict *)
module P4Type = Type
open Runtime
open Runtime.Scope
open Runtime.Ienv

(* Store *)

let ienv = ref Env.empty

let register_ienv (_ienv : IEnv.t) =
  print_endline "Registering instantiated environment";
  Format.printf "%a@." IEnv.pp _ienv;
  ienv := _ienv

(* Checkers *)

(* It is illegal to use names only for some arguments:
   either all or no arguments must specify the parameter name. (8.20) *)

let check_args (args : Argument.t list) =
  assert (
    List.for_all
      (fun (arg : Argument.t) ->
        match arg with Expression _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : Argument.t) ->
           match arg with KeyValue _ -> true | _ -> false)
         args)

(* Value *)

let extract_ref (value : Value.t) : string =
  match value with VRef ref -> String.concat "." ref | _ -> assert false

let rec default_value (typ : Type.t) : Value.t =
  match typ with
  | TBool -> VBool false
  | TAInt -> VAInt Bigint.zero
  | TInt { width } -> VInt { value = Bigint.zero; width }
  | TBit { width } -> VBit { value = Bigint.zero; width }
  | TString -> VString ""
  | TError -> VError
  | TTuple types -> VTuple (List.map default_value types)
  | TStruct { entries } ->
      let entries =
        List.map (fun (name, typ) -> (name, default_value typ)) entries
      in
      VStruct { entries }
  | THeader { entries } ->
      let entries =
        List.map (fun (name, typ) -> (name, default_value typ)) entries
      in
      VHeader { valid = false; entries }
  | _ ->
      Format.asprintf "(TODO) default_value: not implemented for %a" Type.pp typ
      |> failwith

(* Statement evaluation *)

(* lvalue
   : prefixedNonTypeName | lvalue "." member
   | lvalue "[" expression "]" | lvalue "[" expression ":" expression "]" *)

let rec eval_write (bscope : bscope) (arg : Expression.t)
    (value : Value.t) : bscope =
  match arg with
  | Name { name = BareName text; _ } ->
      let var = text.str in
      let typ, _ = find_var var bscope in
      let value = Ops.eval_cast typ value in
      let bscope = update_value var value bscope in
      bscope
  (* (TODO) Ugly hack to get things done *)
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = Eval.eval_expr bscope expr in
      let name = name.str in
      match vexpr with
      | VStruct { entries } ->
          let entries =
            List.map
              (fun (k, v) -> if k = name then (k, value) else (k, v))
              entries
          in
          let vexpr = Value.VStruct { entries } in
          let bscope = eval_write bscope expr vexpr in
          bscope
      | _ ->
          Format.asprintf "(TODO: eval_write) Write to l-value %a" Value.pp
            vexpr
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_write) Write to l-value %s"
        (Pretty.print_expr arg)
      |> failwith

let rec eval_stmt (bscope : bscope) (stmt : Statement.t) :
    bscope =
  match stmt with
  | MethodCall { func; args; type_args = targs; _ } -> (
      match func with
      | ExpressionMember { expr; name; _ } ->
          let ref = Eval.eval_expr bscope expr |> extract_ref in
          eval_method_call bscope ref name.str args targs
      | _ -> failwith "(TODO: eval_stmt) MethodCall on non-ExpressionMember.")
  | Assignment { lhs; rhs; _ } ->
      let value = Eval.eval_expr bscope rhs in
      eval_write bscope lhs value
  (* Assume conditionals always go to the true branch *)
  | Conditional { tru; _ } -> eval_stmt bscope tru
  (*
  | Conditional { cond; tru; fls; _ } ->
      let vcond = Eval.eval_expr bscope cond |> Ops.eval_cast TBool in
      let body =
        match vcond with
        | VBool true -> Some tru
        | VBool false -> fls
        | _ -> assert false
      in
      (match body with
      | Some body -> eval_stmt bscope body
      | None -> bscope)
  *)
  | BlockStatement { block; _ } -> eval_block bscope block.statements
  | EmptyStatement _ -> bscope
  | DeclarationStatement { decl; _ } -> eval_decl bscope decl
  | _ ->
      Printf.sprintf "(TODO: eval_stmt) %s"
        (Syntax.Pretty.print_stmt 0 stmt |> Utils.Print.print_inline)
      |> print_endline;
      bscope

(* Block evaluation *)

and eval_block (bscope : bscope) (stmts : Statement.t list) :
    bscope =
  let tdenv, genv, lenv, _ = bscope in
  let _, _, _, sto = List.fold_left eval_stmt bscope stmts in
  (tdenv, genv, lenv, sto)

(* State evaluation *)
(* (TODO) Each states having a scope of functions would be desirable *)

and eval_state (bscope : bscope) (stmts : Statement.t list) :
    bscope =
  let tdenv, genv, lenv, _ = bscope in
  let _, _, _, sto = List.fold_left eval_stmt bscope stmts in
  (tdenv, genv, lenv, sto)

and eval_state_transition (bscope : bscope)
    (stmts : Statement.t list) (transition : Parser.transition)
    (funcs : Func.t list) : bscope =
  let bscope = eval_state bscope stmts in
  let next =
    match transition with
    | Direct { next; _ } -> next
    | Select { exprs; cases; _ } ->
        let vexprs =
          List.map (fun expr -> Eval.eval_expr bscope expr) exprs
        in
        let find_match (mtch : Text.t option) (case : Parser.case) =
          let matchcases = case.matches in
          let next = case.next in
          let find_match' (mtch : Text.t option) (vexpr : Value.t)
              (matchcase : Match.t) =
            match mtch with
            | Some _ -> mtch
            | None -> (
                match matchcase with
                | Default _ | DontCare _ -> Some case.next
                | Expression { expr; _ } -> (
                    let vmatch = Eval.eval_expr bscope expr in
                    match
                      Ops.eval_binop (Eq { tags = Info.M "" }) vexpr vmatch
                    with
                    | VBool true -> Some next
                    | VBool false -> None
                    | _ -> assert false))
          in
          List.fold_left2 find_match' mtch vexprs matchcases
        in
        let next = List.fold_left find_match None cases in
        Option.get next
  in
  match next.str with
  | "accept" | "reject" -> bscope
  | next -> (
      let find_state (func : string) = function
        | Func.FParser { name; _ } when name = func -> true
        | _ -> false
      in
      let func = List.find (find_state next) funcs in
      match func with
      | Func.FParser { genv; lenv; body; transition; _ } ->
          let tdenv, _, _, sto = bscope in
          eval_state_transition (tdenv, genv, lenv, sto) body transition funcs
      | _ -> assert false)

(* Declaration evaluation *)

and eval_decl (bscope : bscope) (decl : Declaration.t) :
    bscope =
  let tdenv, genv, lenv, sto = bscope in
  match decl with
  | Variable { name; typ; init; _ } ->
      let name = name.str in
      let typ = Eval.eval_typ bscope typ in
      let value =
        match init with
        | Some value -> Eval.eval_expr bscope value
        | None -> default_value typ
      in
      let lenv, sto = add_var name typ value lenv sto in
      (tdenv, genv, lenv, sto)
  | _ ->
      Printf.sprintf "(TODO: eval_decl) %s" (Pretty.print_decl 0 decl |> Utils.Print.print_inline) |> print_endline;
      bscope

(* Calling convention: Copy-in/out *)

and copyin (preload : bool) (caller_scope : bscope) (callee_scope : bscope)
  (params : Parameter.t list) (args : Argument.t list) :
    bscope =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Copy-in a single parameter-argument pair *)
  let copyin_single (callee_scope : bscope) (param : Parameter.t)
      (arg : Argument.t) =
    match param.direction with
    (* in parameters are initialized by copying the value of the
       corresponding argument when the invocation is executed *)
    | None | Some (In _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO: copyin) Support missing argument."
        in
        let typ = Eval.eval_typ callee_scope typ in
        let value = Eval.eval_expr caller_scope arg in
        (* (TODO) A problem arises because the instantiation phase statically allocates
           parameters to parser/control blocks but not for extern objects *)
        let bscope =
          (* In the case of parser/control apply *)
          if preload then update_value param value callee_scope
          else
            let tdenv, genv, lenv, sto = callee_scope in
            let lenv, sto = add_var param typ value lenv sto in
            (tdenv, genv, lenv, sto)
        in
        bscope
    (* Direction out parameters are always initialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This defaultialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let param, typ = (param.variable.str, param.typ) in
        let typ = Eval.eval_typ callee_scope typ in
        let value = default_value typ in
        (* (TODO) A problem arises because the instantiation phase statically allocates
           parameters to parser/control blocks but not for extern objects *)
        let bscope =
          (* In the case of parser/control apply *)
          if preload then update_value param value callee_scope
          else
            let tdenv, genv, lenv, sto = callee_scope in
            let lenv, sto = add_var param typ value lenv sto in
            (tdenv, genv, lenv, sto)
        in
        bscope
    (* inout parameters behave like a combination of in and out
       parameters simultaneously *)
    | Some (InOut _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO: copyin) Support missing argument."
        in
        let typ = Eval.eval_typ callee_scope typ in
        let value = Eval.eval_expr caller_scope arg in
        (* (TODO) A problem arises because the instantiation phase statically allocates
           parameters to parser/control blocks but not for extern objects *)
        let bscope =
          (* In the case of parser/control apply *)
          if preload then update_value param value callee_scope
          else
            let tdenv, genv, lenv, sto = callee_scope in
            let lenv, sto = add_var param typ value lenv sto in
            (tdenv, genv, lenv, sto)
        in
        bscope
  in
  List.fold_left2 copyin_single callee_scope params args

and copyout (caller_scope : bscope) (callee_scope : bscope)
    (params : Parameter.t list) (args : Argument.t list) : bscope =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Copy-out a single parameter-argument pair *)
  let copyout_single (bscope : bscope) (param : Parameter.t) (arg : Argument.t)
      =
    match param.direction with
    | Some (InOut _ | Out _) ->
        let param, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, value)
          | KeyValue { key; value; _ } -> (key.str, value)
          | _ -> failwith "(TODO: copyout) Support missing argument."
        in
        let _, value = find_var param callee_scope in
        eval_write bscope arg value
    | _ -> bscope
  in
  List.fold_left2 copyout_single caller_scope params args

(* Entry *)

and eval_parser_call (tdenv_callee : TDEnv.t) (sto_callee : Sto.t)
    (caller_scope : bscope) (funcs : Func.t list) (mthd : string)
    (args : Argument.t list) (_targs : P4Type.t list) =
  (* Find the method in the object *)
  let find_func = function
    | Func.FParser { name; _ } when name = mthd -> true
    | _ -> false
  in
  let func = List.find find_func funcs in
  (* Evaluate the method *)
  match func with
  | FParser { params; genv; lenv; body; transition; _ } ->
      let callee_scope = (tdenv_callee, genv, lenv, sto_callee) in
      let callee_scope = copyin true caller_scope callee_scope params args in
      let callee_scope =
        eval_state_transition callee_scope body transition funcs
      in
      let caller_scope = copyout caller_scope callee_scope params args in
      caller_scope
  | _ -> Format.asprintf "(TODO: eval_parser_call) %a" Func.pp func |> failwith

and eval_control_call (tdenv_callee : TDEnv.t) (sto_callee : Sto.t)
    (caller_scope : bscope) (funcs : Func.t list) (mthd : string)
    (args : Argument.t list) (_targs : P4Type.t list) =
  (* Find the method in the object *)
  let find_func = function
    | Func.FNormal { name; _ } when name = mthd -> true
    | _ -> false
  in
  let func = List.find find_func funcs in
  (* Evaluate the method *)
  match func with
  | FNormal { params; genv; lenv; body; _ } ->
      let callee_scope = (tdenv_callee, genv, lenv, sto_callee) in
      let callee_scope = copyin true caller_scope callee_scope params args in
      let callee_scope = eval_block callee_scope body in
      let caller_scope = copyout caller_scope callee_scope params args in
      caller_scope
  | _ -> Format.asprintf "(TODO: eval_control_call) %a" Func.pp func |> failwith

and eval_extern_method_call (tdenv_callee : TDEnv.t) (sto_callee : Sto.t)
    (caller_scope : bscope) (funcs : Func.t list) (mthd : string)
    (args : Argument.t list) (targs : P4Type.t list) =
  (* Find the method in the object *)
  let find_func = function
    | Func.FExtern { name; _ } when name = mthd -> true
    | _ -> false
  in
  let func = List.find find_func funcs in
  (* Evaluate the method *)
  match func with
  | FExtern { tparams; params; _ } ->
      let callee_scope = (tdenv_callee, Env.empty, Env.empty, sto_callee) in
      let callee_scope = Eval.eval_targs caller_scope callee_scope tparams targs in
      let callee_scope = copyin false caller_scope callee_scope params args in
      let callee_scope = Core.eval_builtin callee_scope mthd in
      let caller_scope = copyout caller_scope callee_scope params args in
      caller_scope
  | _ -> Format.asprintf "(TODO: eval_extern_method_call) %a" Func.pp func |> failwith

and eval_table_call (caller_scope : bscope) (keys : Table.key list)
  (actions : Table.action_ref list) (default : Table.action_ref option) =
    let keys =
      List.map
        (fun ({ key; match_kind; _ } : Table.key) ->
          let vkey = Eval.eval_expr caller_scope key in
          Format.printf "Key = %a\n" Value.pp vkey;
          (vkey, match_kind.str))
        keys
    in
    let action = Control.match_action keys actions default in
    match action with
    | Some (Name.BareName name, _args)
    | Some (Name.QualifiedName ([], name), _args) ->
        (* (TODO) Evaluate the action, requires a func-env and -store? *)
        Printf.printf "(TODO: eval_table_call) Action %s\n" name.str;
        caller_scope
    | _ -> caller_scope

and eval_method_call (caller_scope : bscope)
    (ref : string) (mthd : string) (args : Argument.t list)
    (targs : P4Type.t list) =
  (* Retrieve the object from the store *)
  let obj = IEnv.find ref !ienv in
  (* Evaluate the method call, pattern-matched against the object type *)
  match obj with
  | OParser { tdenv; sto; funcs } ->
      assert (mthd = "apply");
      eval_parser_call tdenv sto caller_scope funcs mthd args targs
  | OControl { tdenv; sto; funcs } ->
      assert (mthd = "apply");
      eval_control_call tdenv sto caller_scope funcs mthd args targs
  | OExtern { tdenv; sto; funcs; _ } ->
      eval_extern_method_call tdenv sto caller_scope funcs mthd args targs
  | OTable { genv; lenv; keys; actions; default } ->
      assert (mthd = "apply");
      assert (List.length args = 0);
      let tdenv, _, _, sto = caller_scope in
      let caller_scope = (tdenv, genv, lenv, sto) in
      eval_table_call caller_scope keys actions default
  | _ -> failwith "(TODO: eval_method_call)"
