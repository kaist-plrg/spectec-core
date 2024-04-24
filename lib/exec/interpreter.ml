open Syntax
open Syntax.Ast
open Runtime
open Runtime.Domain
open Runtime.Scope

(* Store *)

let ienv = ref Env.empty

let register_store (_ienv : ienv) =
  print_endline "Registering store";
  ienv := _ienv

(* Utils *)

let print_benv (benv : benv) =
  let genv, lenv, tsto, vsto = benv in
  print_string "\tGlobal environment: ";
  Runtime.Pretty.print_env genv |> print_endline;
  print_string "\tLocal environment: ";
  Runtime.Pretty.print_env lenv |> print_endline;
  print_string "\tType store: ";
  Runtime.Pretty.print_tsto tsto |> print_endline;
  print_string "\tValue store: ";
  Runtime.Pretty.print_vsto vsto |> print_endline

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

let extract_ref (value : value) : string =
  match value with VRef ref -> String.concat "." ref | _ -> assert false

let rec default_value (typ : typ) : value =
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
      Printf.sprintf "(TODO) default_value: not implemented for %s" "TODO"
      |> failwith

(* Statement evaluation *)

(* lvalue
   : prefixedNonTypeName | lvalue "." member
   | lvalue "[" expression "]" | lvalue "[" expression ":" expression "]" *)

let rec eval_write (tdenv : tdenv) (benv : benv) (arg : Expression.t)
    (value : value) : benv =
  match arg with
  | Name { name = BareName text; _ } ->
      let var = text.str in
      let typ, _ = find_var var benv in
      let value = Ops.eval_cast typ value in
      let benv = update_value var value benv in
      benv
  (* (TODO) Ugly hack to get things done *)
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = Eval.eval_expr tdenv benv expr in
      let name = name.str in
      match vexpr with
      | VStruct { entries } ->
          let entries =
            List.map
              (fun (k, v) -> if k = name then (k, value) else (k, v))
              entries
          in
          let vexpr = VStruct { entries } in
          let benv = eval_write tdenv benv expr vexpr in
          benv
      | _ ->
          Printf.sprintf "(TODO: eval_write) Write to l-value %s"
            (Runtime.Pretty.print_value vexpr)
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_write) Write to l-value %s"
        (Syntax.Debug.debug_expr arg)
      |> failwith

let rec eval_stmt (tdenv : tdenv) (benv : benv) (stmt : Statement.t) : benv =
  match stmt with
  | MethodCall { func; args; type_args = targs; _ } -> (
      match func with
      | ExpressionMember { expr; name; _ } ->
          let ref = Eval.eval_expr tdenv benv expr |> extract_ref in
          eval_method_call tdenv benv ref name.str args targs
      | _ -> failwith "(TODO: eval_stmt) MethodCall on non-ExpressionMember.")
  | Assignment { lhs; rhs; _ } ->
      let value = Eval.eval_expr tdenv benv rhs in
      eval_write tdenv benv lhs value
  | Conditional { cond; tru; fls = Some fls; _ } ->
      let vcond = Eval.eval_expr tdenv benv cond |> Ops.eval_cast TBool in
      let body =
        match vcond with
        | VBool true -> tru
        | VBool false -> fls
        | _ -> assert false
      in
      eval_stmt tdenv benv body
  | BlockStatement { block; _ } -> eval_block tdenv benv block.statements
  | EmptyStatement _ -> benv
  | DeclarationStatement { decl; _ } -> eval_decl tdenv benv decl
  | _ ->
      Printf.sprintf "(TODO: eval_stmt) %s"
        (Syntax.Pretty.print_stmt 0 stmt |> Utils.Print.print_inline)
      |> print_endline;
      benv

(* Block evaluation *)

and eval_block (tdenv : tdenv) (benv : benv) (stmts : Statement.t list) : benv =
  let genv, lenv, _, _ = benv in
  let _, _, tsto, vsto = List.fold_left (eval_stmt tdenv) benv stmts in
  (genv, lenv, tsto, vsto)

(* State evaluation *)
(* (TODO) Each states having a scope of functions would be desirable. *)

and eval_state (tdenv : tdenv) (benv : benv) (stmts : Statement.t list) : benv =
  let genv, lenv, _, _ = benv in
  let _, _, tsto, vsto = List.fold_left (eval_stmt tdenv) benv stmts in
  (genv, lenv, tsto, vsto)

and eval_state_transition (tdenv : tdenv) (benv : benv)
    (stmts : Statement.t list) (transition : Parser.transition)
    (funcs : func list) : benv =
  let benv = eval_state tdenv benv stmts in
  let next =
    match transition with
    | Direct { next; _ } -> next
    | Select { exprs; cases; _ } ->
        let vexprs =
          List.map (fun expr -> Eval.eval_expr tdenv benv expr) exprs
        in
        let find_match (mtch : Text.t option) (case : Parser.case) =
          let matchcases = case.matches in
          let next = case.next in
          let find_match' (mtch : Text.t option) (vexpr : value)
              (matchcase : Match.t) =
            match mtch with
            | Some _ -> mtch
            | None -> (
                match matchcase with
                | Default _ | DontCare _ -> Some case.next
                | Expression { expr; _ } -> (
                    let vmatch = Eval.eval_expr tdenv benv expr in
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
  | "accept" | "reject" -> benv
  | next -> (
      let find_state (func : string) = function
        | FParser { name; _ } when name = func -> true
        | _ -> false
      in
      let func = List.find (find_state next) funcs in
      match func with
      | FParser { genv; lenv; body; transition; _ } ->
          let _, _, tsto, vsto = benv in
          eval_state_transition tdenv (genv, lenv, tsto, vsto) body transition
            funcs
      | _ -> assert false)

(* Declaration evaluation *)

and eval_decl (tdenv : tdenv) (benv : benv) (decl : Declaration.t) : benv =
  let genv, lenv, tsto, vsto = benv in
  match decl with
  | Variable { name; typ; init; _ } ->
      let name = name.str in
      let typ = Eval.eval_typ tdenv benv typ in
      let value =
        match init with
        | Some value -> Eval.eval_expr tdenv benv value
        | None -> default_value typ
      in
      let lenv, tsto, vsto = add_var name typ value lenv tsto vsto in
      (genv, lenv, tsto, vsto)
  | _ ->
      Printf.sprintf "(TODO: eval_decl) %s" "TODO" |> print_endline;
      benv

(* Calling convention: Copy-in/out *)

and copyin (preallocated : bool) (tdenv : tdenv) (caller_env : benv)
    (callee_env : benv) (params : Parameter.t list) (args : Argument.t list) :
    benv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Copy-in a single parameter-argument pair *)
  let copyin_single (callee_env : benv) (param : Parameter.t) (arg : Argument.t)
      =
    match param.direction with
    (* in parameters are initialized by copying the value of the
       corresponding argument when the invocation is executed. *)
    | None | Some (In _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO) (copyin) Support missing argument."
        in
        let typ = Eval.eval_typ tdenv caller_env typ in
        let value = Eval.eval_expr tdenv caller_env arg in
        (* (TODO) A problem arises because the instantiation phase statically allocates
           parameters to parser/control blocks but not for extern objects *)
        let benv =
          (* In the case of parser/control apply *)
          if preallocated then update_value param value callee_env
          else
            let genv, lenv, tsto, vsto = callee_env in
            let lenv, tsto, vsto = add_var param typ value lenv tsto vsto in
            (genv, lenv, tsto, vsto)
        in
        benv
    (* Direction out parameters are always initialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This defaultialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let param, typ = (param.variable.str, param.typ) in
        let typ = Eval.eval_typ tdenv caller_env typ in
        let value = default_value typ in
        (* (TODO) A problem arises because the instantiation phase statically allocates
           parameters to parser/control blocks but not for extern objects *)
        let benv =
          (* In the case of parser/control apply *)
          if preallocated then update_value param value callee_env
          else
            let genv, lenv, tsto, vsto = callee_env in
            let lenv, tsto, vsto = add_var param typ value lenv tsto vsto in
            (genv, lenv, tsto, vsto)
        in
        benv
    (* inout parameters behave like a combination of in and out
       parameters simultaneously. *)
    | Some (InOut _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO) (copyin) Support missing argument."
        in
        let typ = Eval.eval_typ tdenv caller_env typ in
        let value = Eval.eval_expr tdenv caller_env arg in
        (* (TODO) A problem arises because the instantiation phase statically allocates
           parameters to parser/control blocks but not for extern objects *)
        let benv =
          (* In the case of parser/control apply *)
          if preallocated then update_value param value callee_env
          else
            let genv, lenv, tsto, vsto = callee_env in
            let lenv, tsto, vsto = add_var param typ value lenv tsto vsto in
            (genv, lenv, tsto, vsto)
        in
        benv
  in
  List.fold_left2 copyin_single callee_env params args

and copyout (tdenv : tdenv) (caller_env : benv) (callee_env : benv)
    (params : Parameter.t list) (args : Argument.t list) : benv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Copy-out a single parameter-argument pair. *)
  let copyout_single (benv : benv) (param : Parameter.t) (arg : Argument.t) =
    match param.direction with
    | Some (InOut _ | Out _) ->
        let param, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, value)
          | KeyValue { key; value; _ } -> (key.str, value)
          | _ -> failwith "(TODO) (copyout) Support missing argument."
        in
        let _, value = find_var param callee_env in
        eval_write tdenv benv arg value
    | _ -> benv
  in
  List.fold_left2 copyout_single caller_env params args

(* Entry *)

and eval_method_call (caller_tdenv : tdenv) (caller_env : benv) (ref : string)
    (mthd : string) (args : Argument.t list) (targs : Type.t list) =
  (* Retrieve the object from the store. *)
  let obj = Env.find ref !ienv in
  let tdenv, tsto, vsto, funcs =
    match obj with
    | OParser { tdenv; tsto; vsto; funcs } -> (tdenv, tsto, vsto, funcs)
    | OControl { tdenv; tsto; vsto; funcs } -> (tdenv, tsto, vsto, funcs)
    | OExtern { tdenv; tsto; vsto; funcs; _ } -> (tdenv, tsto, vsto, funcs)
    | _ -> failwith "(TODO) eval_method_call"
  in
  (* Find the method in the object. *)
  let find_func = function
    | FParser { name; _ } when name = mthd -> true
    | FNormal { name; _ } when name = mthd -> true
    | FExtern { name; _ } when name = mthd -> true
    | _ -> false
  in
  let func = List.find find_func funcs in
  (* Evaluate the method. *)
  match func with
  | FParser { params; genv; lenv; body; transition; _ } ->
      let callee_env = (genv, lenv, tsto, vsto) in
      let callee_env = copyin true tdenv caller_env callee_env params args in
      let callee_env =
        eval_state_transition tdenv callee_env body transition funcs
      in
      let caller_env = copyout tdenv caller_env callee_env params args in
      caller_env
  | FNormal { params; genv; lenv; body; _ } ->
      let callee_env = (genv, lenv, tsto, vsto) in
      let callee_env = copyin true tdenv caller_env callee_env params args in
      let callee_env = eval_block tdenv callee_env body in
      let caller_env = copyout tdenv caller_env callee_env params args in
      caller_env
  | FExtern { tparams; params; genv; _ } ->
      let tdenv = Eval.eval_targs caller_tdenv tdenv caller_env tparams targs in
      let callee_env = (genv, Env.empty, tsto, vsto) in
      let callee_env = copyin false tdenv caller_env callee_env params args in
      let callee_env = Core.eval_builtin callee_env mthd in
      let caller_env = copyout tdenv caller_env callee_env params args in
      caller_env
