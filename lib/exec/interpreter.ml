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

let rec eval_stmt (tdenv : tdenv) (benv : benv) (stmt : Statement.t) : benv =
  match stmt with
  (* (TODO) Perform implicit casts on assignment. *)
  | Assignment
      { lhs = Expression.Name { name = Name.BareName text; _ }; rhs; _ } ->
      let name = text.str in
      let typ, _ = find_var name benv in
      let value = Eval.eval_expr tdenv benv rhs |> Ops.eval_cast typ in
      let benv = update_value name value benv in
      benv
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
      Printf.sprintf "(TODO: eval_stmt) %s" "TODO" |> print_endline;
      benv

(* Block evaluation *)

and eval_block (tdenv : tdenv) (benv : benv) (stmts : Statement.t list) : benv =
  let genv, lenv, _, _ = benv in
  let _, _, tsto, vsto = List.fold_left (eval_stmt tdenv) benv stmts in
  (genv, lenv, tsto, vsto)

(* State evaluation *)

and eval_state (tdenv : tdenv) (benv : benv) (stmts : Statement.t list)
    (_transition : Parser.transition) : benv =
  let genv, lenv, _, _ = benv in
  let _, _, tsto, vsto = List.fold_left (eval_stmt tdenv) benv stmts in
  (genv, lenv, tsto, vsto)

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

let copyin (tdenv : tdenv) (caller_env : benv) (callee_env : benv)
    (params : Parameter.t list) (args : Argument.t list) : benv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Copy-in a single parameter-argument pair *)
  let copyin' (callee_env : benv) (param : Parameter.t) (arg : Argument.t) =
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
        let _typ = Eval.eval_typ tdenv caller_env typ in
        let value = Eval.eval_expr tdenv caller_env arg in
        let benv = update_value param value callee_env in
        benv
    (* Direction out parameters are always initialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This defaultialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let param, typ = (param.variable.str, param.typ) in
        let typ = Eval.eval_typ tdenv caller_env typ in
        let value = default_value typ in
        let benv = update_value param value callee_env in
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
        let _typ = Eval.eval_typ tdenv caller_env typ in
        let value = Eval.eval_expr tdenv caller_env arg in
        let benv = update_value param value callee_env in
        benv
  in
  List.fold_left2 copyin' callee_env params args

let copyout (_tdenv : tdenv) (caller_env : benv) (callee_env : benv)
    (params : Parameter.t list) (args : Argument.t list) : benv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Write a value to the l-value argument. *)
  let write (arg : Expression.t) (value : value) (benv : benv) =
    match arg with
    | Name { name = BareName text; _ } ->
        let var = text.str in
        let benv = update_value var value benv in
        benv
    | _ ->
        Printf.sprintf "(TODO) (copyout) Write to l-value %s" "TODO" |> failwith
  in
  (* Copy-out a single parameter-argument pair. *)
  let copyout' (benv : benv) (param : Parameter.t) (arg : Argument.t) =
    match param.direction with
    | Some (InOut _ | Out _) ->
        let param, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, value)
          | KeyValue { key; value; _ } -> (key.str, value)
          | _ -> failwith "(TODO) (copyout) Support missing argument."
        in
        let _, value = find_var param callee_env in
        write arg value benv
    | _ -> benv
  in
  List.fold_left2 copyout' caller_env params args

(* Entry *)

let eval_method_call (caller_env : benv) (obj : obj) (mthd : string)
    (args : Argument.t list) =
  let find_parser_func (func : string) = function
    | FParser { name; _ } when name = func -> true
    | _ -> false
  in
  let find_normal_func (func : string) = function
    | FNormal { name; _ } when name = func -> true
    | _ -> false
  in
  match obj with
  | OParser { tdenv; tsto; vsto; funcs } ->
      let func = List.find (find_parser_func mthd) funcs in
      let params, genv, lenv, body, transition =
        match func with
        | FParser { params; genv; lenv; body; transition; _ } ->
            (params, genv, lenv, body, transition)
        | _ -> assert false
      in
      let callee_env = (genv, lenv, tsto, vsto) in
      let callee_env = copyin tdenv caller_env callee_env params args in
      let callee_env = eval_state tdenv callee_env body transition in
      let caller_env = copyout tdenv caller_env callee_env params args in
      caller_env
  | OControl { tdenv; tsto; vsto; funcs } ->
      let func = List.find (find_normal_func mthd) funcs in
      let params, genv, lenv, body =
        match func with
        | FNormal { params; genv; lenv; body; _ } -> (params, genv, lenv, body)
        | _ -> assert false
      in
      let callee_env = (genv, lenv, tsto, vsto) in
      let callee_env = copyin tdenv caller_env callee_env params args in
      let callee_env = eval_block tdenv callee_env body in
      let caller_env = copyout tdenv caller_env callee_env params args in
      caller_env
  | _ -> failwith "(TODO) eval_method"
