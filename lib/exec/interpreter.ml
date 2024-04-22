open Syntax
open Syntax.Ast
open Runtime
open Runtime.Domain
open Utils
open Utils.Scope

(* Store *)

let ienv = ref Env.empty

let register_store (_ienv : ienv) =
  print_endline "Registering store";
  ienv := _ienv

(* Block Environment *)

type benv = env * env * tsto * vsto

(* Utils *)

let print_benv (benv : benv) =
  let cenv, lenv, tsto, vsto = benv in
  print_string "\tGlobal environment: ";
  Runtime.Pretty.print_env cenv |> print_endline;
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

(* Scoping *)

let find_value (genv : env) (lenv : env) (vsto : vsto) (name : string) =
  match find name lenv vsto with
  | Some value -> Some value
  | None -> find name genv vsto

let find_value_toplevel (genv : env) (vsto : vsto) (name : string) =
  find name genv vsto

let find_typ (genv : env) (lenv : env) (tsto : tsto) (name : string) =
  match find name genv tsto with
  | Some value -> Some value
  | None -> find name lenv tsto

let update_value (lenv : env) (vsto : vsto) (name : string) (value : value) =
  update name value lenv vsto

let add_known = Scope.add_double

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

(* Expression evaluation *)

let rec eval_simplify_typ (tdenv : tdenv) (typ : typ) : typ =
  match typ with
  | TName { name } -> eval_simplify_typ tdenv (Env.find name tdenv)
  | TNewType { name } -> Env.find name tdenv
  | _ -> typ

let rec eval_typ (tdenv : tdenv) (benv : benv) (typ : Type.t) : typ =
  match typ with
  | Bool _ -> TBool
  | Integer _ -> TAInt
  | IntType { expr; _ } ->
      let width = eval_expr tdenv benv expr |> Ops.extract_bigint in
      TBit { width }
  | BitType { expr; _ } ->
      let width = eval_expr tdenv benv expr |> Ops.extract_bigint in
      TBit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr tdenv benv expr |> Ops.extract_bigint in
      TBit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ tdenv benv header in
      let size = eval_expr tdenv benv size |> Ops.extract_bigint in
      TArray { typ = header; size }
  | String _ -> TString
  | Error _ -> TError
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ tdenv benv) args in
      TTuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Env.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ tdenv benv base
  | _ -> Printf.sprintf "(TODO: eval_typ) %s" "TODO" |> failwith

and eval_expr (tdenv : tdenv) (benv : benv) (expr : Expression.t) : value =
  let cenv, lenv, _tsto, vsto = benv in
  match expr with
  | True _ -> VBool true
  | False _ -> VBool false
  | Int { i; _ } -> (
      let value = i.value in
      match i.width_signed with
      | Some (width, signed) ->
          if signed then VInt { value; width } else VBit { value; width }
      | None -> VAInt value)
  | String { text; _ } -> VString text.str
  | Name { name = BareName name; _ } ->
      let name = name.str in
      find_value cenv lenv vsto name |> Option.get
  | Name { name = QualifiedName ([], name); _ } ->
      let name = name.str in
      find_value_toplevel cenv vsto name |> Option.get
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr tdenv benv bits in
      let vlo = eval_expr tdenv benv lo in
      let vhi = eval_expr tdenv benv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr tdenv benv) values in
      VTuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr tdenv benv entry.value in
            (key, value))
          entries
      in
      VStruct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr tdenv benv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr tdenv benv arg_fst in
      let varg_snd = eval_expr tdenv benv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ tdenv benv typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr tdenv benv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr tdenv benv expr in
      let name = name.str in
      match vexpr with
      | VHeader { entries; _ } | VStruct { entries } -> List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" "TODO" |> failwith)
  | _ -> Printf.sprintf "(TODO: eval_expr) %s" "TODO" |> failwith

(* Statement evaluation *)

let rec eval_stmt (tdenv : tdenv) (benv : benv) (stmt : Statement.t) : benv =
  let cenv, lenv, tsto, vsto = benv in
  match stmt with
  (* (TODO) Perform implicit casts on assignment. *)
  | Assignment
      { lhs = Expression.Name { name = Name.BareName text; _ }; rhs; _ } ->
      let name = text.str in
      let typ = find_typ cenv lenv tsto name |> Option.get in
      let value = eval_expr tdenv benv rhs |> Ops.eval_cast typ in
      let vsto = update_value lenv vsto name value in
      (cenv, lenv, tsto, vsto)
  | Conditional { cond; tru; fls = Some fls; _ } ->
      let vcond = eval_expr tdenv benv cond |> Ops.eval_cast TBool in
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
  let _benv = List.fold_left (eval_stmt tdenv) benv stmts in
  benv

(* Declaration evaluation *)

and eval_decl (tdenv : tdenv) (benv : benv) (decl : Declaration.t) : benv =
  let cenv, lenv, tsto, vsto = benv in
  match decl with
  | Variable { name; typ; init; _ } ->
      let name = name.str in
      let typ = eval_typ tdenv benv typ in
      let value =
        match init with
        | Some value -> eval_expr tdenv benv value
        | None -> default_value typ
      in
      let lenv, tsto, vsto = add_known name typ value lenv tsto vsto in
      (cenv, lenv, tsto, vsto)
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
    let cenv, lenv, tsto, vsto = callee_env in
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
        let _typ = eval_typ tdenv caller_env typ in
        let value = eval_expr tdenv caller_env arg in
        let vsto = update_value lenv vsto param value in
        (cenv, lenv, tsto, vsto)
    (* Direction out parameters are always initialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This defaultialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let param, typ = (param.variable.str, param.typ) in
        let typ = eval_typ tdenv caller_env typ in
        let value = default_value typ in
        let vsto = update_value lenv vsto param value in
        (cenv, lenv, tsto, vsto)
    (* inout parameters behave like a combination of in and out
       parameters simultaneously. *)
    | Some (InOut _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO) (copyin) Support missing argument."
        in
        let _typ = eval_typ tdenv caller_env typ in
        let value = eval_expr tdenv caller_env arg in
        let vsto = update_value lenv vsto param value in
        (cenv, lenv, tsto, vsto)
  in
  List.fold_left2 copyin' callee_env params args

let copyout (_tdenv : tdenv) (caller_env : benv) (callee_env : benv)
    (params : Parameter.t list) (args : Argument.t list) : benv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Write a value to the l-value argument. *)
  let write (arg : Expression.t) (value : value) (benv : benv) =
    let cenv, lenv, tsto, vsto = benv in
    match arg with
    | Name { name = BareName text; _ } ->
        let var = text.str in
        let vsto = update_value lenv vsto var value in
        (cenv, lenv, tsto, vsto)
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
        let _, lenv_callee, _, vsto_callee = callee_env in
        let value =
          find_value Env.empty lenv_callee vsto_callee param |> Option.get
        in
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
