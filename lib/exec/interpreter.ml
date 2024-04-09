open Syntax
open Ast
open Runtime
open Func
open Envs
open Store

(* Store *)

type store = GSto.t

let gsto = ref GSto.empty
let register_store (store : store) = gsto := store

(* Environment *)

type env = CEnv.t * LEnv.t * TSto.t * VSto.t
type tdenv = TDEnv.t

let enter (env : env) : env =
  let cenv, lenv, tsto, vsto = env in
  (CEnv.enter cenv, LEnv.enter lenv, TSto.enter tsto, VSto.enter vsto)

let exit (env : env) : env =
  let cenv, lenv, tsto, vsto = env in
  (CEnv.exit cenv, LEnv.exit lenv, TSto.exit tsto, VSto.exit vsto)

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

let rec default (typ : Typ.t) : Value.t =
  match typ with
  | Bool -> Bool false
  | AInt -> AInt Bigint.zero
  | Int { width } -> Int { value = Bigint.zero; width }
  | Bit { width } -> Bit { value = Bigint.zero; width }
  | String -> String ""
  | Tuple types -> Tuple (List.map default types)
  | Struct { entries } ->
      let entries = List.map (fun (name, typ) -> (name, default typ)) entries in
      Struct { entries }
  | Header { entries } ->
      let entries = List.map (fun (name, typ) -> (name, default typ)) entries in
      Header { valid = false; entries }
  | _ -> failwith "(TODO) default: not implemented"

(* Expression evaluation *)

let rec eval_simplify_typ (tdenv : tdenv) (typ : Typ.t) : Typ.t =
  match typ with
  | Typ.Name { name } -> eval_simplify_typ tdenv (TDEnv.find name tdenv)
  | Typ.NewType { name } -> TDEnv.find name tdenv
  | _ -> typ

let rec eval_typ (tdenv : tdenv) (env : env) (typ : Type.t) :
    Typ.t =
  match typ with
  | Bool _ -> Typ.Bool
  | Integer _ -> Typ.AInt
  | IntType { expr; _ } ->
      let width = eval_expr tdenv env expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | BitType { expr; _ } ->
      let width = eval_expr tdenv env expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr tdenv env expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ tdenv env header in
      let size = eval_expr tdenv env size |> Ops.extract_bigint in
      Typ.Array { typ = header; size }
  | String _ -> Typ.String
  | Error _ -> Typ.Error
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ tdenv env) args in
      Typ.Tuple vargs
  | TypeName { name = BareName text; _ }
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      TDEnv.find var tdenv
  (* (TODO) handle specialized types *)
  | SpecializedType { base; _ } -> eval_typ tdenv env base
  | _ ->
      Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

and eval_expr (tdenv : tdenv) (env : env) (expr : Expression.t)
    : Value.t =
  let cenv, lenv, _tsto, vsto = env in
  match expr with
  | True _ -> Value.Bool true
  | False _ -> Value.Bool false
  | Int { i; _ } -> (
      let value = i.value in
      match i.width_signed with
      | Some (width, signed) ->
          if signed then Value.Int { value; width }
          else Value.Bit { value; width }
      | None -> Value.AInt value)
  | String { text; _ } -> Value.String text.str
  | Name { name = BareName name; _ } ->
      let name = name.str in
      find_value cenv lenv vsto name
  | Name { name = QualifiedName ([], name); _ } ->
      let name = name.str in
      find_value_toplevel cenv lenv vsto name
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr tdenv env bits in
      let vlo = eval_expr tdenv env lo in
      let vhi = eval_expr tdenv env hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr tdenv env) values in
      Value.Tuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr tdenv env entry.value in
            (key, value))
          entries
      in
      Value.Struct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr tdenv env arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr tdenv env arg_fst in
      let varg_snd = eval_expr tdenv env arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ tdenv env typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr tdenv env expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr tdenv env expr in
      let name = name.str in
      match vexpr with
      | Value.Header { entries; _ } | Value.Struct { entries } ->
          List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" (Value.print vexpr)
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith

(* Statement evaluation *)

let print_env (env : env) =
  let cenv, lenv, tsto, vsto = env in
  Printf.sprintf "cenv:\n%s\nlenv:\n%s\ntsto:\n%s\nvsto:\n%s"
    (CEnv.print cenv) (LEnv.print lenv) (TSto.print tsto) (VSto.print vsto)
  |> print_endline

let rec eval_stmt (tdenv : tdenv) (env : env) (stmt : Statement.t) : env =
  let cenv, lenv, tsto, vsto = env in
  match stmt with
  (* (TODO) Perform implicit casts on assignment. *)
  | Assignment
      { lhs = Expression.Name { name = Name.BareName text; _ }; rhs; _ } ->
      let name = text.str in
      let typ = find_typ cenv lenv tsto name in
      let value = eval_expr tdenv env rhs |> Ops.eval_cast typ in
      let vsto = update_var lenv vsto name value in
      (cenv, lenv, tsto, vsto)
  | Conditional { cond; tru; fls = Some fls; _ } ->
      let vcond = eval_expr tdenv env cond |> Ops.eval_cast Typ.Bool in
      let body =
        match vcond with
        | Bool true -> tru
        | Bool false -> fls
        | _ -> assert false
      in
      eval_stmt tdenv env body 
  | BlockStatement { block; _ } -> eval_block tdenv env block.statements
  | EmptyStatement _ -> env 
  | DeclarationStatement { decl; _ } -> eval_decl tdenv env decl
  | _ ->
      Printf.sprintf "(TODO: eval_stmt) %s" (Pretty.print_stmt 0 stmt)
      |> print_endline;
      env

(* Block evaluation *)

and eval_block (tdenv : tdenv) (env : env) (stmts : Statement.t list) : env =
  let env = enter env in
  let env = List.fold_left (eval_stmt tdenv) env stmts in
  exit env

(* Declaration evaluation *)

and eval_decl (tdenv : tdenv) (env : env) (decl : Declaration.t) : env =
  let cenv, lenv, tsto, vsto = env in
  match decl with
  | Variable { name; typ; init; _ } ->
      let name = name.str in
      let typ = eval_typ tdenv env typ in
      let value =
        match init with
        | Some value -> eval_expr tdenv env value
        | None -> default typ
      in
      let lenv, tsto, vsto = add_var lenv tsto vsto name typ value in
      (cenv, lenv, tsto, vsto)
  | _ ->
      Printf.sprintf "(TODO: eval_decl) %s" (Pretty.print_decl 0 decl)
      |> print_endline;
      env

(* Calling convention: Copy-in/out *)

let copyin (tdenv : tdenv) (caller_env : env) (callee_env : env)
    (params : Parameter.t list) (args : Argument.t list) :
    env =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  (* It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. (8.20) *)
  check_args args;
  (* Copy-in a single parameter-argument pair *)
  let copyin' (callee_env : env) (param : Parameter.t)
      (arg : Argument.t) =
    let cenv, lenv, tsto, vsto = callee_env in
    match param.direction with
    (* in parameters are defaultialized by copying the value of the
       corresponding argument when the invocation is executed. *)
    | Some (In _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO) (copyin) Support missing argument."
        in
        let typ = eval_typ tdenv caller_env typ in
        let value = eval_expr tdenv caller_env arg in
        let cenv, tsto, vsto = load_const cenv tsto vsto param typ value in
        (cenv, lenv, tsto, vsto)
    (* Direction out parameters are always defaultialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This defaultialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let param, typ = (param.variable.str, param.typ) in
        let typ = eval_typ tdenv caller_env typ in
        let value = default typ in
        let lenv, tsto, vsto = add_var lenv tsto vsto param typ value in
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
        let typ = eval_typ tdenv caller_env typ in
        let value = eval_expr tdenv caller_env arg in
        let lenv, tsto, vsto = add_var lenv tsto vsto param typ value in
        (cenv, lenv, tsto, vsto)
    | None ->
        failwith "(TODO) (copyin) Non-directional parameter is not supported."
  in
  List.fold_left2 copyin' callee_env params args

let copyout (_tdenv : tdenv) (caller_env : env) (callee_env : env)
    (params : Parameter.t list) (args : Argument.t list) : env =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Write a value to the l-value argument. *)
  let write (arg : Expression.t) (value : Value.t) (env : env) =
    let cenv, lenv, tsto, vsto = env in
    match arg with
    | Name { name = BareName text; _ } ->
        let var = text.str in
        let vsto = update_var lenv vsto var value in
        (cenv, lenv, tsto, vsto)
    | _ ->
        Printf.sprintf "(TODO) (copyout) Write to l-value %s"
          (Pretty.print_expr arg)
        |> failwith
  in
  (* Copy-out a single parameter-argument pair. *)
  let copyout' (env : env) (param : Parameter.t) (arg : Argument.t) =
    match param.direction with
    | Some (InOut _ | Out _) ->
        let param, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, value)
          | KeyValue { key; value; _ } -> (key.str, value)
          | _ -> failwith "(TODO) (copyout) Support missing argument."
        in
        let _, lenv_callee, _, vsto_callee = callee_env in
        let value = find_var lenv_callee vsto_callee param in
        write arg value env
    | Some (In _) -> env
    | None ->
        failwith "(TODO) (copyout) Non-directional parameter is not supported."
  in
  List.fold_left2 copyout' caller_env params args

(* Entry *)

let eval_method_call (caller_env : env) (obj : Object.t) (mthd : string) (args : Argument.t list) =
  match obj with
  | Control { tdenv; tsto; vsto; funcs } ->
    let func =
      List.find
        (fun func -> match func with
          | Control { name; _ } when name = mthd -> true
          | _ -> false)
        funcs
    in
    let params, cenv, lenv, body =
      match func with
      | Control { params; cenv; lenv; body; _ } -> params, cenv, lenv, body
      | _ -> assert false
    in
    let callee_env = (cenv, lenv, tsto, vsto) in
    let callee_env = copyin tdenv caller_env callee_env params args in
    let callee_env = eval_block tdenv callee_env body in
    let caller_env = copyout tdenv caller_env callee_env params args in
    caller_env
  | _ -> failwith "(TODO) eval_method"
