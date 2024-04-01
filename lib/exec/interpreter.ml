open Syntax
open Ast
open Runtime
open Value
open Typ
open Envs

(* (TODO) expression evaluation relies on the compile-time evaluation
   in Runtime.Eval, which is not ideal. *)
(* (TODO) register the store as a global referenced variable in OCaml *)

(* Environments *)

type store = Store.t

let gstore = ref Store.empty
let register_store (store : store) = gstore := store

(* Utils *)

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

(* Interpreter *)

let rec eval_simplify_typ (tdenv : tdenv) (typ : Typ.t) : Typ.t =
  match typ with
  | Typ.Name { name } -> eval_simplify_typ tdenv (TDEnv.find name tdenv)
  | Typ.NewType { name } -> TDEnv.find name tdenv
  | _ -> typ

let rec eval_typ (env : env) (tdenv : tdenv) (typ : Type.t) : Typ.t =
  match typ with
  | Bool _ -> Typ.Bool
  | Integer _ -> Typ.AInt
  | IntType { expr; _ } ->
      let width = eval_expr env tdenv expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | BitType { expr; _ } ->
      let width = eval_expr env tdenv expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr env tdenv expr |> Ops.extract_bigint in
      Typ.Bit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ env tdenv header in
      let size = eval_expr env tdenv size |> Ops.extract_bigint in
      Typ.Array { typ = header; size }
  | String _ -> Typ.String
  | Error _ -> Typ.Error
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ env tdenv) args in
      Typ.Tuple vargs
  | TypeName { name = BareName text; _ } ->
      let var = text.str in
      TDEnv.find var tdenv
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      TDEnv.find var tdenv
  | _ ->
      Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

and eval_expr (env : env) (tdenv : tdenv) (expr : Expression.t) : Value.t =
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
  | Name { name = BareName text; _ } ->
      let var = text.str in
      Env.find var env
  | Name { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Env.find_toplevel var env
  | BitStringAccess { bits; lo; hi; _ } ->
      let vbits = eval_expr env tdenv bits in
      let vlo = eval_expr env tdenv lo in
      let vhi = eval_expr env tdenv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr env tdenv) values in
      Value.Tuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr env tdenv entry.value in
            (key, value))
          entries
      in
      Value.Struct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr env tdenv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr env tdenv arg_fst in
      let varg_snd = eval_expr env tdenv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ env tdenv typ in
      let typ = eval_simplify_typ tdenv typ in
      let vexpr = eval_expr env tdenv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr env tdenv expr in
      let name = name.str in
      match vexpr with
      | Value.Header { entries; _ } | Value.Struct { entries } ->
          List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" (Value.print vexpr)
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith

let eval_decl (env : env) (tenv : tenv) (tdenv : tdenv) (decl : Declaration.t) :
    env * tenv =
  match decl with
  | Variable { name; typ; init = Some value; _ } ->
      let typ = eval_typ env tdenv typ in
      let value = eval_expr env tdenv value in
      let env = Env.add name.str value env in
      let tenv = TEnv.add name.str typ tenv in
      (env, tenv)
  | Variable { name; typ; init = None; _ } ->
      let typ = eval_typ env tdenv typ in
      let value = default typ in
      let env = Env.add name.str value env in
      let tenv = TEnv.add name.str typ tenv in
      (env, tenv)
  | _ ->
      Printf.sprintf "(TODO: eval_decl) %s" (Pretty.print_decl 0 decl)
      |> print_endline;
      (env, tenv)

let rec eval_stmt (env : env) (tenv : tenv) (tdenv : tdenv) (stmt : Statement.t)
    : env * tenv =
  match stmt with
  (* (TODO) Perform implicit casts on assignment. *)
  | Assignment
      { lhs = Expression.Name { name = Name.BareName text; _ }; rhs; _ } ->
      let name = text.str in
      let typ = TEnv.find name tenv in
      let rvalue = eval_expr env tdenv rhs in
      let rvalue = Ops.eval_cast typ rvalue in
      let env = Env.update name rvalue env in
      (env, tenv)
  | Conditional { cond; tru; fls = Some fls; _ } ->
      let vcond = eval_expr env tdenv cond in
      let vcond = Ops.eval_cast Typ.Bool vcond in
      let env = Env.enter env in
      let tenv = TEnv.enter tenv in
      let stmts =
        match vcond with
        | Bool true -> tru
        | Bool false -> fls
        | _ -> assert false
      in
      let env, tenv = eval_stmt env tenv tdenv stmts in
      let tenv = TEnv.exit tenv in
      let env = Env.exit env in
      (env, tenv)
  | BlockStatement { block; _ } -> eval_block env tenv tdenv block
  | EmptyStatement _ -> (env, tenv)
  | DeclarationStatement { decl; _ } -> eval_decl env tenv tdenv decl
  | _ ->
      Printf.sprintf "(TODO: eval_stmt) %s" (Pretty.print_stmt 0 stmt)
      |> print_endline;
      (env, tenv)

and eval_block (env : env) (tenv : tenv) (tdenv : tdenv) (block : Block.t) :
    env * tenv =
  let env = Env.enter env in
  let tenv = TEnv.enter tenv in
  let env, tenv =
    List.fold_left
      (fun (env, tenv) stmt -> eval_stmt env tenv tdenv stmt)
      (env, tenv) block.statements
  in
  let tenv = TEnv.exit tenv in
  let env = Env.exit env in
  (env, tenv)

let copyin (caller_env : env) (callee_env : env) (callee_tenv : tenv)
    (tdenv : tdenv) (params : Parameter.t list) (args : Argument.t list) :
    env * tenv =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  (* It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. (8.20) *)
  assert (
    List.for_all
      (fun (arg : Argument.t) ->
        match arg with Expression _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : Argument.t) ->
           match arg with KeyValue _ -> true | _ -> false)
         args);
  (* Copy-in a single parameter-argument pair. *)
  let copyin' ((env, tenv) : env * tenv) (param : Parameter.t)
      (arg : Argument.t) =
    match param.direction with
    (* in parameters are defaultialized by copying the value of the
       corresponding argument when the invocation is executed.
       inout parameters behave like a combination of in and out
       parameters simultaneously. *)
    | Some (In _ | InOut _) ->
        let param, typ, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, param.typ, value)
          | KeyValue { key; value; _ } -> (key.str, param.typ, value)
          | _ -> failwith "(TODO) (copyin) Support missing argument."
        in
        let typ = eval_typ caller_env tdenv typ in
        let value = eval_expr caller_env tdenv arg in
        let env = Env.add param value env in
        let tenv = TEnv.add param typ tenv in
        (env, tenv)
    (* Direction out parameters are always defaultialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This defaultialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let param, typ = (param.variable.str, param.typ) in
        let typ = eval_typ caller_env tdenv typ in
        let value = default typ in
        let env = Env.add param value env in
        let tenv = TEnv.add param typ tenv in
        (env, tenv)
    | None ->
        failwith "(TODO) (copyin) Non-directional parameter is not supported."
  in
  List.fold_left2 copyin' (callee_env, callee_tenv) params args

let copyout (caller_env : env) (callee_env : env) (_tdenv : tdenv)
    (params : Parameter.t list) (args : Argument.t list) : env =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  (* It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. (8.20) *)
  assert (
    List.for_all
      (fun (arg : Argument.t) ->
        match arg with Expression _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : Argument.t) ->
           match arg with KeyValue _ -> true | _ -> false)
         args);
  (* Write a value to the l-value argument. *)
  let write (arg : Expression.t) (value : Value.t) (env : env) =
    match arg with
    | Name { name = BareName text; _ } ->
        let var = text.str in
        Env.update var value env
    | Name { name = QualifiedName ([], text); _ } ->
        let var = text.str in
        Env.update_toplevel var value env
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
        let value = Env.find param callee_env in
        write arg value env
    | Some (In _) -> env
    | None ->
        failwith "(TODO) (copyout) Non-directional parameter is not supported."
  in
  List.fold_left2 copyout' caller_env params args

let eval_object_apply (caller_env : env) (obj : Object.t)
    (args : Argument.t list) =
  print_endline "Called eval_object_apply on object:";
  Object.print obj |> print_endline;
  "with arguments: " ^ String.concat ", " (List.map Pretty.print_arg args)
  |> print_endline;
  print_endline "and with environment:";
  Env.print caller_env |> print_endline;
  match obj with
  | Control { env; tenv; tdenv; params; apply; _ } ->
      let callee_env = env in
      let callee_tenv = tenv in
      let callee_env, callee_tenv =
        copyin caller_env callee_env callee_tenv tdenv params args
      in
      let callee_env, _ = eval_block callee_env callee_tenv tdenv apply in
      let caller_env = copyout caller_env callee_env tdenv params args in
      print_endline "After eval_object_apply";
      Env.print caller_env |> print_endline;
      ()
  | _ ->
      Printf.sprintf "(TODO: eval_object) %s" (Object.print obj)
      |> print_endline

(* (TODO) isn't store enough to evaluate? why need a program? *)
let eval_program (_program : program) =
  print_endline "(TODO: interpreter)";
  Printf.sprintf "Instantiation results in this store:\n%s"
    (Store.print !gstore ~indent:1)
  |> print_endline;
  let main = Store.find [ "main" ] !gstore in
  Printf.sprintf "main object:\n%s" (Object.print main ~indent:1)
  |> print_endline
