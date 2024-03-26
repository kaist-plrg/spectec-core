open Syntax
open Ast
open Runtime

(* (TODO) expression evaluation relies on the compile-time evaluation
   in Runtime.Eval, which is not ideal. *)
(* (TODO) register the store as a global referenced variable in OCaml *)

(* Environments *)

type env = Env.t
type tenv = Tenv.t
type store = Store.t

let gstore = ref Store.empty
let register_store (store : store) = gstore := store

(* Interpreter *)

let rec eval_simplify_typ (tenv : tenv) (typ : Typ.t) : Typ.t =
  match typ with
  | Typ.Name { name } -> eval_simplify_typ tenv (Tenv.find name tenv)
  | Typ.NewType { name } -> Tenv.find name tenv
  | _ -> typ

let rec eval_typ (env : env) (tenv : tenv) (typ : Type.t) : Typ.t =
  match typ with
  | Bool _ -> Typ.Bool
  | Integer _ -> Typ.AInt
  | IntType { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      Typ.Bit { width }
  | BitType { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      Typ.Bit { width }
  | VarBit { expr; _ } ->
      let width = eval_expr env tenv expr |> Value.extract_bigint in
      Typ.Bit { width }
  | HeaderStack { header; size; _ } ->
      let header = eval_typ env tenv header in
      let size = eval_expr env tenv size |> Value.extract_bigint in
      Typ.Array { typ = header; size }
  | String _ -> Typ.String
  | Error _ -> Typ.Error
  | Tuple { args; _ } ->
      let vargs = List.map (eval_typ env tenv) args in
      Typ.Tuple vargs
  | TypeName { name = BareName text; _ } ->
      let var = text.str in
      Tenv.find var tenv
  | TypeName { name = QualifiedName ([], text); _ } ->
      let var = text.str in
      Tenv.find_toplevel var tenv
  | _ ->
      Printf.sprintf "(TODO: eval_typ) %s" (Pretty.print_type typ) |> failwith

and eval_expr (env : env) (tenv : tenv) (expr : Expression.t) : Value.t =
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
      let vbits = eval_expr env tenv bits in
      let vlo = eval_expr env tenv lo in
      let vhi = eval_expr env tenv hi in
      Ops.eval_bitstring_access vbits vlo vhi
  | List { values; _ } ->
      let vvalues = List.map (eval_expr env tenv) values in
      Value.Tuple vvalues
  | Record { entries; _ } ->
      let ventries =
        List.map
          (fun (entry : KeyValue.t) ->
            let key = entry.key.str in
            let value = eval_expr env tenv entry.value in
            (key, value))
          entries
      in
      Value.Struct { entries = ventries }
  | UnaryOp { op; arg; _ } ->
      let varg = eval_expr env tenv arg in
      Ops.eval_unop op varg
  | BinaryOp { op; args = arg_fst, arg_snd; _ } ->
      let varg_fst = eval_expr env tenv arg_fst in
      let varg_snd = eval_expr env tenv arg_snd in
      Ops.eval_binop op varg_fst varg_snd
  | Cast { typ; expr; _ } ->
      let typ = eval_typ env tenv typ in
      let typ = eval_simplify_typ tenv typ in
      let vexpr = eval_expr env tenv expr in
      Ops.eval_cast typ vexpr
  | ExpressionMember { expr; name; _ } -> (
      let vexpr = eval_expr env tenv expr in
      let name = name.str in
      match vexpr with
      | Value.Header { entries; _ } | Value.Struct { entries } ->
          List.assoc name entries
      | _ ->
          Printf.sprintf "(eval_expr) %s cannot be accessed" (Value.print vexpr)
          |> failwith)
  | _ ->
      Printf.sprintf "(TODO: eval_expr) %s" (Pretty.print_expr expr) |> failwith

let eval_decl (env : env) (tenv : tenv) (decl : Declaration.t) : env =
  match decl with
  | Variable { name; init = Some value; _ } ->
      let value = eval_expr env tenv value in
      let env = Env.insert name.str value env in
      env
  | Variable { name; typ; init = None; _ } ->
      let value = Value.init (eval_typ env tenv typ) in
      let env = Env.insert name.str value env in
      env
  | _ ->
      Printf.sprintf "(TODO: eval_decl) %s" (Pretty.print_decl 0 decl)
      |> print_endline;
      env

let rec eval_stmt (env : env) (tenv : tenv) (stmt : Statement.t) : env =
  match stmt with
  (* (TODO) Perform implicit casts on assignment. *)
  | Assignment
      { lhs = Expression.Name { name = Name.BareName text; _ }; rhs; _ } ->
      let rvalue = eval_expr env tenv rhs in
      let env = Env.update text.str rvalue env in
      env
  | Conditional { cond; tru; fls = Some fls; _ } ->
      let vcond = eval_expr env tenv cond in
      let vcond = Ops.eval_cast Typ.Bool vcond in
      let env = Env.enter env in
      let stmts =
        match vcond with
        | Bool true -> tru
        | Bool false -> fls
        | _ -> assert false
      in
      let env = eval_stmt env tenv stmts in
      Env.exit env
  | BlockStatement { block; _ } -> eval_block env tenv block
  | EmptyStatement _ -> env
  | DeclarationStatement { decl; _ } -> eval_decl env tenv decl
  | _ ->
      Printf.sprintf "(TODO: eval_stmt) %s" (Pretty.print_stmt 0 stmt)
      |> print_endline;
      env

and eval_block (env : env) (tenv : tenv) (block : Block.t) : env =
  let env = Env.enter env in
  let env =
    List.fold_left
      (fun env stmt -> eval_stmt env tenv stmt)
      env block.statements
  in
  Env.exit env

let copyin (caller_env : env) (callee_env : env) (tenv : tenv)
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
  (* Copy-in a single parameter-argument pair. *)
  let copyin' (env : env) (param : Parameter.t) (arg : Argument.t) =
    match param.direction with
    (* in parameters are initialized by copying the value of the
       corresponding argument when the invocation is executed.
       inout parameters behave like a combination of in and out
       parameters simultaneously. *)
    | Some (In _ | InOut _) ->
        let param, arg =
          match arg with
          | Expression { value; _ } -> (param.variable.str, value)
          | KeyValue { key; value; _ } -> (key.str, value)
          | _ -> failwith "(TODO) (copyin) Support missing argument."
        in
        let value = eval_expr caller_env tenv arg in
        Env.insert param value env
    (* Direction out parameters are always initialized at the beginning
       of the execution of the portion of the program that has the out
       parameters. This initialization is not performed for parameters
       with any direction that is not out. (6.8) *)
    | Some (Out _) ->
        let typ = param.typ in
        let param = param.variable.str in
        let value = Value.init (eval_typ callee_env tenv typ) in
        Env.insert param value env
    | None ->
        failwith "(TODO) (copyin) Non-directional parameter is not supported."
  in
  List.fold_left2 copyin' callee_env params args

let copyout (caller_env : env) (callee_env : env) (_tenv : tenv)
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
  print_endline "Called eval_object_apply";
  Object.print obj |> print_endline;
  String.concat ", " (List.map Pretty.print_arg args) |> print_endline;
  Env.print caller_env |> print_endline;
  match obj with
  | Control { env; tenv; params; apply; _ } ->
      let callee_env = env in
      let callee_env = copyin caller_env callee_env tenv params args in
      let callee_env = eval_block callee_env tenv apply in
      let caller_env = copyout caller_env callee_env tenv params args in
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
