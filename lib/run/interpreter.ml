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

(* Interpreter *)

let eval_decl (_store : store) (env : env) (tenv : tenv) (decl : Declaration.t)
    =
  match decl with
  | Variable { name; init = Some value; _ } ->
      let value = Static.eval_expr env tenv value in
      let env = Env.insert name.str value env in
      env
  | _ ->
      Printf.sprintf "(TODO: eval_decl) %s" (Pretty.print_decl 0 decl)
      |> print_endline;
      env

let rec eval_stmt (store : store) (env : env) (tenv : tenv) (stmt : Statement.t)
    =
  match stmt with
  | Assignment
      { lhs = Expression.Name { name = Name.BareName text; _ }; rhs; _ } ->
      let rvalue = Static.eval_expr env tenv rhs in
      let env = Env.update text.str rvalue env in
      env
  | Conditional { cond; tru; fls = Some fls; _ } -> (
      let vcond = Static.eval_expr env tenv cond in
      let vcond = Ops.eval_cast Typ.Bool vcond in
      match vcond with
      | Bool true ->
          let env = Env.enter env in
          let env = eval_stmt store env tenv tru in
          Env.exit env
      | Bool false ->
          let env = Env.enter env in
          let env = eval_stmt store env tenv fls in
          Env.exit env
      | _ -> assert false)
  | BlockStatement { block; _ } -> eval_block store env tenv block
  | DeclarationStatement { decl; _ } -> eval_decl store env tenv decl
  | _ ->
      Printf.sprintf "(TODO: eval_stmt) %s" (Pretty.print_stmt 0 stmt)
      |> print_endline;
      env

and eval_block (store : store) (env : env) (tenv : tenv) (block : Block.t) =
  let env = Env.enter env in
  let env =
    List.fold_left
      (fun env stmt -> eval_stmt store env tenv stmt)
      env block.statements
  in
  print_endline "Exiting block";
  print_endline (Env.print env);
  Env.exit env

let eval_object_apply (store : store) (obj : Object.t) (_args : Value.t list) =
  match obj with
  | Control { env; tenv; apply; _ } ->
      let _env = eval_block store env tenv apply in
      ()
  | _ ->
      Printf.sprintf "(TODO: eval_object) %s" (Object.print obj)
      |> print_endline

let eval_program (store : store) (_program : program) =
  print_endline "(TODO: interpreter)";
  Printf.sprintf "Instantiation results in this store:\n%s"
    (Store.print store ~indent:1)
  |> print_endline;
  let main = Store.find [ "main" ] store in
  Printf.sprintf "main object:\n%s" (Object.print main ~indent:1)
  |> print_endline
