open Syntax
open Ast

(* Stateful objects *)
(* The stateful types of objects in P4_16 are
   packages, parsers, controls, externs, tables, and value-sets
   P4_16 functions are also considered to be in that group,
   even if they happen to be pure functions of their arguments (Appendix F) *)

type t =
  | Package of
      { scope: Env.t; }
  | Parser of
      { scope: Env.t;
        params: Parameter.t list;
        locals: Declaration.t list;
        states: Parser.state list; }
  | Control of
      { scope: Env.t;
        params: Parameter.t list;
        locals: Declaration.t list;
        apply: Block.t; }
  | Extern
  | Table of
      { scope: Env.t;
        properties: Table.property list; }
  | Function
  | ValueSet


(* Utils *)

let print_object (obj: t) =
  match obj with
  | Package { scope } ->
      Printf.sprintf "Package { scope = %s }"
        (Env.print scope)
  | Parser { scope; _ } ->
      Printf.sprintf "Parser { scope = %s }"
        (Env.print scope)
  | Control { scope; _ } ->
      Printf.sprintf "Control { scope = %s }"
        (Env.print scope)
  | Extern -> "Extern"
  | Table { scope; _ } ->
      Printf.sprintf "Table { scope = %s }"
        (Env.print scope)
  | Function -> "Function"
  | ValueSet -> "ValueSet"
