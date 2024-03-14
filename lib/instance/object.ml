open Syntax
open Ast

(* Stateful objects *)
(* The stateful types of objects in P4_16 are
   packages, parsers, controls, externs, tables, and value-sets
   P4_16 functions are also considered to be in that group,
   even if they happen to be pure functions of their arguments (Appendix F) *)

type t =
  | Package of
      { scope: Value.env; }
  | Parser of
      { scope: Value.env;
        params: Parameter.t list;
        locals: Declaration.t list;
        states: Parser.state list; }
  | Control of
      { scope: Value.env;
        params: Parameter.t list;
        locals: Declaration.t list;
        apply: Block.t; }
  | Extern
  | Table of
      { scope: Value.env;
        properties: Table.property list; }
  | Function
  | ValueSet


(* Global store of objects *)
(* Instances should be distinguishable by their paths,
   or fully-qualified names, so a store can be a flat map *)

type store = t Path.PMap.t

let empty_store = Path.PMap.empty

let insert_store
  (path: Path.t) (obj: t) (store: store) =
  Path.PMap.add path obj store

let find_store
  (path: Path.t) (store: store) =
  Path.PMap.find path store


(* Utils *)

let print_object (obj: t) =
  match obj with
  | Package { scope } ->
      Printf.sprintf "Package { scope = %s }"
        (Value.print_env scope)
  | Parser { scope; _ } ->
      Printf.sprintf "Parser { scope = %s }"
        (Value.print_env scope)
  | Control { scope; _ } ->
      Printf.sprintf "Control { scope = %s }"
        (Value.print_env scope)
  | Extern -> "Extern"
  | Table { scope; _ } ->
      Printf.sprintf "Table { scope = %s }"
        (Value.print_env scope)
  | Function -> "Function"
  | ValueSet -> "ValueSet"

let print_store (store: store) =
  let print_binding path obj acc =
    Printf.sprintf "%s\n  %s -> %s"
      acc (String.concat "." path) (print_object obj)
  in
  "[" ^ (Path.PMap.fold print_binding store "") ^ "\n]"
