open Syntax
open Ast
open Utils

(* Stateful objects *)
(* The stateful types of objects in P4_16 are
   packages, parsers, controls, externs, tables, and value-sets
   P4_16 functions are also considered to be in that group,
   even if they happen to be pure functions of their arguments (Appendix F) *)

type t =
  | Package of { scope : Env.t }
  | Parser of {
      scope : Env.t;
      params : Parameter.t list;
      locals : Declaration.t list;
      states : Parser.state list;
    }
  | Control of {
      scope : Env.t;
      params : Parameter.t list;
      locals : Declaration.t list;
      apply : Block.t;
    }
  | Extern
  | Table of { scope : Env.t; properties : Table.property list }
  | Function
  | ValueSet

(* Printer *)

let print ?(indent = 0) (obj : t) =
  match obj with
  | Package { scope } ->
      Printf.sprintf "%sPackage {\n%sscope =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print scope ~indent:(indent + 3))
  | Parser { scope; _ } ->
      Printf.sprintf "%sParser {\n%sscope =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print scope ~indent:(indent + 3))
  | Control { scope; _ } ->
      Printf.sprintf "%sControl {\n%sscope =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print scope ~indent:(indent + 3))
  | Extern -> Printf.sprintf "%sExtern" (Print.print_indent indent)
  | Table { scope; _ } ->
      Printf.sprintf "%sTable {\n%sscope =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (Env.print scope ~indent:(indent + 3))
  | Function -> Printf.sprintf "%sFunction" (Print.print_indent indent)
  | ValueSet -> Printf.sprintf "%sValueSet" (Print.print_indent indent)
