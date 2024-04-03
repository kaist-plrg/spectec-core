open Syntax
open Ast
open Utils
open Envs

(* Stateful objects *)
(* The stateful types of objects in P4_16 are
   packages, parsers, controls, externs, tables, and value-sets
   P4_16 functions are also considered to be in that group,
   even if they happen to be pure functions of their arguments (Appendix F) *)

type t =
  | Package of { tdenv : tdenv; cenv : cenv; tsto : tsto; vsto : vsto }
  | Parser of { tdenv : tdenv; tsto : tsto; vsto : vsto; funcs : Func.t list }
  | Control of { tdenv : tdenv; tsto : tsto; vsto : vsto; funcs : Func.t list }
  | Extern
  | Table of { cenv : cenv; lenv : lenv; properties : Table.property list }
  | Function
  | ValueSet

let print (t : t) =
  let indent = 1 in
  match t with
  | Package { cenv; tsto; vsto; _ } ->
      Printf.sprintf "%sPackage {\n%scenv =\n%s\n%ststo =\n%s\n%svsto =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (TSto.print tsto ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (VSto.print vsto ~indent:(indent + 3))
  | Parser { tsto; vsto; funcs; _ } ->
      Printf.sprintf "%sParser {\n%ststo =\n%s\n%svsto =\n%s\n%sfuncs =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (TSto.print tsto ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (VSto.print vsto ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (String.concat "\n" (List.map (Func.print ~indent:(indent + 3)) funcs))
  | Control { tsto; vsto; funcs; _ } ->
      Printf.sprintf "%sControl {\n%ststo =\n%s\n%svsto =\n%s\n%sfuncs =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (TSto.print tsto ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (VSto.print vsto ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (String.concat "\n" (List.map (Func.print ~indent:(indent + 3)) funcs))
  | Extern -> Printf.sprintf "%sExtern" (Print.print_indent indent)
  | Table { cenv; lenv; _ } ->
      Printf.sprintf "%sTable {\n%scenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | Function -> Printf.sprintf "%sFunction" (Print.print_indent indent)
  | ValueSet -> Printf.sprintf "%sValueSet" (Print.print_indent indent)
