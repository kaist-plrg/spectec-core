open Syntax
open Ast
open Utils
open Envs

(* Modeling various functions in P4 *)

type t =
  | Normal of {
      name : string;
      params : Parameter.t list;
      cenv : cenv;
      lenv : lenv;
      body : Statement.t list;
    }
  | Parser of {
      name : string;
      params : Parameter.t list;
      cenv : cenv;
      lenv : lenv;
      body : Statement.t list;
      transition : Parser.transition;
    }
  | External of { name : string; params : Parameter.t list; cenv : cenv }

(* Printer *)

let print ?(indent = 0) (t : t) =
  match t with
  | Normal { name; params; cenv; lenv; _ } ->
      Printf.sprintf "%s%s(%s) {\n%scenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        name
        (String.concat ", " (List.map Pretty.print_param params))
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | Parser { name; cenv; lenv; _ } ->
      Printf.sprintf "%sstate %s {\n%scenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        name
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | External { name; params; cenv } ->
      Printf.sprintf "%s%s(%s) {\n%scenv =\n%s }"
        (Print.print_indent indent)
        name
        (String.concat ", " (List.map Pretty.print_param params))
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
