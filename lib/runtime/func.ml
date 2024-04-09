open Syntax
open Ast
open Utils
open Envs

(* Function (method) defined in a block *)

type t =
  | Parser of {
      name : string;
      params : Parameter.t list;
      cenv : cenv;
      lenv : lenv;
      init : Statement.t list;
    }
  | State of {
      name : string;
      cenv : cenv;
      lenv : lenv;
      body : Statement.t list;
      transition : Parser.transition;
    }
  | Control of {
      name : string;
      params : Parameter.t list;
      cenv : cenv;
      lenv : lenv;
      body : Statement.t list;
    }

(* Printer *)

let print ?(indent = 0) (t : t) =
  match t with
  | Parser { name; params; cenv; lenv; _ } ->
      Printf.sprintf "%s%s(%s) {\n%scenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        name
        (String.concat ", " (List.map Pretty.print_param params))
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | State { name; cenv; lenv; _ } ->
      Printf.sprintf "%sstate %s {\n%scenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        name
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
  | Control { name; params; cenv; lenv; _ } ->
      Printf.sprintf "%s%s(%s) {\n%scenv =\n%s\n%slenv =\n%s }"
        (Print.print_indent indent)
        name
        (String.concat ", " (List.map Pretty.print_param params))
        (Print.print_indent (indent + 2))
        (CEnv.print cenv ~indent:(indent + 3))
        (Print.print_indent (indent + 2))
        (LEnv.print lenv ~indent:(indent + 3))
