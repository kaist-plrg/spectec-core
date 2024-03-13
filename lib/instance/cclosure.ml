open Syntax
open Ast

(* Constructor closures *)

type t =
  | Package of
      { params: Parameter.t list; }
  | Parser of
      { params: Parameter.t list;
        cparams: Parameter.t list;
        locals: Declaration.t list;
        states: Parser.state list; }
  | Control of
      { params: Parameter.t list;
        cparams: Parameter.t list;
        locals: Declaration.t list;
        apply: Block.t; }
  | Extern


(* Constructor closure environment *)

type cenv = t Path.PMap.t

let empty_cenv = Path.PMap.empty

let insert_cenv
  (path: Path.t) (cclos: t) (cenv: cenv) =
  Path.PMap.add path cclos cenv

let find_cenv (path: Path.t) (cenv: cenv) =
  Path.PMap.find path cenv


(* Utils *)

let print (cclos: t) =
  match cclos with
  | Package { params } ->
      Printf.sprintf "Package { params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
  | Parser { params; cparams; _ } ->
      Printf.sprintf "Parser { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
        (String.concat ", " (List.map Print.print_param cparams))
  | Control { params; cparams; _ } ->
      Printf.sprintf "Control { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Print.print_param params))
        (String.concat ", " (List.map Print.print_param cparams))
  | Extern -> "Extern"
