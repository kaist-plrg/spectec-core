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


(* Utils *)

let print (cclos: t) =
  match cclos with
  | Package { params } ->
      Printf.sprintf "Package { params = (%s) }"
        (String.concat ", " (List.map Pretty.print_param params))
  | Parser { params; cparams; _ } ->
      Printf.sprintf "Parser { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Pretty.print_param params))
        (String.concat ", " (List.map Pretty.print_param cparams))
  | Control { params; cparams; _ } ->
      Printf.sprintf "Control { params = (%s); constructor_params = (%s) }"
        (String.concat ", " (List.map Pretty.print_param params))
        (String.concat ", " (List.map Pretty.print_param cparams))
  | Extern -> "Extern"
