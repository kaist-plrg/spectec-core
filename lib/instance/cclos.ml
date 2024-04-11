open Syntax
open Ast
open Runtime.Envs

(* Constructor closures *)

type t =
  | Package of { params : Parameter.t list; tparams : string list }
  | Parser of {
      tdenv : tdenv;
      cenv : cenv;
      tsto : tsto;
      vsto : vsto;
      params : Parameter.t list;
      tparams : string list;
      cparams : Parameter.t list;
      locals : Declaration.t list;
      states : Parser.state list;
    }
  | Control of {
      tdenv : tdenv;
      cenv : cenv;
      tsto : tsto;
      vsto : vsto;
      params : Parameter.t list;
      tparams : string list;
      cparams : Parameter.t list;
      locals : Declaration.t list;
      apply : Block.t;
    }
  | Extern of {
      tdenv : tdenv;
      cenv : cenv;
      tsto : tsto;
      vsto : vsto;
      tparams : string list;
      cparams : Parameter.t list;
      methods : MethodPrototype.t list;
    }

let print (t : t) =
  match t with
  | Package { params; _ } ->
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
  | Extern _ -> "Extern"
