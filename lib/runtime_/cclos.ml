open Syntax.Ast
open Domain
open Base

(* Constructor closures for instantiation *)

module CClos = struct
  type t =
    | ExternCC of {
        vis_glob : vis_glob;
        tparams : string list;
        cparams : param list;
        methods : decl list;
      }
    | ParserCC of {
        vis_glob : vis_glob;
        tparams : string list;
        params : param list;
        cparams : param list;
        locals : decl list;
        states : parser_state list;
      }
    | ControlCC of {
        vis_glob : vis_glob;
        tparams : string list;
        params : param list;
        cparams : param list;
        locals : decl list;
        body : block;
      }
    | PackageCC of {
        vis_glob : vis_glob;
        tparams : string list;
        cparams : param list;
      }

  let pp fmt = function
    | ExternCC _ -> Format.fprintf fmt "extern"
    | ParserCC _ -> Format.fprintf fmt "parser"
    | ControlCC _ -> Format.fprintf fmt "control"
    | PackageCC _ -> Format.fprintf fmt "package"
end

(* Environment for constructor closures *)

module CCEnv = MakeEnv (Var) (CClos)
