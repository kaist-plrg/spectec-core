open Syntax.Ast
open Domain

(* Constructor closures for instantiation *)

module CClos = struct
  type t =
    | ParserCC of {
        tparams : tparam list;
        params : param list;
        cparams : param list;
        locals : decl list;
        states : parser_state list;
      }
    | ControlCC of {
        tparams : tparam list;
        params : param list;
        cparams : param list;
        locals : decl list;
        body : block;
      }
    | PackageCC of { tparams : tparam list; cparams : param list }
    | ExternCC of {
        tparams : tparam list;
        cparams : param list;
        mthds : decl list;
      }

  let pp fmt = function
    | ParserCC _ -> Format.fprintf fmt "parser"
    | ControlCC _ -> Format.fprintf fmt "control"
    | PackageCC _ -> Format.fprintf fmt "package"
    | ExternCC _ -> Format.fprintf fmt "extern"
end

(* Environment for constructor closures *)

module CCEnv = MakeEnv (Var) (CClos)
