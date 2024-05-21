open Syntax.Ast
open Domain
open Base
open Func

(* Constructor closures for instantiation *)

module CClos = struct
  type t =
    | ExternCC of {
        tdenv : TDEnv.t;
        gscope : VVis.t * FVis.t;
        tparams : string list;
        params : param list;
        cparams : param list;
        methods : decl list;
      }
    | ParserCC of {
        tdenv : TDEnv.t;
        gscope : VVis.t * FVis.t;
        tparams : string list;
        params : param list;
        cparams : param list;
        locals : decl list;
        states : parser_state list;
      }
    | ControlCC of {
        tdenv : TDEnv.t;
        gscope : VVis.t * FVis.t;
        tparams : string list;
        params : param list;
        cparams : param list;
        locals : decl list;
        apply : block;
      }
    | PackageCC of { tparams : string list; params : param list }

  let pp fmt = function
    | ExternCC _ -> Format.fprintf fmt "extern"
    | ParserCC _ -> Format.fprintf fmt "parser"
    | ControlCC _ -> Format.fprintf fmt "control"
    | PackageCC _ -> Format.fprintf fmt "package"
end

(* Environment for constructor closures *)

module CCEnv = MakeEnv (Var) (CClos)
