open Syntax.Ast
open Base_env
open Func_env

type t =
  (* Objects that are actually stateful *)
  | ValueSetO
  | TableO of {
      key: table_key list;
      actions: table_action list;
      entries: table_entry list;
      default: table_default option;
      custom: table_custom list;
      mthd: Func.t; (* "apply" *)
    }
  | ExternO of {
      tdenv: TDEnv.t;
      mthd: FEnv.t;
    }
  (* Objects serving as wrappers *)
  | ParserO of {
      tdenv: TDEnv.t;
      gscope: Scope.t;
      benv: VEnv.t;
      fenv: FEnv.t; (* states *)
      mthd: Func.t; (* "apply" *)
    }
  | ControlO of {
      tdenv: TDEnv.t;
      gscope: Scope.t;
      benv: VEnv.t;
      fenv: FEnv.t; (* actions *)
      mthd: Func.t; (* "apply" *)
    }
  | PackageO

let pp fmt = function
  | ValueSetO -> Format.fprintf fmt "value set"
  | TableO _ -> Format.fprintf fmt "table"
  | ExternO _ -> Format.fprintf fmt "extern"
  | ParserO _ -> Format.fprintf fmt "parser"
  | ControlO _ -> Format.fprintf fmt "control"
  | PackageO -> Format.fprintf fmt "package"
