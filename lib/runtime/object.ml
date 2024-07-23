open Syntax.Ast

(* Runtime representation of objects *)

type t =
  (* Objects that are actually stateful *)
  | ValueSetO
  | TableO of { table : table; mthd : Func.t (* "apply" *) }
  | ExternO of { vis_glob : Vis.vis; env_obj : Env.env }
  (* Objects serving as wrappers *)
  | ParserO of {
      vis_glob : Vis.vis; (* global scope *)
      env_obj : Env.env; (* block environment for locals and states *)
      mthd : Func.t; (* "apply" is the only entry point *)
    }
  | ControlO of {
      vis_glob : Vis.vis; (* global scope *)
      env_obj : Env.env; (* block environment for locals and actions *)
      mthd : Func.t; (* "apply" is the only entry point *)
    }
  | PackageO

let pp fmt = function
  | ValueSetO -> Format.fprintf fmt "valueset"
  | TableO _ -> Format.fprintf fmt "table"
  | ExternO _ -> Format.fprintf fmt "extern"
  | ParserO _ -> Format.fprintf fmt "parser"
  | ControlO _ -> Format.fprintf fmt "control"
  | PackageO -> Format.fprintf fmt "package"
