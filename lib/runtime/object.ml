open Syntax.Ast
open Domain
open Base

(* Runtime representation of objects *)

module Object = struct
  type t =
    (* Objects that are actually stateful *)
    | ValueSetO
    | TableO of {
        key : table_key list;
        actions : table_action list;
        entries : table_entry list;
        default : table_default option;
        custom : table_custom list;
        mthd : Func.t; (* "apply" *)
      }
    | ExternO of { vis_glob : vis; env_obj : env }
    (* Objects serving as wrappers *)
    | ParserO of {
        vis_glob : vis; (* global scope *)
        env_obj : env; (* block environment for locals and states *)
        mthd : Func.t; (* "apply" is the only entry point *)
      }
    | ControlO of {
        vis_glob : vis; (* global scope *)
        env_obj : env; (* block environment for locals and actions *)
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
end

(* Store maps object identifiers (fully-qualified paths) to objects *)
module Sto = MakeEnv (Path) (Object)
