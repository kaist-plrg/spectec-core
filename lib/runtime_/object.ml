open Syntax.Ast
open Domain
open Base
open Func

(* Runtime representation of objects *)

module rec Object : sig
  type t =
    | ValueSetO
    | TableO of {
        key : table_key list;
        actions : table_action list;
        entries : table_entry list;
        default : table_default option;
        custom : table_custom list;
        mthd : Func.t;
      }
    | ExternO of { tdenv : TDEnv.t; mthd : FEnv.t }
    | ParserO of {
        tdenv : TDEnv.t;
        gscope : VVis.t * FVis.t;
        benv : VTEnv.t * FEnv.t;
        lsto : Sto.t;
        mthd : Func.t;
      }
    | ControlO of {
        tdenv : TDEnv.t;
        gscope : VVis.t * FVis.t;
        benv : VTEnv.t * FEnv.t;
        lsto : Sto.t;
        mthd : Func.t;
      }
    | PackageO of { lsto : Sto.t }

  val pp : Format.formatter -> t -> unit
end = struct
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
    | ExternO of { tdenv : TDEnv.t; mthd : FEnv.t (* method prototypes *) }
    (* Objects serving as wrappers *)
    | ParserO of {
        tdenv : TDEnv.t; (* typedef environment *)
        gscope : VVis.t * FVis.t; (* global scope *)
        benv : VTEnv.t * FEnv.t; (* block environment for locals and states *)
        lsto : Sto.t; (* local store *)
        mthd : Func.t; (* "apply" is the only entry point *)
      }
    | ControlO of {
        tdenv : TDEnv.t; (* typedef environment *)
        gscope : VVis.t * FVis.t; (* global scope *)
        benv : VTEnv.t * FEnv.t; (* block environment for locals and actions *)
        lsto : Sto.t; (* local store *)
        mthd : Func.t; (* "apply" is the only entry point *)
      }
    | PackageO of { lsto : Sto.t }

  let pp fmt = function
    | ValueSetO -> Format.fprintf fmt "value set"
    | TableO _ -> Format.fprintf fmt "table"
    | ExternO _ -> Format.fprintf fmt "extern"
    | ParserO _ -> Format.fprintf fmt "parser"
    | ControlO _ -> Format.fprintf fmt "control"
    | PackageO _ -> Format.fprintf fmt "package"
end

(* Store maps object identifiers (fully-qualified paths) to objects *)
and Sto : sig
  type t

  val empty : t
  val find : Path.t -> t -> Object.t option
  val add : Path.t -> Object.t -> t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  include MakeEnv (Path) (Object)
end
