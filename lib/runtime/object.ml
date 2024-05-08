open Surface.Ast
open Scope

type t =
  | OPackage of { tdenv : TDEnv.t; genv : Env.t; sto : Sto.t }
  | OParser of { tdenv : TDEnv.t; sto : Sto.t; funcs : Func.t list }
  | OControl of { tdenv : TDEnv.t; sto : Sto.t; funcs : Func.t list }
  | OExtern of { tdenv : TDEnv.t; sto : Sto.t; funcs : Func.t list }
  | OTable of {
      genv : Env.t;
      lenv : Env.t;
      keys : Table.key list;
      actions : Table.action_ref list;
      default : Table.action_ref option;
    }
  | OFunction
  | OValueSet

let pp fmt obj =
  let depth = 1 in
  let indent depth = String.make (depth * 2) ' ' in
  match obj with
  | OPackage { genv; sto; _ } ->
      Format.fprintf fmt "Package {\n%sgenv = %s\n%ssto = %s }"
        (indent (depth + 2))
        (Format.asprintf "%a" Env.pp genv)
        (indent (depth + 2))
        (Format.asprintf "%a" Sto.pp sto)
  | OParser { sto; funcs; _ } ->
      Format.fprintf fmt "Parser {\n%ssto = %s\n%sfuncs =\n%s }"
        (indent (depth + 2))
        (Format.asprintf "%a" Sto.pp sto)
        (indent (depth + 2))
        (String.concat "\n" (List.map (Format.asprintf "%a" Func.pp) funcs))
  | OControl { sto; funcs; _ } ->
      Format.fprintf fmt "Control {\n%ssto = %s\n%sfuncs =\n%s }"
        (indent (depth + 2))
        (Format.asprintf "%a" Sto.pp sto)
        (indent (depth + 2))
        (String.concat "\n" (List.map (Format.asprintf "%a" Func.pp) funcs))
  | OExtern { sto; funcs; _ } ->
      Format.fprintf fmt "Extern {\n%ssto = %s\n%sfuncs =\n%s }"
        (indent (depth + 2))
        (Format.asprintf "%a" Sto.pp sto)
        (indent (depth + 2))
        (String.concat "\n" (List.map (Format.asprintf "%a" Func.pp) funcs))
  | OTable { genv; lenv; _ } ->
      Format.fprintf fmt "Table {\n%sgenv = %s\n%slenv = %s }"
        (indent (depth + 2))
        (Format.asprintf "%a" Env.pp genv)
        (indent (depth + 2))
        (Format.asprintf "%a" Env.pp lenv)
  | OFunction -> Format.fprintf fmt "Function"
  | OValueSet -> Format.fprintf fmt "ValueSet"
