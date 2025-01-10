open Domain.Dom
module L = Lang.Ast
module P = Lang.Pp
module F = Format
module Value = Runtime_static.Value
open Util.Pp

type t =
  | ExternO of L.id' * Value.t IdMap.t * Func.t FIdMap.t
  | ParserO of Value.t IdMap.t * Func.t FIdMap.t
  | ControlO of Value.t IdMap.t * Func.t FIdMap.t
  | PackageO of Value.t IdMap.t
  | TableO of L.id' * Func.t FIdMap.t

let pp ?(level = 0) fmt = function
  | ExternO (id, venv, fenv) ->
      F.fprintf fmt "ExternO %a {\n%svenv : %a\n%sfenv: %a\n%s}" P.pp_id' id
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv (indent level)
  | ParserO (venv, fenv) ->
      F.fprintf fmt "ParserO {\n%svenv : %a\n%sfenv: %a\n%s}"
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv (indent level)
  | ControlO (venv, fenv) ->
      F.fprintf fmt "ControlO {\n%svenv : %a\n%sfenv: %a\n%s}"
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv (indent level)
  | PackageO venv ->
      F.fprintf fmt "PackageO {\n%svenv : %a\n%s}"
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv (indent level)
  | TableO (id, fenv) ->
      F.fprintf fmt "TableO %a {\n%sfenv : %a\n%s}" P.pp_id' id
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv (indent level)
