open Domain.Dom
module F = Format
module Value = Runtime_static.Value
open Util.Pp

type t =
  | ExternO of Value.t IdMap.t * Func.t FIdMap.t
  | ParserO of Value.t IdMap.t * Func.t FIdMap.t
  | ControlO of Value.t IdMap.t * Func.t FIdMap.t
  | PackageO of Value.t IdMap.t
  | TableO of Func.t FIdMap.t

let pp ?(level = 0) fmt = function
  | ExternO (venv, fenv) ->
      F.fprintf fmt "ExternO {\n%svenv : %a\n%sfenv: %a\n%s}"
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
  | TableO fenv ->
      F.fprintf fmt "PackageO {\n%sfenv : %a\n%s}"
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv (indent level)
