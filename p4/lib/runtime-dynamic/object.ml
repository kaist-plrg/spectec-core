open Domain.Dom
open Il.Ast
module Value = Runtime_static.Value

type t =
  | ExternO of Value.t IdMap.t * Func.t FIdMap.t
  | ParserO of Value.t IdMap.t * Func.t FIdMap.t
  | ControlO of Value.t IdMap.t * Func.t FIdMap.t
  | PackageO of Value.t IdMap.t
  | TableO of table

let pp fmt = function
  | ExternO (venv, fenv) ->
      Format.fprintf fmt "ExternO {\nvenv : %a\nfenv: %a\n}" (IdMap.pp Value.pp)
        venv (FIdMap.pp Func.pp) fenv
  | ParserO (venv, fenv) ->
      Format.fprintf fmt "ParserO {\nvenv : %a\nfenv: %a\n}" (IdMap.pp Value.pp)
        venv (FIdMap.pp Func.pp) fenv
  | ControlO (venv, fenv) ->
      Format.fprintf fmt "ControlO {\nvenv : %a\nfenv: %a\n}"
        (IdMap.pp Value.pp) venv (FIdMap.pp Func.pp) fenv
  | PackageO venv ->
      Format.fprintf fmt "PackageO {\nvenv : %a\n}" (IdMap.pp Value.pp) venv
  | TableO table ->
      Format.fprintf fmt "TableO {\n%a\n}" (Il.Pp.pp_table ~level:1) table
