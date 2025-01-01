open Domain.Dom
open Il.Ast
module Value = Runtime_static.Value

type t =
  | ActionF of param list * block
  | FuncF of tparam list * param list * block
  | ExternFuncF of tparam list * param list
  | ExternMethodF of tparam list * param list
  | ParserApplyMethodF of
      param list * Value.t IdMap.t * t FIdMap.t * decl list * block
  | ControlApplyMethodF of
      param list * Value.t IdMap.t * t FIdMap.t * decl list * block
  | BuiltinMethodF of param list
  | TableApplyMethodF

let rec pp fmt = function
  | ActionF (params, block) ->
      Format.fprintf fmt "ActionF%a %a" Il.Pp.pp_params params
        (Il.Pp.pp_block ~level:1) block
  | FuncF (tparams, params, block) ->
      Format.fprintf fmt "FuncF%a%a %a" Il.Pp.pp_tparams tparams Il.Pp.pp_params
        params (Il.Pp.pp_block ~level:1) block
  | ExternFuncF (tparams, params) ->
      Format.fprintf fmt "ExternFuncF%a %a" Il.Pp.pp_tparams tparams
        Il.Pp.pp_params params
  | ExternMethodF (tparams, params) ->
      Format.fprintf fmt "ExternMethodF%a %a" Il.Pp.pp_tparams tparams
        Il.Pp.pp_params params
  | ParserApplyMethodF (params, venv, fenv, locals, block) ->
      Format.fprintf fmt
        "ParserApplyMethodF%a {\nvenv : %a\nfenv : %a\nlocals : %a\n%a\n}"
        Il.Pp.pp_params params (IdMap.pp Value.pp) venv (FIdMap.pp pp) fenv
        (Il.Pp.pp_decls ~level:1) locals (Il.Pp.pp_block ~level:1) block
  | ControlApplyMethodF (params, venv, fenv, locals, block) ->
      Format.fprintf fmt
        "ControlApplyMethodF%a {\nvenv : %a\nfenv : %a\nlocals : %a\n%a\n}"
        Il.Pp.pp_params params (IdMap.pp Value.pp) venv (FIdMap.pp pp) fenv
        (Il.Pp.pp_decls ~level:1) locals (Il.Pp.pp_block ~level:1) block
  | BuiltinMethodF params ->
      Format.fprintf fmt "BuiltinMethodF%a" Il.Pp.pp_params params
  | TableApplyMethodF -> Format.fprintf fmt "TableApplyMethodF"

let eq_kind func_a func_b =
  match (func_a, func_b) with
  | ActionF _, ActionF _ -> true
  | FuncF _, FuncF _ -> true
  | ExternFuncF _, ExternFuncF _ -> true
  | ExternMethodF _, ExternMethodF _ -> true
  | ParserApplyMethodF _, ParserApplyMethodF _ -> true
  | ControlApplyMethodF _, ControlApplyMethodF _ -> true
  | BuiltinMethodF _, BuiltinMethodF _ -> true
  | TableApplyMethodF, TableApplyMethodF -> true
  | _ -> false
