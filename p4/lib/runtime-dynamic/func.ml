open Domain.Dom
open Il.Ast
module Value = Runtime_static.Value
open Util.Pp

type t =
  | ActionF of param list * block
  | FuncF of tparam list * param list * block
  | ExternFuncF of tparam list * param list
  | ExternMethodF of tparam list * param list
  | ParserApplyMethodF of param list * t FIdMap.t * decl list * block
  | ControlApplyMethodF of param list * t FIdMap.t * decl list * block
  | BuiltinMethodF of param list
  | TableApplyMethodF
  | ParserStateF of block

let rec pp ?(level = 0) fmt = function
  | ActionF (params, block) ->
      Format.fprintf fmt "ActionF%a %a"
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (Il.Pp.pp_block ~level:(level + 1))
        block
  | FuncF (tparams, params, block) ->
      Format.fprintf fmt "FuncF%a%a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (Il.Pp.pp_block ~level:(level + 1))
        block
  | ExternFuncF (tparams, params) ->
      Format.fprintf fmt "ExternFuncF%a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
  | ExternMethodF (tparams, params) ->
      Format.fprintf fmt "ExternMethodF%a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
  | ParserApplyMethodF (params, fenv, locals, block) ->
      Format.fprintf fmt
        "ParserApplyMethodF%a {\n%sfenv : %a\n%slocals :\n%a\n%s%a\n%s}"
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) pp)
        fenv
        (indent (level + 1))
        (Il.Pp.pp_decls ~level:(level + 2))
        locals
        (indent (level + 1))
        (Il.Pp.pp_block ~level:(level + 1))
        block (indent level)
  | ControlApplyMethodF (params, fenv, locals, block) ->
      Format.fprintf fmt
        "ControlApplyMethodF%a {\n%sfenv : %a\n%slocals :\n%a\n%s%a\n%s}"
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) pp)
        fenv
        (indent (level + 1))
        (Il.Pp.pp_decls ~level:(level + 2))
        locals
        (indent (level + 1))
        (Il.Pp.pp_block ~level:(level + 1))
        block (indent level)
  | BuiltinMethodF params ->
      Format.fprintf fmt "BuiltinMethodF%a"
        (Il.Pp.pp_params ~level:(level + 1))
        params
  | TableApplyMethodF -> Format.fprintf fmt "TableApplyMethodF"
  | ParserStateF block ->
      Format.fprintf fmt "ParserStateF %a"
        (Il.Pp.pp_block ~level:(level + 1))
        block

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
  | ParserStateF _, ParserStateF _ -> true
  | _ -> false
