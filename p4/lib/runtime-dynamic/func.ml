open Domain.Dom
open Il.Ast
module F = Format
module Value = Runtime_static.Value
module LValue = Runtime_static.Lvalue
open Util.Pp

type t =
  | BuiltinMethodF of param list * LValue.t
  | ActionF of param list * block
  | FuncF of tparam list * param list * block
  | ExternFuncF of tparam list * param list
  | ExternMethodF of tparam list * param list * block option
  | ExternAbstractMethodF of tparam list * param list
  | ParserApplyMethodF of param list * decl list * State.t IdMap.t
  | ControlApplyMethodF of param list * decl list * t FIdMap.t * block
  | TableApplyMethodF of table

let rec pp ?(level = 0) fmt = function
  | BuiltinMethodF (params, _lvalue) ->
      F.fprintf fmt "BuiltinMethodF%a"
        (Il.Pp.pp_params ~level:(level + 1))
        params
  | ActionF (params, block) ->
      F.fprintf fmt "ActionF%a %a"
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (Il.Pp.pp_block ~level:(level + 1))
        block
  | FuncF (tparams, params, block) ->
      F.fprintf fmt "FuncF%a%a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (Il.Pp.pp_block ~level:(level + 1))
        block
  | ExternFuncF (tparams, params) ->
      F.fprintf fmt "ExternFuncF%a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
  | ExternMethodF (tparams, params, block) ->
      F.fprintf fmt "ExternMethodF%a %a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (pp_option (Il.Pp.pp_block ~level:(level + 1)))
        block
  | ExternAbstractMethodF (tparams, params) ->
      F.fprintf fmt "ExternAbstractMethodF%a %a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
  | ParserApplyMethodF (params, decls, senv) ->
      F.fprintf fmt "ParserApplyMethodF%a {\n%ssenv : %a\n%slocals :\n%a\n%s}"
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (indent (level + 1))
        (Il.Pp.pp_decls ~level:(level + 2))
        decls
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) State.pp)
        senv
        (indent (level + 1))
  | ControlApplyMethodF (params, decls, fenv, block) ->
      F.fprintf fmt
        "ControlApplyMethodF%a {\n%slocals : %a\n%sfenv :\n%a\n%s%a\n%s}"
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (indent (level + 1))
        (Il.Pp.pp_decls ~level:(level + 2))
        decls
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) pp)
        fenv
        (indent (level + 1))
        (Il.Pp.pp_block ~level:(level + 1))
        block (indent level)
  | TableApplyMethodF table ->
      F.fprintf fmt "TableApplyMethodF %a"
        (Il.Pp.pp_table ~level:(level + 1))
        table

let eq_kind func_a func_b =
  match (func_a, func_b) with
  | BuiltinMethodF _, BuiltinMethodF _ -> true
  | ActionF _, ActionF _ -> true
  | FuncF _, FuncF _ -> true
  | ExternFuncF _, ExternFuncF _ -> true
  | ExternMethodF _, ExternMethodF _ -> true
  | ParserApplyMethodF _, ParserApplyMethodF _ -> true
  | ControlApplyMethodF _, ControlApplyMethodF _ -> true
  | TableApplyMethodF _, TableApplyMethodF _ -> true
  | _ -> false

let get_params = function
  | BuiltinMethodF (params, _) -> params
  | ActionF (params, _) -> params
  | FuncF (_, params, _) -> params
  | ExternFuncF (_, params) -> params
  | ExternMethodF (_, params, _) -> params
  | ParserApplyMethodF (params, _, _) -> params
  | ControlApplyMethodF (params, _, _, _) -> params
  | _ -> []
