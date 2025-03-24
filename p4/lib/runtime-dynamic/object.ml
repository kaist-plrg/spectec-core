module F = Format
open Domain.Dom
module Value = Runtime_value.Value
module Type = Il.Types.Type
module L = Il.Ast
module P = Il.Pp
open Util.Pp

type t =
  | ExternO of L.id' * Type.t TIdMap.t * Value.t IdMap.t * Func.t FIdMap.t
  | ParserO of Value.t IdMap.t * L.param list * L.decl list * State.t IdMap.t
  | ControlO of
      Value.t IdMap.t * L.param list * L.decl list * Func.t FIdMap.t * L.block
  | PackageO of Type.t TIdMap.t * Value.t IdMap.t
  | TableO of L.id' * Value.t IdMap.t * Table.t

let pp ?(level = 0) fmt = function
  | ExternO (id, theta, venv, fenv) ->
      F.fprintf fmt "ExternO %a {\n%stheta : %a\n%svenv : %a\n%sfenv: %a\n%s}"
        P.pp_id' id
        (indent (level + 1))
        (TIdMap.pp ~level:(level + 1) Type.pp)
        theta
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv (indent level)
  | ParserO (venv, params, decls, senv) ->
      F.fprintf fmt "ParserO%a {\n%svenv : %a\n%slocals: %a\n%ssenv: %a\n%s}"
        (P.pp_params ~level:(level + 1))
        params
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (P.pp_decls ~level:(level + 2))
        decls
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) State.pp)
        senv (indent level)
  | ControlO (venv, params, decls, fenv, block) ->
      F.fprintf fmt
        "ControlO%a {\n%svenv : %a\n%slocals :\n%a\n%sfenv : %a\n%s%a\n%s}"
        (P.pp_params ~level:(level + 1))
        params
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (P.pp_decls ~level:(level + 2))
        decls
        (indent (level + 1))
        (FIdMap.pp ~level:(level + 1) Func.pp)
        fenv
        (indent (level + 1))
        (P.pp_block ~level:(level + 1))
        block (indent level)
  | PackageO (theta, venv) ->
      F.fprintf fmt "PackageO {\n%stheta : %a\n%svenv : %a\n%s}"
        (indent (level + 1))
        (TIdMap.pp ~level:(level + 1) Type.pp)
        theta
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv (indent level)
  | TableO (id, venv, table) ->
      F.fprintf fmt "TableO %a {\n%svenv: %a\n%stable : %a\n%s}" P.pp_id' id
        (indent (level + 1))
        (IdMap.pp ~level:(level + 1) Value.pp)
        venv
        (indent (level + 1))
        (Table.pp ~level:(level + 1))
        table (indent level)

(* Getters *)

let get_control t =
  match t with
  | ControlO (venv, params, decls, fenv, block) ->
      (venv, params, decls, fenv, block)
  | _ -> failwith "(get_control) Not a control"

let get_table t =
  match t with
  | TableO (id, venv, table) -> (id, venv, table)
  | _ -> failwith "(get_table) Not a table"
