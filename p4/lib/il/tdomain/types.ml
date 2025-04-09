include Ast
open Pp
open Eq_typ
open Free
open Subst
open Utils

(* Modules *)

module Type = struct
  type t = typ'

  let pp = pp_typ'
  let eq = eq_typ'
  let eq_alpha = eq_typ_alpha
  let free = free_typ
  let subst = subst_typ
  let unroll = unroll_typ
  let canon = canon_typ
  let is_numeric = is_numeric_typ
  let is_nominal = is_nominal_typ
  let is_ground = is_ground_typ
  let is_assignable = is_assignable_typ
  let is_defaultable = is_defaultable_typ
  let is_equalable = is_equalable_typ
  let get_width = get_width_typ
end

module TypeDef = struct
  type t = typdef

  let pp = pp_typdef
  let eq = eq_typdef
  let free = free_typdef
  let subst = subst_typdef
  let specialize = specialize_typdef
  let specialize_poly = specialize_typdef_poly
end

module FuncType = struct
  type t = functyp

  let pp = pp_functyp
  let eq = eq_functyp
  let eq_alpha = eq_functyp_alpha
  let eq_kind = eq_functyp_kind
  let subst = subst_functyp
  let is_action = is_action_functyp
  let get_params = get_params_functyp
  let get_typ_ret = get_typ_ret_functyp
end

module FuncDef = struct
  type t = funcdef

  let pp = pp_funcdef
  let eq = eq_funcdef
  let eq_alpha = eq_funcdef_alpha
  let eq_kind = eq_funcdef_kind
  let free = free_funcdef
  let subst = subst_funcdef
  let specialize = specialize_funcdef
  let get_tparams = get_tparams_funcdef
  let get_params = get_params_funcdef
  let get_typ_ret = get_typ_ret_funcdef
end

module ConsType = struct
  type t = constyp

  let pp = pp_constyp
  let subst = subst_constyp
end

module ConsDef = struct
  type t = consdef

  let pp = pp_consdef
  let eq_kind cd_a cd_b = eq_consdef_kind cd_a cd_b
  let specialize = specialize_consdef
end
