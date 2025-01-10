module L = Lang.Ast
module P = Lang.Pp
module F = Format
open Il.Ast
open Util.Pp

type t =
  | ExternC of L.id' * tparam list * cparam list * mthd list
  | ParserC of
      tparam list * cparam list * param list * decl list * parser_state list
  | ControlC of tparam list * cparam list * param list * decl list * block
  | PackageC of tparam list * cparam list
  | TableC of L.id' * table

let pp ?(level = 0) fmt = function
  | ExternC (id, tparams, cparams, mthds) ->
      F.fprintf fmt "ExternC%a%a -> %a {\n%a\n}" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_cparams ~level:(level + 1))
        cparams P.pp_id' id
        (pp_list ~level:(level + 1) Il.Pp.pp_mthd ~sep:Nl)
        mthds
  | ParserC (tparams, cparams, params, decls, states) ->
      F.fprintf fmt "ParserC%a%a -> %a {\n%a\n%a\n}" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_cparams ~level:(level + 1))
        cparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (Il.Pp.pp_decls ~level:(level + 1))
        decls
        (Il.Pp.pp_parser_states ~level:(level + 1))
        states
  | ControlC (tparams, cparams, params, decls, block) ->
      F.fprintf fmt "ControlC%a%a -> %a {\n%a\n%a\n}" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_cparams ~level:(level + 1))
        cparams
        (Il.Pp.pp_params ~level:(level + 1))
        params
        (Il.Pp.pp_decls ~level:(level + 1))
        decls
        (Il.Pp.pp_block ~level:(level + 1))
        block
  | PackageC (tparams, cparams) ->
      F.fprintf fmt "PackageC%a%a" Il.Pp.pp_tparams tparams
        (Il.Pp.pp_cparams ~level:(level + 1))
        cparams
  | TableC (id, table) ->
      F.fprintf fmt "TableC %a %a" P.pp_id' id
        (Il.Pp.pp_table ~level:(level + 1))
        table

let eq_kind cons_a cons_b =
  match (cons_a, cons_b) with
  | ExternC _, ExternC _ -> true
  | ParserC _, ParserC _ -> true
  | ControlC _, ControlC _ -> true
  | PackageC _, PackageC _ -> true
  | TableC _, TableC _ -> true
  | _ -> false
