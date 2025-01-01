open Il.Ast
open Util.Pp

type t =
  | ExternC of tparam list * cparam list * mthd list
  | ParserC of
      tparam list * cparam list * param list * decl list * parser_state list
  | ControlC of tparam list * cparam list * param list * decl list * block
  | PackageC of tparam list * cparam list
  | TableC of table

let pp fmt = function
  | ExternC (tparams, cparams, mthds) ->
      Format.fprintf fmt "ExternC%a%a -> {\n%a\n}" Il.Pp.pp_tparams tparams
        Il.Pp.pp_cparams cparams
        (pp_list (Il.Pp.pp_mthd ~level:1) "\n")
        mthds
  | ParserC (tparams, cparams, params, decls, states) ->
      Format.fprintf fmt "ParserC%a%a -> %a {\n%a\n%a\n}" Il.Pp.pp_tparams
        tparams Il.Pp.pp_cparams cparams Il.Pp.pp_params params
        (pp_list (Il.Pp.pp_decl ~level:1) "\n")
        decls
        (pp_list (Il.Pp.pp_parser_state ~level:1) "\n")
        states
  | ControlC (tparams, cparams, params, decls, block) ->
      Format.fprintf fmt "ControlC%a%a -> %a {\n%a\n%a\n}" Il.Pp.pp_tparams
        tparams Il.Pp.pp_cparams cparams Il.Pp.pp_params params
        (pp_list (Il.Pp.pp_decl ~level:1) "\n")
        decls (Il.Pp.pp_block ~level:1) block
  | PackageC (tparams, cparams) ->
      Format.fprintf fmt "PackageC%a%a" Il.Pp.pp_tparams tparams
        Il.Pp.pp_cparams cparams
  | TableC table ->
      Format.fprintf fmt "TableC %a" (Il.Pp.pp_table ~level:0) table

let eq_kind cons_a cons_b =
  match (cons_a, cons_b) with
  | ExternC _, ExternC _ -> true
  | ParserC _, ParserC _ -> true
  | ControlC _, ControlC _ -> true
  | PackageC _, PackageC _ -> true
  | TableC _, TableC _ -> true
  | _ -> false
