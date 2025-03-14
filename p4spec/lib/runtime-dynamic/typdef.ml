open Il.Ast
open Il.Print

(* Type definition *)

type t = tparam list * deftyp

let to_string (tparams, deftyp) =
  string_of_tparams tparams ^ " " ^ string_of_deftyp deftyp
