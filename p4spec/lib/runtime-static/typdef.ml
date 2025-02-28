open El.Ast
open El.Print

(* Type definitions *)

type t =
  | Param
  | Defining of tparam list
  | Defined of
      tparam list
      * [ `Plain of plaintyp
        | `Struct of typfield list
        | `Variant of nottyp list ]

let to_string = function
  | Param -> "Param"
  | Defining tparams -> "Defining" ^ string_of_tparams tparams
  | Defined (tparams, typdef) -> (
      "Defined" ^ string_of_tparams tparams ^ " = "
      ^
      match typdef with
      | `Plain plaintyp -> string_of_plaintyp plaintyp
      | `Struct typfields -> string_of_typfields ", " typfields
      | `Variant nottyps -> string_of_nottyps " | " nottyps)

let get_tparams = function
  | Param -> []
  | Defining tparams -> tparams
  | Defined (tparams, _) -> tparams
