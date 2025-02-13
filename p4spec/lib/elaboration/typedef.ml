open El.Ast

type t = Param | Defining of tparam list | Defined of tparam list * deftyp

let to_string = function
  | Param -> "Param"
  | Defining tparams -> "Defining" ^ El.Print.string_of_tparams tparams
  | Defined (tparams, deftyp) ->
      "Defined"
      ^ El.Print.string_of_tparams tparams
      ^ " = "
      ^ El.Print.string_of_deftyp deftyp

let get_tparams = function
  | Param -> []
  | Defining tparams -> tparams
  | Defined (tparams, _) -> tparams
