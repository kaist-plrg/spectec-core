open El.Ast

module Type = struct
  type t = plaintyp

  let to_string t = El.Print.string_of_plaintyp t
end

module TypeDef = struct
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
end

module Rel = struct
  type t = nottyp * Il.Ast.rule list

  let to_string (nottyp, rules) =
    El.Print.string_of_nottyp nottyp
    ^ " =\n"
    ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
end

module Func = struct
  type t = tparam list * param list * plaintyp * Il.Ast.clause list

  let to_string (tparams, params, plaintyp, clauses) =
    "def "
    ^ El.Print.string_of_tparams tparams
    ^ El.Print.string_of_params params
    ^ " : "
    ^ El.Print.string_of_plaintyp plaintyp
    ^ " =\n"
    ^ String.concat "\n" (List.map Il.Print.string_of_clause clauses)
end
