module Dim = struct
  type t = Il.Ast.iter list

  let equiv dim_a dim_b =
    List.length dim_a = List.length dim_b && List.for_all2 ( = ) dim_a dim_b

  let sub dim_a dim_b =
    List.length dim_a <= List.length dim_b
    && List.for_all2 ( = ) dim_a
         (List.filteri (fun idx _ -> idx < List.length dim_a) dim_b)

  let to_string t =
    match t with
    | [] -> "()"
    | _ -> t |> List.map Il.Print.string_of_iter |> String.concat ", "
end

module Type = struct
  type t = El.Ast.plaintyp

  let to_string t = El.Print.string_of_plaintyp t
end

module TypeDef = struct
  type t =
    | Param
    | Defining of El.Ast.tparam list
    | Defined of
        El.Ast.tparam list
        * [ `Plain of El.Ast.plaintyp
          | `Struct of El.Ast.typfield list
          | `Variant of El.Ast.nottyp list ]

  let to_string = function
    | Param -> "Param"
    | Defining tparams -> "Defining" ^ El.Print.string_of_tparams tparams
    | Defined (tparams, typdef) -> (
        "Defined"
        ^ El.Print.string_of_tparams tparams
        ^ " = "
        ^
        match typdef with
        | `Plain plaintyp -> El.Print.string_of_plaintyp plaintyp
        | `Struct typfields -> El.Print.string_of_typfields ", " typfields
        | `Variant nottyps -> El.Print.string_of_nottyps " | " nottyps)

  let get_tparams = function
    | Param -> []
    | Defining tparams -> tparams
    | Defined (tparams, _) -> tparams
end

module Rel = struct
  type t = El.Ast.nottyp * int list * Il.Ast.rule list

  let to_string (nottyp, inputs, rules) =
    El.Print.string_of_nottyp nottyp
    ^ " hint(input "
    ^ String.concat ", " (List.map string_of_int inputs)
    ^ ") =\n"
    ^ String.concat "\n   " (List.map Il.Print.string_of_rule rules)
end

module Func = struct
  type t =
    El.Ast.tparam list
    * El.Ast.param list
    * El.Ast.plaintyp
    * Il.Ast.clause list

  let to_string (tparams, params, plaintyp, clauses) =
    "def "
    ^ El.Print.string_of_tparams tparams
    ^ El.Print.string_of_params params
    ^ " : "
    ^ El.Print.string_of_plaintyp plaintyp
    ^ " =\n"
    ^ String.concat "\n" (List.map Il.Print.string_of_clause clauses)
end
