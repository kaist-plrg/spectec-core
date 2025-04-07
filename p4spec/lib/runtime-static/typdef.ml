open El.Ast
open El.Print (* Type definitions *)

[@@@ocamlformat "disable"]

type t =
  (* Type parameter *)
  | Param
  (* Type being defined *)
  | Defining of tparam list
  (* Type that is completely defined *)
  | Defined of
      tparam list
      * [ `Plain of plaintyp (* Plain type *)
        | `Struct of typfield list (* Struct type *)
        | `Variant of (nottyp * plaintyp) list
          (* Variant type that is fully expanded, with the second `plaintyp` field
             indicating the type of the variant for subtyping purposes *) ]
[@@@ocamlformat "enable"]

let to_string = function
  | Param -> "Param"
  | Defining tparams -> "Defining" ^ string_of_tparams tparams
  | Defined (tparams, typdef) -> (
      "Defined" ^ string_of_tparams tparams ^ " = "
      ^
      match typdef with
      | `Plain plaintyp -> string_of_plaintyp plaintyp
      | `Struct typfields -> string_of_typfields ", " typfields
      | `Variant typcases ->
          let nottyps = List.map fst typcases in
          string_of_nottyps " | " nottyps)

let get_tparams = function
  | Param -> []
  | Defining tparams -> tparams
  | Defined (tparams, _) -> tparams
