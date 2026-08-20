open Lang.Pl
open Lang.Pl.Print

(* Function *)

type t =
  | Builtin
  | Defined of tparam list * arg list * block * elseblock option

let to_string = function
  | Builtin -> "builtin function"
  | Defined (tparams, args, block, elseblock_opt) -> (
      "def " ^ string_of_tparams tparams ^ string_of_args args ^ " :\n\n"
      ^ string_of_block block
      ^
      match elseblock_opt with
      | None -> ""
      | Some elseblock -> "\n\notherwise\n\n" ^ string_of_block elseblock)
