open Lang.Pl
open Lang.Pl.Print

(* Relation *)

type t = (exp, unit) Lang.Il.Mode.t * block * elseblock option

let to_string (mode, block, elseblock_opt) =
  Lang.Il.Mode.render_inputs ~sep:", " ~string_of_arg:string_of_exp mode
  ^ "\n\n" ^ string_of_block block
  ^
  match elseblock_opt with
  | None -> ""
  | Some elseblock -> "\n\notherwise\n\n" ^ string_of_block elseblock
