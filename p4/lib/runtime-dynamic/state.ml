open Il.Ast
module F = Format
open Util.Pp

type t = block

let pp ?(level = 0) fmt t = F.fprintf fmt "%a" (Il.Pp.pp_block ~level) t
