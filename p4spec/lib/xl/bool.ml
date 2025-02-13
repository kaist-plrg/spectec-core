(* Boolens *)

type t = [ `BoolT ]
type typ = [ `BoolT ]

(* Operations *)

type unop = [ `NotOp ]
type binop = [ `AndOp | `OrOp | `ImplOp | `EquivOp ]
type cmpop = [ `EqOp | `NeOp ]

(* Stringifiers *)

let string_of_bool = string_of_bool
let string_of_unop = function `NotOp -> "~"

let string_of_binop = function
  | `AndOp -> "/\\"
  | `OrOp -> "\\/"
  | `ImplOp -> "=>"
  | `EquivOp -> "<=>"

let string_of_cmpop = function `EqOp -> "=" | `NeOp -> "=/="
