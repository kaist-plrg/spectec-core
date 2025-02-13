(* Numbers: natural numbers and integers *)

type t = [ `Nat of Z.t | `Int of Z.t ]
type typ = [ `NatT | `IntT ]

(* Operations *)

type unop = [ `PlusOp | `MinusOp ]
type binop = [ `AddOp | `SubOp | `MulOp | `DivOp | `ModOp | `PowOp ]
type cmpop = [ `LtOp | `GtOp | `LeOp | `GeOp ]

(* Stringifiers *)

let string_of_num = function
  | `Nat n -> Z.to_string n
  | `Int i -> (if i >= Z.zero then "+" else "-") ^ Z.to_string (Z.abs i)

let string_of_typ = function `NatT -> "nat" | `IntT -> "int"
let string_of_unop = function `PlusOp -> "+" | `MinusOp -> "-"

let string_of_binop = function
  | `AddOp -> "+"
  | `SubOp -> "-"
  | `MulOp -> "*"
  | `DivOp -> "/"
  | `ModOp -> "\\"
  | `PowOp -> "^"

let string_of_cmpop = function
  | `LtOp -> "<"
  | `GtOp -> ">"
  | `LeOp -> "<="
  | `GeOp -> ">="
