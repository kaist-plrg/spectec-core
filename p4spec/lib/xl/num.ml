(* Numbers: natural numbers and integers *)

type t = [ `Nat of Z.t | `Int of Z.t ]
type typ = [ `NatT | `IntT ]

let to_typ = function `Nat _ -> `NatT | `Int _ -> `IntT

(* Operations *)

type unop = [ `PlusOp | `MinusOp ]
type binop = [ `AddOp | `SubOp | `MulOp | `DivOp | `ModOp | `PowOp ]
type cmpop = [ `LtOp | `GtOp | `LeOp | `GeOp ]

(* Subtyping *)

let sub typ_a typ_b =
  match (typ_a, typ_b) with `NatT, `IntT -> true | _, _ -> typ_a = typ_b

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
