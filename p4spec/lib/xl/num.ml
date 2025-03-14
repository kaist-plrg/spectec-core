(* Numbers: natural numbers and integers *)

type t = [ `Nat of Z.t | `Int of Z.t ]
type typ = [ `NatT | `IntT ]

let to_typ = function `Nat _ -> `NatT | `Int _ -> `IntT

(* Operations *)

type unop = [ `PlusOp | `MinusOp ]
type binop = [ `AddOp | `SubOp | `MulOp | `DivOp | `ModOp | `PowOp ]
type cmpop = [ `LtOp | `GtOp | `LeOp | `GeOp ]

(* Equality *)

let eq (n_a : t) (n_b : t) : bool =
  match (n_a, n_b) with
  | `Nat n_a, `Nat n_b -> Z.equal n_a n_b
  | `Int i_a, `Int i_b -> Z.equal i_a i_b
  | _, _ -> false

(* Subtyping *)

let equiv typ_a typ_b = typ_a = typ_b

let sub typ_a typ_b =
  match (typ_a, typ_b) with `NatT, `IntT -> true | _, _ -> equiv typ_a typ_b

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
