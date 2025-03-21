(* Numbers: natural numbers and integers *)

type t = [ `Nat of Z.t | `Int of Z.t ]
type typ = [ `NatT | `IntT ]

let to_typ = function `Nat _ -> `NatT | `Int _ -> `IntT
let to_int = function `Nat i | `Int i -> i

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

(* Unary *)

let un (op : unop) num : t =
  match (op, num) with
  | `PlusOp, `Int _ -> num
  | `MinusOp, `Int n -> `Int (Z.neg n)
  | _ -> assert false

let bin (op : binop) num_l num_r : t =
  match (op, num_l, num_r) with
  | `AddOp, `Nat n_l, `Nat n_r -> `Nat Z.(n_l + n_r)
  | `AddOp, `Int i_l, `Int i_r -> `Int Z.(i_l + i_r)
  | `SubOp, `Nat n_l, `Nat n_r when n_l >= n_r -> `Nat Z.(n_l - n_r)
  | `SubOp, `Int i_l, `Int i_r -> `Int Z.(i_l - i_r)
  | `MulOp, `Nat n_l, `Nat n_r -> `Nat Z.(n_l * n_r)
  | `MulOp, `Int i_l, `Int i_r -> `Int Z.(i_l * i_r)
  | `DivOp, `Nat n_l, `Nat n_r when Z.(n_r <> zero && rem n_l n_r = zero) ->
      `Nat Z.(n_l / n_r)
  | `DivOp, `Int i_l, `Int i_r when Z.(i_r <> zero && rem i_l i_r = zero) ->
      `Int Z.(i_l / i_r)
  | `ModOp, `Nat n_l, `Nat n_r when Z.(n_r <> zero) -> `Nat Z.(rem n_l n_r)
  | `ModOp, `Int i_l, `Int i_r when Z.(i_r <> zero) -> `Int Z.(rem i_l i_r)
  | `PowOp, `Nat n_l, `Nat n_r -> `Nat Z.(n_l ** to_int n_r)
  | `PowOp, `Int i_l, `Int i_r when Z.(i_r >= zero && fits_int i_r) ->
      `Int Z.(i_l ** to_int i_r)
  | _, _, _ -> assert false

(* Comparison *)

let cmp (op : cmpop) num_l num_r : bool =
  match (op, num_l, num_r) with
  | `LtOp, `Nat n_l, `Nat n_r -> n_l < n_r
  | `LtOp, `Int i_l, `Int i_r -> i_l < i_r
  | `GtOp, `Nat n_l, `Nat n_r -> n_l > n_r
  | `GtOp, `Int i_l, `Int i_r -> i_l > i_r
  | `LeOp, `Nat n_l, `Nat n_r -> n_l <= n_r
  | `LeOp, `Int i_l, `Int i_r -> i_l <= i_r
  | `GeOp, `Nat n_l, `Nat n_r -> n_l >= n_r
  | `GeOp, `Int i_l, `Int i_r -> i_l >= i_r
  | _, _, _ -> assert false
