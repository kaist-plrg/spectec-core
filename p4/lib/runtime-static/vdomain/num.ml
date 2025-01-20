open Vdom

(* Bitstring manipulation *)

let rec shift_bitstring_left (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    shift_bitstring_left Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let rec shift_bitstring_right (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    let v_shifted = Bigint.(v / (one + one)) in
    shift_bitstring_right v_shifted Bigint.(o - one)
  else v

let shift_bitstring_right_arith (v : Bigint.t) (o : Bigint.t) (m : Bigint.t) :
    Bigint.t =
  let rec shift_bitstring_right_arith' (v : Bigint.t) (o : Bigint.t) : Bigint.t
      =
    if Bigint.(o > zero) then
      let v_shifted = Bigint.((v / (one + one)) + m) in
      shift_bitstring_right_arith' v_shifted Bigint.(o - one)
    else v
  in
  shift_bitstring_right_arith' v o

let power_of_two (w : Bigint.t) : Bigint.t = shift_bitstring_left Bigint.one w

let slice_bitstring (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Bigint.t =
  let slice_width = Bigint.(m + one - l) in
  if Bigint.(l < zero) then
    raise (Invalid_argument "bitslice x[y:z] must have y > z > 0");
  let shifted = Bigint.(n asr to_int_exn l) in
  let mask = Bigint.(power_of_two slice_width - one) in
  Bigint.bit_and shifted mask

let rec bitwise_neg (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  if Bigint.(w > zero) then
    let w' = power_of_two Bigint.(w - one) in
    let g = slice_bitstring n Bigint.(w - one) Bigint.(w - one) in
    if Bigint.(g = zero) then bitwise_neg Bigint.(n + w') Bigint.(w - one)
    else bitwise_neg Bigint.(n - w') Bigint.(w - one)
  else n

(* Value manipulation : two's complement conversions *)

let rec to_two_complement (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = power_of_two w in
  if Bigint.(n >= w' / two) then to_two_complement Bigint.(n - w') w
  else if Bigint.(n < -(w' / two)) then to_two_complement Bigint.(n + w') w
  else n

let rec of_two_complement (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  let w' = power_of_two w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then of_two_complement Bigint.(n + w') w
  else n

let int_of_raw_int (n : Bigint.t) (w : Bigint.t) : t =
  FIntV (w, of_two_complement n w)

let bit_of_raw_int (n : Bigint.t) (w : Bigint.t) : t =
  FBitV (w, of_two_complement n w)

let vbit_of_raw_int (n : Bigint.t) (w : Bigint.t) (mw : Bigint.t) : t =
  VBitV (mw, w, of_two_complement n w)

let raw_int_of_int (n : Bigint.t) (w : Bigint.t) : Bigint.t =
  to_two_complement n w

let raw_int_of_bit (n : Bigint.t) (_w : Bigint.t) : Bigint.t = n

let raw_int_of_value (value : t) : Bigint.t =
  match value with
  | IntV value -> value
  | FIntV (width, value) -> raw_int_of_int value width
  | FBitV (width, value) -> raw_int_of_bit value width
  | VBitV (_, width, value) -> raw_int_of_bit value width
  | _ ->
      Format.asprintf "Not an number value: %a" (Pp.pp ~level:0) value
      |> failwith
