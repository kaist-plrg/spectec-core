open Utils

(* Values *)

type base =
  | Bool of bool
  | AInt of Bigint.t
  | Int of { value: Bigint.t; width: Bigint.t }
  | Bit of { value: Bigint.t; width: Bigint.t }
  | String of string

type t =
  | Base of base
  | Ref of Path.t


(* Utils *)

let print_base (bvalue: base) =
  match bvalue with
  | Bool value -> Printf.sprintf "Bool(%b)" value
  | AInt value -> Printf.sprintf "AInt(%s)" (Bigint.to_string value)
  | Int { value; width } ->
      Printf.sprintf "Int(%s, %s)"
        (Bigint.to_string value) (Bigint.to_string width)
  | Bit { value; width } ->
      Printf.sprintf "Bit(%s, %s)"
        (Bigint.to_string value) (Bigint.to_string width)
  | String value -> Printf.sprintf "String(%s)" value

let print (t: t) =
  match t with
  | Base bvalue -> print_base bvalue
  | Ref rvalue -> Printf.sprintf "Ref(%s)" (String.concat "." rvalue)
