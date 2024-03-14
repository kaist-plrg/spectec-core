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

let print_base_value (bvalue: base) =
  match bvalue with
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | AInt i -> Printf.sprintf "AInt(%s)" (Bigint.to_string i)
  | Int { value; width } ->
      Printf.sprintf "Int(%s, %s)"
        (Bigint.to_string value) (Bigint.to_string width)
  | Bit { value; width } ->
      Printf.sprintf "Bit(%s, %s)"
        (Bigint.to_string value) (Bigint.to_string width)
  | String s -> Printf.sprintf "String(%s)" s

let print_value (t: t) =
  match t with
  | Base bvalue -> print_base_value bvalue
  | Ref rvalue -> Printf.sprintf "Ref(%s)" (String.concat "." rvalue)
