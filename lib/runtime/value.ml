open Utils

(* Values *)

type base =
  | Bool of bool
  | AInt of int
  | Int of { value: int; width: int }
  | Bit of { value: int; width: int }
  | String of string

type t =
  | Base of base
  | Ref of Path.t


(* Utils *)

let print_base_value (bvalue: base) =
  match bvalue with
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | AInt i -> Printf.sprintf "AInt(%d)" i
  | Int { value; width } ->
      Printf.sprintf "Int(%d, %d)" value width
  | Bit { value; width } ->
      Printf.sprintf "Bit(%d, %d)" value width 
  | String s -> Printf.sprintf "String(%s)" s

let print_value (t: t) =
  match t with
  | Base bvalue -> print_base_value bvalue
  | Ref rvalue -> Printf.sprintf "Ref(%s)" (String.concat "." rvalue)
