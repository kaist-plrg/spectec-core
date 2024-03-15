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
  | Bool value -> Printf.sprintf "%b" value
  | AInt value -> Printf.sprintf "%s" (Bigint.to_string value)
  | Int { value; width } ->
      Printf.sprintf "%ss%s"
        (Bigint.to_string width) (Bigint.to_string value)
  | Bit { value; width } ->
      Printf.sprintf "%sw%s"
        (Bigint.to_string width) (Bigint.to_string value)
  | String value -> Printf.sprintf "\"%s\"" value

let print (t: t) =
  match t with
  | Base bvalue -> print_base bvalue
  | Ref rvalue -> Printf.sprintf "Ref(%s)" (String.concat "." rvalue)
