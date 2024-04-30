open Ds

type t =
  | VBool of bool
  | VAInt of Bigint.t
  | VInt of { value : Bigint.t; width : Bigint.t }
  | VBit of { value : Bigint.t; width : Bigint.t }
  | VString of string
  | VError
  | VTuple of t list
  | VStruct of { entries : (string * t) list }
  | VHeader of { valid : bool; entries : (string * t) list }
  (* Reference type *)
  (* (TODO) A hack for now, representing pointers to objects *)
  | VRef of Path.t

let rec pp fmt value =
  let pp_entries entries =
    List.map
      (fun (key, value) -> Format.asprintf "%s : %a" key pp value)
      entries
    |> String.concat ", "
  in
  match value with
  | VBool value -> Format.fprintf fmt "%b" value
  | VAInt value -> Format.fprintf fmt "%s" (Bigint.to_string value)
  | VInt { value; width } ->
      Format.fprintf fmt "%ss%s" (Bigint.to_string width)
        (Bigint.to_string value)
  | VBit { value; width } ->
      Format.fprintf fmt "%sw%s" (Bigint.to_string width)
        (Bigint.to_string value)
  | VString value -> Format.fprintf fmt "\"%s\"" value
  | VError -> Format.fprintf fmt "error"
  | VTuple values ->
      Format.fprintf fmt "(%s)"
        (String.concat ", " (List.map (Format.asprintf "%a" pp) values))
  | VStruct { entries } -> Format.fprintf fmt "struct {%s}" (pp_entries entries)
  | VHeader { valid; entries } ->
      Format.fprintf fmt "header (valid %b, {%s})" valid (pp_entries entries)
  | VRef rvalue -> Format.fprintf fmt "*%s" (String.concat "." rvalue)
