(* Types *)

type t =
  | Bool
  | AInt
  | Int of { width : Bigint.t }
  | Bit of { width : Bigint.t }
  | VBit of { width : Bigint.t }
  | Array of { typ : t; size : Bigint.t }
  | String
  | Error
  | Tuple of t list
  | Enum of { entries : string list }
  | SEnum of { typ : t; entries : string list }
  | Header of { entries : (string * t) list }
  | Union of { entries : (string * t) list }
  | Struct of { entries : (string * t) list }
  | Name of { name : string }
  | NewType of { name : string }
  (* (TODO) A hack for now, representing objects *)
  | Ref

(* Printer *)

let rec print (t : t) =
  match t with
  | Bool -> "bool"
  | AInt -> "int"
  | Int { width } -> Printf.sprintf "int<%s>" (Bigint.to_string width)
  | Bit { width } -> Printf.sprintf "bit<%s>" (Bigint.to_string width)
  | VBit { width } -> Printf.sprintf "varbit<%s>" (Bigint.to_string width)
  | Array { typ; size } ->
      Printf.sprintf "array<%s>[%s]" (print typ) (Bigint.to_string size)
  | String -> "string"
  | Error -> "error"
  | Tuple typs ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map print typs))
  | Enum { entries } -> Printf.sprintf "enum{%s}" (String.concat ", " entries)
  | SEnum { typ; entries } ->
      Printf.sprintf "enum<%s>{%s}" (print typ) (String.concat ", " entries)
  | Header { entries } ->
      Printf.sprintf "header{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print typ))
              entries))
  | Union { entries } ->
      Printf.sprintf "union{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print typ))
              entries))
  | Struct { entries } ->
      Printf.sprintf "struct{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print typ))
              entries))
  | Name { name } -> name
  | NewType { name } -> name
  | Ref -> "ref"
