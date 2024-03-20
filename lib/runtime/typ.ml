open Utils

(* Values *)

type base =
  | Bool
  | AInt
  | Int of { width : Bigint.t }
  | Bit of { width : Bigint.t }
  | VBit of { width : Bigint.t }
  | Array of { typ : base; size : Bigint.t }
  | String
  | Error
  | Tuple of base list
  | Enum of { entries : string list }
  | SEnum of { typ : base; entries : string list }
  | Header of { entries : (string * base) list }
  | Union of { entries : (string * base) list }
  | Struct of { entries : (string * base) list }
  | Name of { name : string }
  | NewType of { name : string }

type t = Base of base | Ref of Path.t

(* Printers *)

let rec print_base (bvalue : base) =
  match bvalue with
  | Bool -> "bool"
  | AInt -> "int"
  | Int { width } -> Printf.sprintf "int<%s>" (Bigint.to_string width)
  | Bit { width } -> Printf.sprintf "bit<%s>" (Bigint.to_string width)
  | VBit { width } -> Printf.sprintf "varbit<%s>" (Bigint.to_string width)
  | Array { typ; size } ->
      Printf.sprintf "array<%s>[%s]" (print_base typ) (Bigint.to_string size)
  | String -> "string"
  | Error -> "error"
  | Tuple typs ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map print_base typs))
  | Enum { entries } -> Printf.sprintf "enum{%s}" (String.concat ", " entries)
  | SEnum { typ; entries } ->
      Printf.sprintf "enum<%s>{%s}" (print_base typ)
        (String.concat ", " entries)
  | Header { entries } ->
      Printf.sprintf "header{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print_base typ))
              entries))
  | Union { entries } ->
      Printf.sprintf "union{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print_base typ))
              entries))
  | Struct { entries } ->
      Printf.sprintf "struct{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print_base typ))
              entries))
  | Name { name } -> name
  | NewType { name } -> name

let print (t : t) =
  match t with
  | Base btyp -> print_base btyp
  | Ref rtyp -> Printf.sprintf "Ref(%s)" (String.concat "." rtyp)

(* Utils *)

let extract_base (typ : t) : base =
  match typ with
  | Base btyp -> btyp
  | _ -> Printf.sprintf "Not a base type: %s" (print typ) |> failwith
