type t =
  | TBool
  | TAInt
  | TInt of { width : Bigint.t }
  | TBit of { width : Bigint.t }
  | TVBit of { width : Bigint.t }
  | TArray of { typ : t; size : Bigint.t }
  | TString
  | TError
  | TTuple of t list
  | TEnum of { entries : string list }
  | TSEnum of { typ : t; entries : string list }
  | THeader of { entries : (string * t) list }
  | TUnion of { entries : (string * t) list }
  | TStruct of { entries : (string * t) list }
  | TName of { name : string }
  | TNewType of { name : string }
  (* Reference type *)
  (* (TODO) A hack for now, representing objects *)
  | TRef

let rec pp fmt typ =
  let pp_entries entries =
    List.map (fun (key, typ) -> Format.asprintf "%s : %a" key pp typ) entries
    |> String.concat ", "
  in
  match typ with
  | TBool -> Format.fprintf fmt "bool"
  | TAInt -> Format.fprintf fmt "int"
  | TInt { width } -> Format.fprintf fmt "int<%s>" (Bigint.to_string width)
  | TBit { width } -> Format.fprintf fmt "bit<%s>" (Bigint.to_string width)
  | TVBit { width } -> Format.fprintf fmt "varbit<%s>" (Bigint.to_string width)
  | TArray { typ; size } ->
      Format.fprintf fmt "array<%s>[%s]"
        (Format.asprintf "%a" pp typ)
        (Bigint.to_string size)
  | TString -> Format.fprintf fmt "string"
  | TError -> Format.fprintf fmt "error"
  | TTuple typs ->
      Format.fprintf fmt "(%s)"
        (String.concat ", " (List.map (Format.asprintf "%a" pp) typs))
  | TEnum { entries } ->
      Format.fprintf fmt "enum {%s}" (String.concat ", " entries)
  | TSEnum { typ; entries } ->
      Format.fprintf fmt "enum<%s> {%s}"
        (Format.asprintf "%a" pp typ)
        (String.concat ", " entries)
  | THeader { entries } -> Format.fprintf fmt "header {%s}" (pp_entries entries)
  | TUnion { entries } -> Format.fprintf fmt "union {%s}" (pp_entries entries)
  | TStruct { entries } -> Format.fprintf fmt "struct {%s}" (pp_entries entries)
  | TName { name } -> Format.fprintf fmt "%s" name
  | TNewType { name } -> Format.fprintf fmt "%s" name
  | TRef -> Format.fprintf fmt "ref"
