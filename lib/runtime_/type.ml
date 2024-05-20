type t =
  | BoolT
  | AIntT
  | IntT of Bigint.t
  | BitT of Bigint.t
  | StrT
  | ErrT
  | TupleT of t list
  | StructT of (string * t) list
  | HeaderT of (string * t) list
  | RefT

let rec pp fmt typ =
  match typ with
  | BoolT -> Format.fprintf fmt "bool"
  | AIntT -> Format.fprintf fmt "int"
  | IntT w -> Format.fprintf fmt "%ss" (Bigint.to_string w)
  | BitT w -> Format.fprintf fmt "%sw" (Bigint.to_string w)
  | StrT -> Format.fprintf fmt "string"
  | ErrT -> Format.fprintf fmt "error"
  | TupleT ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp)
        ts
  | StructT fs ->
      Format.fprintf fmt "struct { %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (f, t) -> Format.fprintf fmt "%s: %a" f pp t))
        fs
  | HeaderT fs ->
      Format.fprintf fmt "header { %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (f, t) -> Format.fprintf fmt "%s: %a" f pp t))
        fs
  | RefT -> Format.fprintf fmt "ref"
