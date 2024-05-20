type t =
  | BoolV of bool
  | AIntV of Bigint.t
  | IntV of Bigint.t * Bigint.t
  | BitV of Bigint.t * Bigint.t
  | StrV of string
  | ErrV of string
  | TupleV of t list
  | StructV of (string * t) list
  | HeaderV of bool * (string * t) list
  | RefV of string

let rec pp fmt value =
  match value with
  | BoolV b -> Format.fprintf fmt "%b" b
  | AIntV i -> Format.fprintf fmt "%s" (Bigint.to_string i)
  | IntV (w, i) ->
      Format.fprintf fmt "%ss%s" (Bigint.to_string w) (Bigint.to_string i)
  | BitV (w, i) ->
      Format.fprintf fmt "%sw%s" (Bigint.to_string w) (Bigint.to_string i)
  | StrV s -> Format.fprintf fmt "\"%s\"" s
  | ErrV s -> Format.fprintf fmt "%s" s
  | TupleV vs ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp)
        vs
  | StructV fs ->
      Format.fprintf fmt "struct { %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (f, v) ->
             Format.fprintf fmt "%s: %a" f pp v))
        fs
  | HeaderV (v, fs) ->
      Format.fprintf fmt "header { %s, %a }"
        (if v then "valid" else "invalid")
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (f, v) ->
             Format.fprintf fmt "%s: %a" f pp v))
        fs
  | RefV s -> Format.fprintf fmt "ref %s" s
