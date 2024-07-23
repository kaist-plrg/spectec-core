open Syntax.Ast

(* Runtime representation of types *)

type t =
  | VoidT
  | BoolT
  | AIntT
  | IntT of Bigint.t
  | BitT of Bigint.t
  | VBitT of Bigint.t
  | StrT
  | ErrT
  | MatchKindT
  | NameT of id'
  | NewT of id'
  | StackT of (t * Bigint.t)
  | TupleT of t list
  | StructT of (member' * t) list
  | HeaderT of (member' * t) list
  | UnionT of (member' * t) list
  (* (TODO) id' field of EnumT and SEnumT seems redundant,
     but also it may serve some purpose when type checking,
     e.g. enum foo { A, B } and enum bar { A, B } are different types *)
  | EnumT of id' * member' list
  | SEnumT of id' * t * (member' * Value.t) list
  | RefT

let rec pp fmt = function
  | VoidT -> Format.fprintf fmt "void"
  | BoolT -> Format.fprintf fmt "bool"
  | AIntT -> Format.fprintf fmt "int"
  | IntT w -> Format.fprintf fmt "%ss" (Bigint.to_string w)
  | BitT w -> Format.fprintf fmt "%sw" (Bigint.to_string w)
  | VBitT w -> Format.fprintf fmt "%sv" (Bigint.to_string w)
  | StrT -> Format.fprintf fmt "string"
  | ErrT -> Format.fprintf fmt "error"
  | MatchKindT -> Format.fprintf fmt "match_kind"
  | NameT n -> Format.fprintf fmt "%s" n
  | NewT n -> Format.fprintf fmt "new %s" n
  | StackT (t, s) -> Format.fprintf fmt "%a[%s]" pp t (Bigint.to_string s)
  | TupleT ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp)
        ts
  | StructT fs ->
      Format.fprintf fmt "struct { @[<hv>%a@] }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m pp t))
        fs
  | HeaderT fs ->
      Format.fprintf fmt "header { @[<hv>%a@] }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m pp t))
        fs
  | UnionT fs ->
      Format.fprintf fmt "union { @[<hv>%a@] }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m pp t))
        fs
  | EnumT (_, ms) ->
      Format.fprintf fmt "enum { @[<hv>%a@] }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           Format.pp_print_string)
        ms
  | SEnumT (_, t, fs) ->
      Format.fprintf fmt "enum %a { @[<hv>%a@] }" pp t
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m Value.pp t))
        fs
  | RefT -> Format.fprintf fmt "ref"
