open Domain

(* Runtime representation of values *)

module Value = struct
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
             (fun fmt (f, v) -> Format.fprintf fmt "%s: %a" f pp v))
          fs
    | HeaderV (v, fs) ->
        Format.fprintf fmt "header { %s, %a }"
          (if v then "valid" else "invalid")
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, v) -> Format.fprintf fmt "%s: %a" f pp v))
          fs
    | RefV s -> Format.fprintf fmt "ref %s" s
end

(* Runtime representation of types *)

module Type = struct
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
end

module ValueTypePair = struct
  type t = Value.t * Type.t

  let pp fmt (v, t) = Format.fprintf fmt "(%a, %a)" Value.pp v Type.pp t
end

(* Visibility of variables *)

module VVis = MakeVis (Var)

(* Environments map variables to typedefs/values/types *)

module TDEnv = MakeEnv (Var) (Type)
module VTEnv = MakeEnv (Var) (ValueTypePair)
