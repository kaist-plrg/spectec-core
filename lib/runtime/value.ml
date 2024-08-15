module Ast = Common.Ast

type t =
  | BoolV of bool
  | IntV of Bigint.t
  | FIntV of Bigint.t * Bigint.t
  | FBitV of Bigint.t * Bigint.t
  | VBitV of Bigint.t * Bigint.t * Bigint.t
  | StrV of string
  | ErrV of Ast.member'
  | MatchKindV of Ast.member'
  | StackV of (t list * Bigint.t * Bigint.t)
  | TupleV of t list
  | StructV of (Ast.member' * t) list
  | HeaderV of bool * (Ast.member' * t) list
  | UnionV of (Ast.member' * t) list
  | EnumFieldV of Ast.id' * Ast.member'
  | SEnumFieldV of Ast.id' * Ast.member' * t
