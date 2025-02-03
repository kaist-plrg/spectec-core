module F = Format
module L = Lang.Ast

type t =
  (* 1. Base values *)
  | ErrV of L.member'
  | MatchKindV of L.member'
  | StrV of string
  | BoolV of bool
  | IntV of Bigint.t
  | FIntV of Bigint.t * Bigint.t
  | FBitV of Bigint.t * Bigint.t
  | VBitV of Bigint.t * Bigint.t * Bigint.t
  (* 2a. Derived values *)
  | EnumFieldV of L.id' * L.member'
  | SEnumFieldV of L.id' * L.member' * t
  | ListV of t list
  | TupleV of t list
  | StackV of (t list * Bigint.t * Bigint.t)
  | StructV of L.id' * (L.member' * t) list
  | HeaderV of L.id' * bool * (L.member' * t) list
  | UnionV of L.id' * (L.member' * t) list
  (* 2b. Derived object reference values *)
  | RefV of Domain.Dom.OId.t
  (* 3. Synthesized values *)
  | TableEnumFieldV of L.id' * L.member'
  | TableStructV of L.id' * (L.member' * t) list
  | SeqV of t list
  | SeqDefaultV of t list
  | RecordV of (L.member' * t) list
  | RecordDefaultV of (L.member' * t) list
  | DefaultV
  | InvalidV
  | SetV of [ `Singleton of t | `Mask of t * t | `Range of t * t ]
  | StateV of L.id'
