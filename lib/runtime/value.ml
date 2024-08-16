module L = Lang.Ast
module P = Lang.Pp
module F = Format

type t =
  | ErrV of L.member'
  | MatchKindV of L.member'
  | StrV of string
  | BoolV of bool
  | IntV of Bigint.t
  | FIntV of Bigint.t * Bigint.t
  | FBitV of Bigint.t * Bigint.t
  | VBitV of Bigint.t * Bigint.t * Bigint.t
  | TupleV of t list
  | StackV of (t list * Bigint.t * Bigint.t)
  | StructV of (L.member' * t) list
  | HeaderV of bool * (L.member' * t) list
  | UnionV of (L.member' * t) list
  | EnumFieldV of L.id' * L.member'
  | SEnumFieldV of L.id' * L.member' * t

let rec pp fmt value =
  match value with
  | ErrV member -> F.fprintf fmt "error.%a" (P.pp_member' ~level:0) member
  | MatchKindV member -> P.pp_member' ~level:0 fmt member
  | StrV s -> F.pp_print_string fmt s
  | BoolV b -> F.fprintf fmt "%b" b
  | IntV i -> F.fprintf fmt "%a" Bigint.pp i
  | FIntV (width, i) -> F.fprintf fmt "%as%a" Bigint.pp width Bigint.pp i
  | FBitV (width, i) -> F.fprintf fmt "%aw%a" Bigint.pp width Bigint.pp i
  | VBitV (width_max, _, i) ->
      F.fprintf fmt "%av%a" Bigint.pp width_max Bigint.pp i
  | TupleV values -> F.fprintf fmt "tuple { %a }" (P.pp_list pp ", ") values
  | StackV (values, _idx, _size) ->
      F.fprintf fmt "stack { %a }" (P.pp_list pp "; ") values
  | StructV fields ->
      F.fprintf fmt "struct { %a }"
        (P.pp_pairs (P.pp_member' ~level:0) pp "; ")
        fields
  | HeaderV (_valid, fields) ->
      F.fprintf fmt "header { %a }"
        (P.pp_pairs (P.pp_member' ~level:0) pp "; ")
        fields
  | UnionV fields ->
      F.fprintf fmt "header_union { %a }"
        (P.pp_pairs (P.pp_member' ~level:0) pp "; ")
        fields
  | EnumFieldV (id, member) ->
      F.fprintf fmt "%a.%a" P.pp_id' id (P.pp_member' ~level:0) member
  | SEnumFieldV (id, member, value) ->
      F.fprintf fmt "%a.%a(= %a)" P.pp_id' id (P.pp_member' ~level:0) member pp
        value

(* Getters *)

let get_bool t : bool =
  match t with
  | BoolV value -> value
  | _ -> Format.asprintf "Not a bool value: %a" pp t |> failwith

let get_num t : Bigint.t =
  match t with
  | IntV value -> value
  | FIntV (_, value) -> value
  | FBitV (_, value) -> value
  | _ -> Format.asprintf "Not a int/bit value: %a" pp t |> failwith

let rec get_width t =
  match t with
  | BoolV _ -> Bigint.one
  | FIntV (width, _) | FBitV (width, _) | VBitV (_, width, _) -> width
  | TupleV values ->
      List.fold_left
        (fun acc value -> Bigint.(acc + get_width value))
        Bigint.zero values
  | StructV fields | HeaderV (_, fields) ->
      let values = List.map snd fields in
      List.fold_left
        (fun acc value -> Bigint.(acc + get_width value))
        Bigint.zero values
  | _ -> Format.asprintf "Cannot get width of value: %a" pp t |> failwith

let get_tuple t =
  match t with
  | TupleV values -> values
  | _ -> Format.asprintf "Not a tuple value: %a" pp t |> failwith

let get_enum t =
  match t with
  | EnumFieldV (id, member) -> (id, member)
  | _ -> Format.asprintf "Not an enum value: %a" pp t |> failwith
