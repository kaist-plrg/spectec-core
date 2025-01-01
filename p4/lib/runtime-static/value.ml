module L = Lang.Ast
module P = Lang.Pp
module F = Format
open Util.Pp

type t =
  | ErrV of L.member'
  | MatchKindV of L.member'
  | StrV of string
  | BoolV of bool
  | IntV of Bigint.t
  | FIntV of Bigint.t * Bigint.t
  | FBitV of Bigint.t * Bigint.t
  | VBitV of Bigint.t * Bigint.t * Bigint.t
  | EnumFieldV of L.id' * L.member'
  | SEnumFieldV of L.id' * L.member' * t
  | ListV of t list
  | TupleV of t list
  | StackV of (t list * Bigint.t * Bigint.t)
  | StructV of (L.member' * t) list
  | HeaderV of bool * (L.member' * t) list
  | UnionV of (L.member' * t) list
  | TableEnumFieldV of L.id' * L.member'
  | TableStructV of (L.member' * t) list
  | SeqV of t list
  | SeqDefaultV of t list
  | RecordV of (L.member' * t) list
  | RecordDefaultV of (L.member' * t) list
  | DefaultV
  | InvalidV
  | RefV of Domain.Dom.OId.t

let rec pp ?(level = 0) fmt value =
  level |> ignore;
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
  | EnumFieldV (id, member) ->
      F.fprintf fmt "%a.%a" P.pp_id' id (P.pp_member' ~level:0) member
  | SEnumFieldV (id, member, value) ->
      F.fprintf fmt "%a.%a(= %a)" P.pp_id' id (P.pp_member' ~level:0) member
        (pp ~level:0) value
  | ListV values -> F.fprintf fmt "list { %a }" (pp_list pp ", ") values
  | TupleV values -> F.fprintf fmt "tuple { %a }" (pp_list pp ", ") values
  | StackV (values, _idx, _size) ->
      F.fprintf fmt "stack { %a }" (pp_list pp "; ") values
  | StructV fields ->
      F.fprintf fmt "struct { %a }"
        (pp_pairs (P.pp_member' ~level:0) pp " = " "; ")
        fields
  | HeaderV (_valid, fields) ->
      F.fprintf fmt "header { %a }"
        (pp_pairs (P.pp_member' ~level:0) pp " = " "; ")
        fields
  | UnionV fields ->
      F.fprintf fmt "header_union { %a }"
        (pp_pairs (P.pp_member' ~level:0) pp " = " "; ")
        fields
  | TableEnumFieldV (id, member) ->
      F.fprintf fmt "%a.%a" P.pp_id' id (P.pp_member' ~level:0) member
  | TableStructV fields ->
      F.fprintf fmt "table { %a }"
        (pp_pairs (P.pp_member' ~level:0) pp " = " "; ")
        fields
  | SeqV values -> F.fprintf fmt "seq { %a }" (pp_list pp ", ") values
  | SeqDefaultV values ->
      F.fprintf fmt "seq { %a, ... }" (pp_list pp ", ") values
  | RecordV fields ->
      F.fprintf fmt "record { %a }"
        (pp_pairs (P.pp_member' ~level:0) pp " = " "; ")
        fields
  | RecordDefaultV fields ->
      F.fprintf fmt "record { %a, ... }"
        (pp_pairs (P.pp_member' ~level:0) pp " = " "; ")
        fields
  | DefaultV -> F.pp_print_string fmt "..."
  | InvalidV -> F.pp_print_string fmt "{#}"
  | RefV oid -> F.fprintf fmt "!%a" Domain.Dom.OId.pp oid

(* Equality *)

let rec eq t_a t_b =
  match (t_a, t_b) with
  | ErrV member_a, ErrV member_b | MatchKindV member_a, MatchKindV member_b ->
      member_a = member_b
  | StrV s_a, StrV s_b -> s_a = s_b
  | BoolV b_a, BoolV b_b -> b_a = b_b
  | IntV i_a, IntV i_b -> Bigint.(i_a = i_b)
  | FIntV (width_a, i_a), FIntV (width_b, i_b)
  | FBitV (width_a, i_a), FBitV (width_b, i_b) ->
      Bigint.(width_a = width_b) && Bigint.(i_a = i_b)
  | VBitV (width_max_a, _, i_a), VBitV (width_max_b, _, i_b) ->
      Bigint.(width_max_a = width_max_b) && Bigint.(i_a = i_b)
  | EnumFieldV (id_a, member_a), EnumFieldV (id_b, member_b) ->
      id_a = id_b && member_a = member_b
  | SEnumFieldV (id_a, member_a, value_a), SEnumFieldV (id_b, member_b, value_b)
    ->
      id_a = id_b && member_a = member_b && eq value_a value_b
  | ListV values_a, ListV values_b -> List.for_all2 eq values_a values_b
  | TupleV values_a, TupleV values_b -> List.for_all2 eq values_a values_b
  | StackV (values_a, _, size_a), StackV (values_b, _, size_b) ->
      List.for_all2 eq values_a values_b && Bigint.(size_a = size_b)
  | StructV fields_a, StructV fields_b ->
      List.for_all2
        (fun (member_a, value_a) (member_b, value_b) ->
          member_a = member_b && eq value_a value_b)
        fields_a fields_b
  | HeaderV (valid_a, fields_a), HeaderV (valid_b, fields_b) ->
      valid_a = valid_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             member_a = member_b && eq value_a value_b)
           fields_a fields_b
  | UnionV fields_a, UnionV fields_b ->
      List.for_all2
        (fun (member_a, value_a) (member_b, value_b) ->
          member_a = member_b && eq value_a value_b)
        fields_a fields_b
  | TableEnumFieldV (id_a, member_a), TableEnumFieldV (id_b, member_b) ->
      id_a = id_b && member_a = member_b
  | TableStructV fields_a, TableStructV fields_b ->
      List.for_all2
        (fun (member_a, value_a) (member_b, value_b) ->
          member_a = member_b && eq value_a value_b)
        fields_a fields_b
  | SeqV values_a, SeqV values_b | SeqDefaultV values_a, SeqDefaultV values_b ->
      List.for_all2 eq values_a values_b
  | RecordV fields_a, RecordV fields_b
  | RecordDefaultV fields_a, RecordDefaultV fields_b ->
      List.for_all2
        (fun (member_a, value_a) (member_b, value_b) ->
          member_a = member_b && eq value_a value_b)
        fields_a fields_b
  | DefaultV, DefaultV -> true
  | InvalidV, InvalidV -> true
  | RefV oid_a, RefV oid_b -> oid_a = oid_b
  | _ -> false

(* Getters *)

let get_bool t : bool =
  match t with
  | BoolV value -> value
  | _ -> Format.asprintf "Not a bool value: %a" (pp ~level:0) t |> failwith

let get_num t : Bigint.t =
  match t with
  | IntV value -> value
  | FIntV (_, value) -> value
  | FBitV (_, value) -> value
  | _ -> Format.asprintf "Not a int/bit value: %a" (pp ~level:0) t |> failwith

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
  | _ ->
      Format.asprintf "Cannot get width of value: %a" (pp ~level:0) t
      |> failwith

let get_tuple t =
  match t with
  | TupleV values -> values
  | _ -> Format.asprintf "Not a tuple value: %a" (pp ~level:0) t |> failwith

let get_enum t =
  match t with
  | EnumFieldV (id, member) -> (id, member)
  | _ -> Format.asprintf "Not an enum value: %a" (pp ~level:0) t |> failwith
