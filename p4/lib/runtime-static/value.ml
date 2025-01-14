module L = Lang.Ast
module P = Lang.Pp
module F = Format
open Util.Pp

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
  (* 2. Derived values *)
  | EnumFieldV of L.id' * L.member'
  | SEnumFieldV of L.id' * L.member' * t
  | ListV of t list
  | TupleV of t list
  | StackV of (t list * Bigint.t * Bigint.t)
  | StructV of L.id' * (L.member' * t) list
  | HeaderV of L.id' * bool * (L.member' * t) list
  | UnionV of L.id' * (L.member' * t) list
  (* 2c. Derived object reference values *)
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

let rec pp ?(level = 0) fmt value =
  match value with
  | ErrV member -> F.fprintf fmt "error.%a" P.pp_member' member
  | MatchKindV member -> F.fprintf fmt "%a" P.pp_member' member
  | StrV s -> F.fprintf fmt "%s" s
  | BoolV b -> F.fprintf fmt "%b" b
  | IntV i -> F.fprintf fmt "%a" Bigint.pp i
  | FIntV (width, i) -> F.fprintf fmt "%as%a" Bigint.pp width Bigint.pp i
  | FBitV (width, i) -> F.fprintf fmt "%aw%a" Bigint.pp width Bigint.pp i
  | VBitV (width_max, _, i) ->
      F.fprintf fmt "%av%a" Bigint.pp width_max Bigint.pp i
  | EnumFieldV (id, member) ->
      F.fprintf fmt "%a.%a" P.pp_id' id P.pp_member' member
  | SEnumFieldV (id, member, value) ->
      F.fprintf fmt "%a.%a(= %a)" P.pp_id' id P.pp_member' member (pp ~level)
        value
  | ListV values -> F.fprintf fmt "list { %a }" (pp_list pp ~sep:Comma) values
  | TupleV values -> F.fprintf fmt "tuple { %a }" (pp_list pp ~sep:Comma) values
  | StackV (values, _idx, _size) ->
      F.fprintf fmt "stack { %a }" (pp_list pp ~sep:Comma) values
  | StructV (id, fields) ->
      F.fprintf fmt "struct %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member' pp ~rel:Eq ~sep:SemicolonNl)
        fields (indent level)
  | HeaderV (id, valid, fields) ->
      F.fprintf fmt "header %a (%b) {\n%a\n%s}" P.pp_id' id valid
        (pp_pairs ~level:(level + 1) P.pp_member' pp ~rel:Eq ~sep:SemicolonNl)
        fields (indent level)
  | UnionV (id, fields) ->
      F.fprintf fmt "header_union %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member' pp ~rel:Eq ~sep:SemicolonNl)
        fields (indent level)
  | RefV oid -> F.fprintf fmt "ref %a" Domain.Dom.OId.pp oid
  | TableEnumFieldV (id, member) ->
      F.fprintf fmt "%a.%a" P.pp_id' id P.pp_member' member
  | TableStructV (id, fields) ->
      F.fprintf fmt "table_struct %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member' pp ~rel:Eq ~sep:SemicolonNl)
        fields (indent level)
  | SeqV values -> F.fprintf fmt "seq { %a }" (pp_list pp ~sep:Comma) values
  | SeqDefaultV values ->
      F.fprintf fmt "seq { %a, ... }" (pp_list pp ~sep:Comma) values
  | RecordV fields ->
      F.fprintf fmt "record {\n%a\n%s}"
        (pp_pairs ~level:(level + 1) P.pp_member' pp ~rel:Eq ~sep:SemicolonNl)
        fields (indent level)
  | RecordDefaultV fields ->
      F.fprintf fmt "record {\n%a\n%s, ...\n%s}"
        (pp_pairs ~level:(level + 1) P.pp_member' pp ~rel:Eq ~sep:SemicolonNl)
        fields
        (indent (level + 1))
        (indent level)
  | DefaultV -> F.fprintf fmt "..."
  | InvalidV -> F.fprintf fmt "{#}"
  | SetV (`Singleton value) -> F.fprintf fmt "set { %a }" (pp ~level) value
  | SetV (`Mask (value, mask)) ->
      F.fprintf fmt "set { %a &&& %a }" (pp ~level) value (pp ~level) mask
  | SetV (`Range (value_lb, value_ub)) ->
      F.fprintf fmt "set { %a .. %a }" (pp ~level) value_lb (pp ~level) value_ub
  | StateV id -> F.fprintf fmt "state %a" P.pp_id' id

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
  | StructV (id_a, fields_a), StructV (id_b, fields_b) ->
      id_a = id_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             member_a = member_b && eq value_a value_b)
           fields_a fields_b
  | HeaderV (id_a, valid_a, fields_a), HeaderV (id_b, valid_b, fields_b) ->
      id_a = id_b && valid_a = valid_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             member_a = member_b && eq value_a value_b)
           fields_a fields_b
  | UnionV (id_a, fields_a), UnionV (id_b, fields_b) ->
      id_a = id_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             member_a = member_b && eq value_a value_b)
           fields_a fields_b
  | RefV oid_a, RefV oid_b -> oid_a = oid_b
  | TableEnumFieldV (id_a, member_a), TableEnumFieldV (id_b, member_b) ->
      id_a = id_b && member_a = member_b
  | TableStructV (id_a, fields_a), TableStructV (id_b, fields_b) ->
      id_a = id_b
      && List.for_all2
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
  | SetV (`Singleton value_a), SetV (`Singleton value_b) -> eq value_a value_b
  | SetV (`Mask (value_a, mask_a)), SetV (`Mask (value_b, mask_b)) ->
      eq value_a value_b && eq mask_a mask_b
  | ( SetV (`Range (value_lb_a, value_ub_a)),
      SetV (`Range (value_lb_b, value_ub_b)) ) ->
      eq value_lb_a value_lb_b && eq value_ub_a value_ub_b
  | StateV id_a, StateV id_b -> id_a = id_b
  | _ -> false

(* Getters *)

let rec get_width t =
  match t with
  | BoolV _ -> Bigint.one
  | FIntV (width, _) | FBitV (width, _) | VBitV (_, width, _) -> width
  | TupleV values ->
      List.fold_left
        (fun acc value -> Bigint.(acc + get_width value))
        Bigint.zero values
  | StructV (_, fields) | HeaderV (_, _, fields) ->
      let values = List.map snd fields in
      List.fold_left
        (fun acc value -> Bigint.(acc + get_width value))
        Bigint.zero values
  | _ ->
      Format.asprintf "Cannot get width of value: %a" (pp ~level:0) t
      |> failwith

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

let get_tuple t : t list =
  match t with
  | TupleV values -> values
  | _ -> Format.asprintf "Not a tuple value: %a" (pp ~level:0) t |> failwith

let get_struct t : L.id' * (L.member' * t) list =
  match t with
  | StructV (id, fields) -> (id, fields)
  | _ -> Format.asprintf "Not a struct value: %a" (pp ~level:0) t |> failwith

let get_struct_field t member_target : t =
  match t with
  | StructV (_, fields) -> List.assoc member_target fields
  | _ -> Format.asprintf "Not a struct value: %a" (pp ~level:0) t |> failwith

let get_header t : L.id' * bool * (L.member' * t) list =
  match t with
  | HeaderV (id, valid, fields) -> (id, valid, fields)
  | _ -> Format.asprintf "Not a header value: %a" (pp ~level:0) t |> failwith

let get_header_field t member_target : t =
  match t with
  | HeaderV (_, _, fields) -> List.assoc member_target fields
  | _ -> Format.asprintf "Not a header value: %a" (pp ~level:0) t |> failwith

let get_header_valid t : bool =
  match t with
  | HeaderV (_, valid, _) -> valid
  | _ -> Format.asprintf "Not a header value: %a" (pp ~level:0) t |> failwith

let get_state t : L.id' =
  match t with
  | StateV id -> id
  | _ -> Format.asprintf "Not a state value: %a" (pp ~level:0) t |> failwith

(* Setters *)

let set_header_valid t valid =
  match t with
  | HeaderV (id, _, fields) -> HeaderV (id, valid, fields)
  | _ -> Format.asprintf "Not a header value: %a" (pp ~level:0) t |> failwith

(* Updaters *)

let update_struct_field t member_target value_target =
  match t with
  | StructV (id, fields) ->
      let fields =
        List.map
          (fun (member, value) ->
            if member = member_target then (member, value_target)
            else (member, value))
          fields
      in
      StructV (id, fields)
  | _ -> Format.asprintf "Not a struct value: %a" (pp ~level:0) t |> failwith

let update_header_field t member_target value_target =
  match t with
  | HeaderV (id, valid, fields) ->
      let fields =
        List.map
          (fun (member, value) ->
            if member = member_target then (member, value_target)
            else (member, value))
          fields
      in
      HeaderV (id, valid, fields)
  | _ -> Format.asprintf "Not a header value: %a" (pp ~level:0) t |> failwith

let update_union_field t member_target value_target =
  match t with
  | UnionV (id, fields) ->
      let valid_target = get_header_valid value_target in
      let valid_prev = List.assoc member_target fields |> get_header_valid in
      let fields =
        List.map
          (fun (member, value) ->
            if member = member_target then (member, value_target)
            else (member, value))
          fields
      in
      let fields =
        if ((not valid_prev) && valid_target) || (valid_prev && not valid_target)
        then
          List.map
            (fun (member, value) ->
              if member = member_target then (member, value)
              else (member, set_header_valid value false))
            fields
        else fields
      in
      UnionV (id, fields)
  | _ -> Format.asprintf "Not a union value: %a" (pp ~level:0) t |> failwith
