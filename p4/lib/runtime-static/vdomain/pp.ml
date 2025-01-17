module F = Format
module P = Lang.Pp
open Vdom
open Util.Pp

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
