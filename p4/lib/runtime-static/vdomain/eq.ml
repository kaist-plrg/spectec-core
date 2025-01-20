open Vdom

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
