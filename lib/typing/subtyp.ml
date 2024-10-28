module Types = Runtime.Types
module Type = Types.Type

let rec sub (typ_a : Type.t) (typ_b : Type.t) : bool =
  let sub_unequal () =
    match (typ_a, typ_b) with
    (* int <: fint and int <: fbit *)
    | IntT, FIntT _ | IntT, FBitT _ -> true
    (* tau <: senum tau, senum tau <: tau, and
       senum tau <: senum tau' if tau <: tau' *)
    | SEnumT (_, typ_inner_a), _ when sub typ_inner_a typ_b -> true
    | _, SEnumT (_, typ_inner_b) when sub typ_a typ_inner_b -> true
    | SEnumT (_, typ_inner_a), SEnumT (_, typ_inner_b)
      when sub typ_inner_a typ_inner_b ->
        true
    (* tuple tau* <: tuple tau'* if (tau <: tau')* *)
    (* seq tau* <: seq tau'* if (tau <: tau')* *)
    | SeqT typs_inner_a, SeqT typs_inner_b
    (* seq tau* <: tuple tau'* if (tau <: tau')* *)
    | SeqT typs_inner_a, TupleT typs_inner_b ->
        List.length typs_inner_a = List.length typs_inner_b
        && List.for_all2 sub typs_inner_a typs_inner_b
    (* seq tau* <: struct id (id', tau')* if (tau <: tau')* and
       seq tau* <: header id (id', tau')* if (tau <: tau')* *)
    | SeqT typs_inner_a, StructT (_, fields_b)
    | SeqT typs_inner_a, HeaderT (_, fields_b) ->
        let typs_inner_b = List.map snd fields_b in
        List.length typs_inner_a = List.length typs_inner_b
        && List.for_all2 sub typs_inner_a typs_inner_b
    (* record (id', tau)* <: struct id (id', tau')* if (tau <: tau')* and
       record (id', tau)* <: header id (id', tau')* if (tau <: tau')* *)
    | RecordT fields_a, StructT (_, fields_b)
    | RecordT fields_a, HeaderT (_, fields_b) ->
        let members_a, typs_inner_a = List.split fields_a in
        let members_b, typs_inner_b = List.split fields_b in
        List.length typs_inner_a = List.length typs_inner_b
        && List.for_all2 ( = ) members_a members_b
        && List.for_all2 sub typs_inner_a typs_inner_b
    | _ -> false
  in
  if Eq.eq_typ_alpha typ_a typ_b then true else sub_unequal ()
