module Types = Runtime.Types
module Type = Types.Type

(* (8.11.1) Explicit casts

   The following casts are legal in P4:

    - bit<1> ↔ bool:
        converts the value 0 to false, the value 1 to true, and vice versa.
    - int → bool:
        only if the int value is 0 (converted to false) or 1 (converted to true)
    - int<W> → bit<W>:
        preserves all bits unchanged and reinterprets negative values as positive values
    - bit<W> → int<W>:
        preserves all bits unchanged and reinterprets values whose most-significant bit is 1 as negative values
    - bit<W> → bit<X>:
        truncates the value if W > X, and otherwise (i.e., if W <= X) pads the value with zero bits.
    - int<W> → int<X>:
        truncates the value if W > X, and otherwise (i.e., if W < X) extends it with the sign bit.
    - bit<W> → int:
        preserves the value unchanged but converts it to an unlimited-precision integer;
        the result is always non-negative
    - int<W> → int:
        preserves the value unchanged but converts it to an unlimited-precision integer;
        the result may be negative
    - int → bit<W>:
        converts the integer value into a sufficiently large two's complement bit string to avoid information loss,
        and then truncates the result to W bits. The compiler should emit a warning on
        overflow or on conversion of negative value.
    - int → int<W>:
        converts the integer value into a sufficiently-large two's complement bit string to avoid information loss,
        and then truncates the result to W bits. The compiler should emit a warning on overflow.
    - casts between two types that are introduced by typedef and are equivalent to one of the above combinations.
    - casts between a typedef and the original type.
    - casts between a type introduced by type and the original type.
    - casts between an enum with an explicit type and its underlying type
    - casts of a key-value list to a struct type or a header type (see Section 8.13)
    - casts of a tuple expression to a header stack type
    - casts of an invalid expression {#} to a header or a header union type
    - casts where the destination type is the same as the source type
      if the destination type appears in this list (this excludes e.g., parsers or externs). *)

let rec explicit (typ_from : Type.t) (typ_to : Type.t) : bool =
  let explicit_unequal () =
    match (typ_from, typ_to) with
    (* bit<1> ↔ bool *)
    | FBitT width_from, BoolT when width_from = Bigint.one -> true
    | BoolT, FBitT width_to when width_to = Bigint.one -> true
    (* int → bool *)
    | IntT, BoolT -> true
    (* int<W> → bit<W> *)
    | FIntT width_from, FBitT width_to when width_from = width_to -> true
    (* bit<W> → int<W> *)
    | FBitT width_from, FIntT width_to when width_from = width_to -> true
    (* bit<W> → bit<X> *)
    | FBitT _, FBitT _ -> true
    (* int<W> → int<X> *)
    | FIntT _, FIntT _ -> true
    (* bit<W> → int *)
    | FBitT _, IntT -> true
    (* int<W> → int *)
    | FIntT _, IntT -> true
    (* int<W> → int *)
    | IntT, FBitT _ -> true
    (* int → bit<W> *)
    | IntT, FIntT _ -> true
    (* casts between two types that are introduced by typedef and
       are equivalent to one of the above combinations *)
    (* casts between a typedef and the original type *)
    (* casts between a type introduced by type and the original type *)
    | NewT (_, typ_from_inner), _ when explicit typ_from_inner typ_to -> true
    | _, NewT (_, typ_to_inner) when explicit typ_from typ_to_inner -> true
    (* casts between an enum with an explicit type and its underlying type *)
    | SEnumT (_, typ_from_inner), _ when explicit typ_from_inner typ_to -> true
    | _, SEnumT (_, typ_to_inner) when explicit typ_from typ_to_inner -> true
    (* casts of a tuple expression to a list, tuple, struct, or header type *)
    | SeqT typs_from_inner, ListT typ_to_inner ->
        List.for_all
          (fun typ_from_inner -> explicit typ_from_inner typ_to_inner)
          typs_from_inner
    | SeqT typs_from_inner, TupleT typs_to_inner ->
        List.length typs_from_inner = List.length typs_to_inner
        && List.for_all2 explicit typs_from_inner typs_to_inner
    | SeqT typs_from_inner, StructT (_, fields_to)
    | SeqT typs_from_inner, HeaderT (_, fields_to) ->
        let typs_to_inner = List.map snd fields_to in
        List.length typs_from_inner = List.length typs_to_inner
        && List.for_all2 explicit typs_from_inner typs_to_inner
    (* casts of a key-value list to a struct type or a header type (see Section 8.13) *)
    | RecordT fields_from, StructT (_, fields_to)
    | RecordT fields_from, HeaderT (_, fields_to) ->
        let compare (member_a, _) (member_b, _) = compare member_a member_b in
        let members_from, typs_from_inner =
          List.sort compare fields_from |> List.split
        in
        let members_to, typs_to_inner =
          List.sort compare fields_to |> List.split
        in
        List.for_all2 ( = ) members_from members_to
        && List.for_all2 explicit typs_from_inner typs_to_inner
    (* casts of an invalid expression {#} to a header or a header union type *)
    | InvalidT, HeaderT _ | InvalidT, UnionT _ -> true
    (* (TODO) casts where the destination type is the same as the source type
       if the destination type appears in this list (this excludes e.g., parsers or externs). *)
    | _ -> false
  in
  if Eq.eq_typ_alpha typ_from typ_to then true else explicit_unequal ()

(* (8.11.2) Implicit casts

   To keep the language simple and avoid introducing hidden costs, P4 only implicitly casts from int to fixed-width types
   and from enums with an underlying type to the underlying type. In particular, applying a binary operation (except shifts
   and concatenation) to an expression of type int and an expression with a fixed-width type will implicitly cast the int
   expression to the type of the other expression. For enums with an underlying type, it can be implicitly cast to its underlying
   type whenever appropriate, including but not limited to in shifts, concatenation, bit slicing indexes,
   header stack indexes as well as other unary and binary operations.

   The compiler also adds implicit casts when types of different expressions need to match;
   for example, as described in Section 13.6, since select labels are compared against the selected expression,
   the compiler will insert implicit casts for the select labels when they have int types.
   Similarly, when assigning a structure-valued expression to a structure or header, the compiler will add implicit casts for int fields. *)

(* (8.12) Operations on tuple expressions

   A tuple may be used to initialize a structure if the tuple has the same number of elements
   as fields in the structure. The effect of such an initializer is to assign the nth element
   of the tuple to the nth field in the structure.

   A tuple expression can have an explicit structure or header type specified, and then it is
   converted automatically to a structure-valued expression (see 8.13). *)

let rec implicit (typ_a : Type.t) (typ_b : Type.t) : bool =
  let implicit_unequal () =
    match (typ_a, typ_b) with
    (* int <: fint and int <: fbit *)
    | IntT, FIntT _ | IntT, FBitT _ -> true
    (* tau <: senum tau, senum tau <: tau, and
       senum tau <: senum tau' if tau <: tau' *)
    | SEnumT (_, typ_a_inner), _ when implicit typ_a_inner typ_b -> true
    | _, SEnumT (_, typ_b_inner) when implicit typ_a typ_b_inner -> true
    (* seq tau* <: list tau' if (tau <: tau')* *)
    | SeqT typs_a_inner, ListT typ_b_inner ->
        List.for_all
          (fun typ_a_inner -> implicit typ_a_inner typ_b_inner)
          typs_a_inner
    (* seq tau* <: tuple tau'* if (tau <: tau')* *)
    | SeqT typs_a_inner, TupleT typs_b_inner ->
        List.length typs_a_inner = List.length typs_b_inner
        && List.for_all2 implicit typs_a_inner typs_b_inner
    (* seq tau* <: struct id (id', tau')* if (tau <: tau')* and
       seq tau* <: header id (id', tau')* if (tau <: tau')* *)
    | SeqT typs_a_inner, StructT (_, fields_b)
    | SeqT typs_a_inner, HeaderT (_, fields_b) ->
        let typs_b_inner = List.map snd fields_b in
        List.length typs_a_inner = List.length typs_b_inner
        && List.for_all2 implicit typs_a_inner typs_b_inner
    (* seq tau* <: seq tau'* if (tau <: tau')* *)
    | SeqT typs_a_inner, SeqT typs_b_inner ->
        List.length typs_a_inner = List.length typs_b_inner
        && List.for_all2 implicit typs_a_inner typs_b_inner
    (* record (id', tau)* <: struct id (id', tau')* if (tau <: tau')* and
       record (id', tau)* <: header id (id', tau')* if (tau <: tau')* *)
    | RecordT fields_a, StructT (_, fields_b)
    | RecordT fields_a, HeaderT (_, fields_b) ->
        let compare (member_a, _) (member_b, _) = compare member_a member_b in
        let members_a, typs_a_inner =
          List.sort compare fields_a |> List.split
        in
        let members_b, typs_b_inner =
          List.sort compare fields_b |> List.split
        in
        List.length typs_a_inner = List.length typs_b_inner
        && List.for_all2 ( = ) members_a members_b
        && List.for_all2 implicit typs_a_inner typs_b_inner
    (* invalid <: header _ and invalid <: union *)
    | InvalidT, HeaderT _ | InvalidT, UnionT _ -> true
    | _ -> false
  in
  if Eq.eq_typ_alpha typ_a typ_b then true else implicit_unequal ()
