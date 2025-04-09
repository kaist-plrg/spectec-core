module Value = Runtime_value.Value
module Type = Il.Tdomain.Types.Type

(* Size evaluation *)

(* (9) Compile-time size determination

   The definition of e.minSizeInBits() and e.maxSizeInBits() is
   given recursively on the type of e as described in the following table:

   Type         |	minSizeInBits                                          | maxSizeInBits
   bit<N>       |	N	                                                     | N
   int<N>	      | N	                                                     | N
   bool	        | 1	                                                     | 1
   enum bit<N>  | N                                                      | N
   enum int<N>  | N                                                      | N
   tuple	      | foreach field(tuple) sum of	field.minSizeInBits()      | foreach field(tuple) sum of field.maxSizeInBits()
   varbit<N>    |	0                                                      | N
   struct       | foreach field(struct) sum of field.minSizeInBits()     | foreach field(struct) sum of field.maxSizeInBits()
   header       | foreach field(header) sum of field.minSizeInBits()     | foreach field(header) sum of field.maxSizeInBits()
   H[N]	        | N * H.minSizeInBits()                                  | N * H.maxSizeInBits()
   header_union	| max(foreach field(header_union)	field.minSizeInBits()) | max(foreach field(header_union) field.maxSizeInBits())

   The methods can also be applied to type name expressions e:

    * if the type of e is a type introduced by type, the result is
      the application of the method to the underlying type *)

let rec min_size_in_bits' (typ : Type.t) : Bigint.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | BoolT -> Bigint.one
  | FIntT width | FBitT width -> width
  | VBitT _ -> Bigint.zero
  | NewT (_, typ_inner) -> min_size_in_bits' typ_inner
  | SEnumT (_, typ_inner, _) -> min_size_in_bits' typ_inner
  | TupleT typs_inner ->
      List.fold_left
        (fun size typ_inner -> Bigint.(size + min_size_in_bits' typ_inner))
        Bigint.zero typs_inner
  | StackT (typ_inner, size) -> Bigint.(size * min_size_in_bits' typ_inner)
  | StructT (_, fields) | HeaderT (_, fields) ->
      List.fold_left
        (fun size (_, typ_inner) -> Bigint.(size + min_size_in_bits' typ_inner))
        Bigint.zero fields
  | UnionT (_, fields) ->
      let sizes =
        List.map (fun (_, typ_inner) -> min_size_in_bits' typ_inner) fields
      in
      List.fold_left Bigint.min Bigint.zero sizes
  | _ ->
      Format.asprintf "(TODO) Size of type %a undefined" (Type.pp ~level:0) typ
      |> failwith

let min_size_in_bits (typ : Type.t) : Value.t =
  let size = min_size_in_bits' typ in
  IntV size

let min_size_in_bytes (typ : Type.t) : Value.t =
  let size = min_size_in_bits' typ in
  let size = Bigint.((size + of_int 7) asr 3) in
  IntV size

let rec max_size_in_bits' (typ : Type.t) : Bigint.t =
  let typ = Type.canon typ in
  match typ with
  | SpecT _ | DefT _ -> assert false
  | BoolT -> Bigint.one
  | FIntT width | FBitT width | VBitT width -> width
  | NewT (_, typ_inner) -> max_size_in_bits' typ_inner
  | SEnumT (_, typ_inner, _) -> max_size_in_bits' typ_inner
  | TupleT typs_inner ->
      List.fold_left
        (fun size typ_inner -> Bigint.(size + max_size_in_bits' typ_inner))
        Bigint.zero typs_inner
  | StackT (typ_inner, size) -> Bigint.(size * max_size_in_bits' typ_inner)
  | StructT (_, fields) | HeaderT (_, fields) ->
      List.fold_left
        (fun size (_, typ_inner) -> Bigint.(size + max_size_in_bits' typ_inner))
        Bigint.zero fields
  | UnionT (_, fields) ->
      let sizes =
        List.map (fun (_, typ_inner) -> max_size_in_bits' typ_inner) fields
      in
      List.fold_left Bigint.max Bigint.zero sizes
  | _ ->
      Format.asprintf "(TODO) Size of type %a undefined" (Type.pp ~level:0) typ
      |> failwith

let max_size_in_bits (typ : Type.t) : Value.t =
  let size = max_size_in_bits' typ in
  IntV size

let max_size_in_bytes (typ : Type.t) : Value.t =
  let size = max_size_in_bits' typ in
  let size = Bigint.((size + of_int 7) asr 3) in
  IntV size

let size (typ : Type.t) (member : string) : Value.t =
  match member with
  | "minSizeInBits" -> min_size_in_bits typ
  | "minSizeInBytes" -> min_size_in_bytes typ
  | "maxSizeInBits" -> max_size_in_bits typ
  | "maxSizeInBytes" -> max_size_in_bytes typ
  | _ -> assert false
