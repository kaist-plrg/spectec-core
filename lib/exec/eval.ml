open Runtime.Base
open Runtime.Context

let rec eval_simplify_type (ctx : Ctx.t) (typ : Type.t) : Type.t =
  match typ with
  | NameT name -> Ctx.find_td name ctx |> Option.get |> eval_simplify_type ctx
  | NewT name -> Ctx.find_td name ctx |> Option.get
  | _ -> typ

let rec unpack_width (value : Value.t) =
  match value with
  | BoolV _ -> Bigint.one
  | IntV (width, _) | BitV (width, _) | VBitV (_, width, _) -> width
  | TupleV values ->
      List.fold_left
        (fun acc value -> Bigint.(acc + unpack_width value))
        Bigint.zero values
  | StructV fields | HeaderV (_, fields) ->
      let values = List.map snd fields in
      List.fold_left
        (fun acc value -> Bigint.(acc + unpack_width value))
        Bigint.zero values
  | _ -> assert false

let unpack_value (value : Value.t) : Bigint.t =
  match value with
  | BoolV b -> if b then Bigint.one else Bigint.zero
  | AIntV value | IntV (_, value) | BitV (_, value) | VBitV (_, _, value) ->
      value
  | _ -> assert false
