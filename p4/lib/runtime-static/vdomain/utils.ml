module L = Lang.Ast
open Vdom

(* Getters *)

let rec get_width t : Bigint.t =
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
      Format.asprintf "Cannot get width of value: %a" (Pp.pp ~level:0) t
      |> failwith

let get_bool t : bool =
  match t with
  | BoolV value -> value
  | _ -> Format.asprintf "Not a bool value: %a" (Pp.pp ~level:0) t |> failwith

let get_num t : Bigint.t = Num.raw_int_of_value t

let get_tuple t : t list =
  match t with
  | TupleV values -> values
  | _ -> Format.asprintf "Not a tuple value: %a" (Pp.pp ~level:0) t |> failwith

let get_seq t : t list =
  match t with
  | SeqV values -> values
  | _ ->
      Format.asprintf "Not a sequence value: %a" (Pp.pp ~level:0) t |> failwith

let get_tuple_or_seq t : t list =
  match t with
  | TupleV values | SeqV values -> values
  | _ ->
      Format.asprintf "Not a tuple or sequence value: %a" (Pp.pp ~level:0) t
      |> failwith

let get_struct t : L.id' * (L.member' * t) list =
  match t with
  | StructV (id, fields) -> (id, fields)
  | _ -> Format.asprintf "Not a struct value: %a" (Pp.pp ~level:0) t |> failwith

let get_struct_field t member_target : t =
  match t with
  | StructV (_, fields) -> List.assoc member_target fields
  | _ -> Format.asprintf "Not a struct value: %a" (Pp.pp ~level:0) t |> failwith

let get_header t : L.id' * bool * (L.member' * t) list =
  match t with
  | HeaderV (id, valid, fields) -> (id, valid, fields)
  | _ -> Format.asprintf "Not a header value: %a" (Pp.pp ~level:0) t |> failwith

let get_header_valid t : bool =
  match t with
  | HeaderV (_, valid, _) -> valid
  | _ -> Format.asprintf "Not a header value: %a" (Pp.pp ~level:0) t |> failwith

let get_header_field t member_target : t =
  match t with
  | HeaderV (_, _, fields) -> List.assoc member_target fields
  | _ -> Format.asprintf "Not a header value: %a" (Pp.pp ~level:0) t |> failwith

let get_state t : L.id' =
  match t with
  | StateV id -> id
  | _ -> Format.asprintf "Not a state value: %a" (Pp.pp ~level:0) t |> failwith

(* Setters *)

let set_header_invalid t =
  match t with
  | HeaderV (id, _, fields) -> HeaderV (id, false, fields)
  | _ -> Format.asprintf "Not a header value: %a" (Pp.pp ~level:0) t |> failwith

let set_union_invalid t =
  match t with
  | UnionV (id, fields) ->
      let fields =
        List.map
          (fun (member, value) -> (member, set_header_invalid value))
          fields
      in
      UnionV (id, fields)
  | _ -> Format.asprintf "Not a union value: %a" (Pp.pp ~level:0) t |> failwith

let set_invalid t =
  match t with
  | HeaderV _ -> set_header_invalid t
  | UnionV _ -> set_union_invalid t
  | _ ->
      Format.asprintf "Not a header or union value: %a" (Pp.pp ~level:0) t
      |> failwith

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
  | _ -> Format.asprintf "Not a struct value: %a" (Pp.pp ~level:0) t |> failwith

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
  | _ -> Format.asprintf "Not a header value: %a" (Pp.pp ~level:0) t |> failwith

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
              else (member, set_header_invalid value))
            fields
        else fields
      in
      UnionV (id, fields)
  | _ -> Format.asprintf "Not a union value: %a" (Pp.pp ~level:0) t |> failwith
