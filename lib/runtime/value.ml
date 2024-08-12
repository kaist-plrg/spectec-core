open Syntax.Ast

(* Runtime representation of values *)

type t =
  | BoolV of bool
  | IntV of Bigint.t
  | FIntV of Bigint.t * Bigint.t
  | FBitV of Bigint.t * Bigint.t
  | VBitV of Bigint.t * Bigint.t * Bigint.t
  | StrV of string
  | ErrV of member'
  | MatchKindV of member'
  | StackV of (t list * Bigint.t * Bigint.t)
  | TupleV of t list
  | StructV of (member' * t) list
  | HeaderV of bool * (member' * t) list
  | UnionV of (member' * t) list
  | EnumFieldV of id' * member'
  | SEnumFieldV of id' * member' * t
  | StateV of id'
  | RefV of path'

let rec pp fmt = function
  | BoolV b -> Format.fprintf fmt "%b" b
  | IntV i -> Format.fprintf fmt "%s" (Bigint.to_string i)
  | FIntV (w, i) ->
      Format.fprintf fmt "%ss%s" (Bigint.to_string w) (Bigint.to_string i)
  | FBitV (w, i) ->
      Format.fprintf fmt "%sw%s" (Bigint.to_string w) (Bigint.to_string i)
  | VBitV (_mw, w, i) ->
      Format.fprintf fmt "%sv%s" (Bigint.to_string w) (Bigint.to_string i)
  | StrV s -> Format.fprintf fmt "\"%s\"" s
  | ErrV s -> Format.fprintf fmt "%s" s
  | MatchKindV s -> Format.fprintf fmt "%s" s
  | StackV (vs, _i, s) ->
      Format.fprintf fmt "%a[%s]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp)
        vs (Bigint.to_string s)
  | TupleV vs ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp)
        vs
  | StructV fs ->
      Format.fprintf fmt "struct { @[<hv>%a@] }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, v) -> Format.fprintf fmt "%s: %a" m pp v))
        fs
  | HeaderV (v, fs) ->
      Format.fprintf fmt "header { %s, @[<hv>%a@] }"
        (if v then "valid" else "invalid")
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, v) -> Format.fprintf fmt "%s: %a" m pp v))
        fs
  | UnionV fs ->
      Format.fprintf fmt "union { @[<hv>%a@] }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (m, v) -> Format.fprintf fmt "%s: %a" m pp v))
        fs
  | EnumFieldV (_, m) -> Format.fprintf fmt "%s" m
  | SEnumFieldV (_, m, v) -> Format.fprintf fmt "%s(%a)" m pp v
  | StateV s -> Format.fprintf fmt "state %s" s
  | RefV p -> Format.fprintf fmt "ref %s" (String.concat "." p)

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

let get_state t =
  match t with
  | StateV state -> state
  | _ -> Format.asprintf "Not a state value: %a" pp t |> failwith

(* Aggregate accessors *)

let access_field (member : member') t =
  match t with
  | StructV fields -> List.assoc member fields
  | _ ->
      Format.asprintf "Cannot access field %s of value: %a" member pp t
      |> failwith
