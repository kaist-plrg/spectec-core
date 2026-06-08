open Lang.Il
open Common.Source
open Error
open Mixfix

let ( let* ) = Result.bind

type 'a t = region -> Value.t -> 'a result

let value : Value.t t = fun _at v -> Ok v

let text : string t =
 fun at v ->
  match v.it with
  | TextV s -> Ok s
  | _ -> Error (TypeError (at, "Expected TextV", v))

let bool : bool t =
 fun at v ->
  match v.it with
  | BoolV b -> Ok b
  | _ -> Error (type_err at "Expected BoolV" v)

let nat : Bigint.t t =
 fun at v ->
  match v.it with
  | NumV (`Nat n) -> Ok n
  | _ -> Error (type_err at "Expected Nat NumV" v)

let int : Bigint.t t =
 fun at v ->
  match v.it with
  | NumV (`Int n) -> Ok n
  | _ -> Error (type_err at "Expected Int NumV" v)

let num : num t =
 fun at v ->
  match v.it with NumV n -> Ok n | _ -> Error (type_err at "Expected NumV" v)

let result_all (l : 'a result list) : 'a list result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs -> (
        match x with Ok y -> aux (y :: acc) xs | Error e -> Error e)
  in
  aux [] l

let pair : (Value.t * Value.t) t =
 fun at v ->
  match v.it with
  | TupleV [ a; b ] -> Ok (a, b)
  | _ -> Error (type_err at "Expected a 2-tuple" v)

let list_of (p : 'a t) : 'a list t =
 fun at v ->
  match v.it with
  | ListV vs -> result_all (List.map (p at) vs)
  | _ -> Error (type_err at "Expected ListV" v)

module VSet = Set.Make (Value)

let set : VSet.t t =
 fun at v ->
  match v.it with
  | CaseV [ Atom { it = LBrace; _ }; Arg values; Atom { it = RBrace; _ } ] -> (
      match values.it with
      | ListV vs -> Ok (VSet.of_list vs)
      | _ -> Error (type_err at "Expected set's inner value to be a list" v))
  | _ -> Error (type_err at "Expected set notation `{ ... }" v)

module VMap = Map.Make (Value)

(** Parses a single 'k:v' pair (CaseV) *)
let colon_pair : (Value.t * Value.t) t =
 fun at v ->
  match v.it with
  | CaseV [ Arg key; Atom sep; Arg value ]
    when Lang.Xl.Atom.is_operator sep.it ":" ->
      Ok (key, value)
  | _ -> Error (type_err at "Expected a k ':' v pair" v)

(** Parses a map value into an OCaml VMap.t *)
let map : Value.t VMap.t t =
 fun at v ->
  let open Mixfix in
  match v.it with
  | CaseV [ Atom { it = LBrace; _ }; Arg value; Atom { it = RBrace; _ } ] ->
      let* pairs = (list_of colon_pair) at value in
      Ok (VMap.of_list pairs)
  | _ -> Error (type_err at "Expected map notation `{ ... }" v)
