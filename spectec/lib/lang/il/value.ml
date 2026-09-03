open Types
open Common.Source

type t = value

let rec compare (value_l : t) (value_r : t) =
  if value_l == value_r then 0 else compare_struct value_l value_r

and compare_struct (value_l : t) (value_r : t) =
  let tag (value : t) =
    match value.it with
    | BoolV _ -> 0
    | NumV _ -> 1
    | TextV _ -> 2
    | StructV _ -> 3
    | CaseV _ -> 4
    | TupleV _ -> 5
    | OptV _ -> 6
    | ListV _ -> 7
    | FuncV _ -> 8
  in
  match (value_l.it, value_r.it) with
  | BoolV b_l, BoolV b_r -> Stdlib.compare b_l b_r
  | NumV n_l, NumV n_r -> Xl.Num.compare n_l n_r
  | TextV s_l, TextV s_r -> String.compare s_l s_r
  | StructV fields_l, StructV fields_r ->
      let atoms_l, values_l = List.split fields_l in
      let atoms_r, values_r = List.split fields_r in
      let cmp_atoms =
        List.compare (fun a b -> Xl.Atom.compare a.it b.it) atoms_l atoms_r
      in
      if cmp_atoms <> 0 then cmp_atoms else compares values_l values_r
  | CaseV vc_l, CaseV vc_r -> Mixfix.compare ~compare_arg:compare vc_l vc_r
  | TupleV values_l, TupleV values_r -> compares values_l values_r
  | OptV value_opt_l, OptV value_opt_r -> (
      match (value_opt_l, value_opt_r) with
      | Some value_l, Some value_r -> compare value_l value_r
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None -> 0)
  | ListV values_l, ListV values_r -> compares values_l values_r
  | _ -> Int.compare (tag value_l) (tag value_r)

and compares (values_l : t list) (values_r : t list) : int =
  match (values_l, values_r) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | value_l :: values_l, value_r :: values_r ->
      let cmp = compare value_l value_r in
      if cmp <> 0 then cmp else compares values_l values_r

let eq (value_l : t) (value_r : t) : bool = compare value_l value_r = 0

(* Vid provider signature *)
module type VidProvider = sig
  val fresh : unit -> vid
end

(* Global mutable vid provider for shared use across parsing and interpretation *)
module GlobalVidProvider : sig
  val with_provider : (unit -> vid) -> (unit -> 'a) -> 'a
  val fresh : unit -> vid
end = struct
  let provider : (unit -> vid) ref = ref (fun () -> 0)

  let with_provider p f =
    let previous = !provider in
    provider := p;
    Fun.protect f ~finally:(fun () -> provider := previous)

  let fresh () = !provider ()
end

(* Functor for creating value module with custom vid provider *)
module MakeWithVid (VidProvider : VidProvider) = struct
  (* Incremental hashing: compute hash from value' using child vhash values 
     -> O(width) not O(tree-size) *)
  let hash_of (v : value') : int =
    let ( +! ) h1 h2 = (h1 * 65599) + h2 in
    let hash_atom (atom : Xl.Atom.t) : int = Hashtbl.hash atom in

    let hash_num (num : Xl.Num.t) : int =
      match num with
      | `Nat n -> 0 +! Bigint.hash n
      | `Int i -> 1 +! Bigint.hash i
    in

    match v with
    | BoolV b -> 0 +! Hashtbl.hash b
    | NumV n -> 1 +! hash_num n
    | TextV s -> 2 +! Hashtbl.hash s
    | StructV fields ->
        List.fold_left
          (fun hash (atom, v) ->
            hash +! (hash_atom atom.Common.Source.it +! v.note.vhash))
          3 fields
    | CaseV vc ->
        List.fold_left
          (fun h p ->
            match p with
            | Mixfix.Arg v -> h +! v.note.vhash
            | Mixfix.Atom atom -> h +! 1 +! hash_atom atom.Common.Source.it)
          4 vc
    | TupleV values ->
        List.fold_left (fun hash v -> hash +! v.note.vhash) 5 values
    | OptV None -> 6
    | OptV (Some v) -> 7 +! v.note.vhash
    | ListV values ->
        List.fold_left (fun hash v -> hash +! v.note.vhash) 8 values
    | FuncV id -> 9 +! Hashtbl.hash id.Common.Source.it

  let with_fresh_vid (typ : typ') (vhash : int) : vnote =
    let vid = VidProvider.fresh () in
    { vid; vhash; typ }

  let make_val (typ : typ') (v : value') : t =
    let vhash = hash_of v in
    v $$$ with_fresh_vid typ vhash

  module Make = struct
    let value (t' : typ') (v : value') : t = make_val t' v
    let bool (t' : typ') (b : bool) : t = make_val t' (BoolV b)
    let num (t' : typ') (n : num) : t = make_val t' (NumV n)
    let nat (t' : typ') (n : Bigint.t) : t = make_val t' (NumV (`Nat n))
    let int (t' : typ') (n : Bigint.t) : t = make_val t' (NumV (`Int n))
    let text (t' : typ') (s : string) : t = make_val t' (TextV s)
    let tuple (t' : typ') (vs : t list) : t = make_val t' (TupleV vs)

    let record (t' : typ') (fs : valuefield list) : value =
      make_val t' (StructV fs)

    let opt (t' : typ') (v : t option) : t = make_val t' (OptV v)
    let list (t' : typ') (vs : t list) : t = make_val t' (ListV vs)
    let case (t' : typ') (vc : valuecase) : t = make_val t' (CaseV vc)
  end

  (* Re-export other functions that need vid *)
  let bool (b : bool) : t = Make.bool Typ.bool b
  let nat (i : Bigint.t) : t = Make.nat Typ.nat i
  let int (i : Bigint.t) : t = Make.int Typ.int i
  let text (s : string) : t = Make.text Typ.text s
  let func (id : id) : t = FuncV id |> make_val Typ.func

  let record (tid : string) (fields : valuefield list) : t =
    Make.record (Typ.var tid []) fields

  let tuple (vs : t list) : t =
    let typs = List.map (fun v -> v.note.typ $ no_region) vs in
    TupleV vs |> make_val (Typ.tuple typs)

  let opt (typ : typ) (v : t option) : t = OptV v |> make_val (Typ.opt typ)
  let list (typ : typ) (vs : t list) : t = ListV vs |> make_val (Typ.list typ)
  let list' (typ : typ') (vs : t list) : t = list (typ $ no_region) vs
end

(* Default instance using global provider *)
module DefaultVidProvider = struct
  let fresh () = GlobalVidProvider.fresh ()
end

(* Default module instance *)
module Default = MakeWithVid (DefaultVidProvider)

(* Export default as main module for backward compatibility *)
include Default

let get_bool (value : t) =
  match value.it with BoolV b -> b | _ -> failwith "get_bool"

let get_num (value : t) =
  match value.it with NumV n -> n | _ -> failwith "get_num"

let get_text (value : t) =
  match value.it with TextV s -> s | _ -> failwith "get_text"

let get_list (value : t) =
  match value.it with ListV values -> values | _ -> failwith "unseq"

let get_opt (value : t) =
  match value.it with OptV value -> value | _ -> failwith "get_opt"

let get_struct (value : t) =
  match value.it with StructV fields -> fields | _ -> failwith "get_struct"

let bool (b : bool) : t = Make.bool Typ.bool b
let nat (i : Bigint.t) : t = Make.nat Typ.nat i
let int (i : Bigint.t) : t = Make.int Typ.int i
let text (s : string) : t = Make.text Typ.text s
let func (id : id) : t = FuncV id |> make_val Typ.func

let tuple (vs : t list) : t =
  let typs = List.map (fun v -> v.note.typ $ no_region) vs in
  TupleV vs |> make_val (Typ.tuple typs)

let opt (typ : typ) (v : t option) : t = OptV v |> make_val (Typ.opt typ)
let list (typ : typ) (vs : t list) : t = ListV vs |> make_val (Typ.list typ)

let id_of_case_v (v : t) : string =
  match (v.it, v.note.typ) with
  | CaseV _, VarT { synid; _ } -> synid.it
  | _ -> failwith "not a case value"

let flatten_case_v (value : t) : string * string list * t list =
  match (value.it, value.note.typ) with
  | CaseV valuecase, VarT { synid; _ } ->
      let shape, values = Mixfix.split valuecase in
      let atoms =
        Mixfix.atoms shape
        |> List.map (fun a -> Xl.Atom.to_string a.Common.Source.it)
      in
      (synid.it, atoms, values)
  | _ -> failwith "Expected a CaseV value"

let flatten_case_v' (value : t) : string * string list * value' list =
  let id, atoms, values = flatten_case_v value in
  (id, atoms, List.map (fun (v : t) -> v.it) values)
