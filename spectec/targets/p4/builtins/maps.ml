open Lang.Xl
open Lang.Il
open Common.Source
open Builtins
open Atom
open Mixfix

(* Value map *)

module VMap = Map.Make (Value)

type map = Value.t VMap.t

(* Conversion between meta-maps and OCaml assoc lists *)

let value_of_map (typ_key : typ) (typ_value : typ) (map : map) : value =
  let value_of_tuple ((value_key, value_value) : value * value) : value =
    let value =
      let typ = Typ.var "pair" [ typ_key; typ_value ] in
      [ Arg value_key; Atom (operator ":" $ no_region); Arg value_value ]
      |> Value.Make.case typ
    in
    value
  in
  let value_pairs =
    let typ_inner = Typ.var "pair" [ typ_key; typ_value ] $ no_region in
    VMap.bindings map |> List.map value_of_tuple |> Value.list typ_inner
  in
  let value =
    let typ = Typ.var "map" [ typ_key; typ_value ] in
    [ Atom (LBrace $ no_region); Arg value_pairs; Atom (RBrace $ no_region) ]
    |> Value.Make.case typ
  in
  value

(* Built-in implementations *)

(* dec $find_map<K, V>(map<K, V>, K) : V? *)

let find_map ~at (_typ_key : targ) (typ_val : targ) (map : map) (key : Value.t)
    =
  at |> ignore;
  let result_opt = VMap.find_opt key map in
  Ok (Value.opt typ_val result_opt)

(* dec $find_maps<K, V>(map<K, V>*, K) : V? *)

let find_maps ~at (_typ_key : targ) (typ_val : targ) (maps : map list)
    (key : Value.t) =
  at |> ignore;
  let result_opt =
    List.fold_left
      (fun value_opt map ->
        match value_opt with
        | Some _ -> value_opt
        | None -> VMap.find_opt key map)
      None maps
  in
  Ok (Value.opt typ_val result_opt)

(* dec $add_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let add_map ~at (typ_key : targ) (typ_val : targ) (map : map) (key : Value.t)
    (v : Value.t) =
  at |> ignore;
  let map = VMap.add key v map in
  Ok (value_of_map typ_key typ_val map)

(* dec $adds_map<K, V>(map<K, V>, K*, V* ) : map<K, V> *)

let adds_map ~at (typ_key : targ) (typ_val : targ) (map : map)
    (keys : Value.t list) (vals : Value.t list) =
  try
    let map = List.fold_left2 (fun m k v -> VMap.add k v m) map keys vals in
    Ok (value_of_map typ_key typ_val map)
  with Invalid_argument _ ->
    let msg =
      Printf.sprintf
        "adds_map: key list length (%d) does not match value list length (%d)"
        (List.length keys) (List.length vals)
    in
    Error (Error.RuntimeError (at, msg))

(* dec $update_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let update_map = add_map

let builtins : (string * Define.t) list =
  [
    ("find_map", Define.T2.a2 Arg.map Arg.value find_map);
    ("find_maps", Define.T2.a2 (Arg.list_of Arg.map) Arg.value (* K *) find_maps);
    ("add_map", Define.T2.a3 Arg.map Arg.value Arg.value add_map);
    ( "adds_map",
      Define.T2.a3 Arg.map (Arg.list_of Arg.value) (Arg.list_of Arg.value)
        adds_map );
    ("update_map", Define.T2.a3 Arg.map Arg.value Arg.value update_map);
  ]
