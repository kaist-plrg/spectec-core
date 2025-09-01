open Xl
open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

(* dec $rev_<X>(X* ) : X* *)

let rev_ (at : region) (targs : targ list) (values_input : value list) : value =
  let typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let value =
    let vid = Value.fresh () in
    let typ = Il.Ast.IterT (typ, Il.Ast.List) in
    ListV (List.rev values) $$$ { vid; typ }
  in
  value

(* dec $concat_<X>((X* )* ) : X* *)

let concat_ (at : region) (targs : targ list) (values_input : value list) :
    value =
  let typ = Extract.one at targs in
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.concat_map Value.get_list
  in
  let value =
    let vid = Value.fresh () in
    let typ = Il.Ast.IterT (typ, Il.Ast.List) in
    ListV values $$$ { vid; typ }
  in
  value

(* dec $distinct_<K>(K* ) : bool *)

let distinct_ (at : region) (targs : targ list) (values_input : value list) :
    value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let set = Sets.VSet.of_list values in
  let value =
    let vid = Value.fresh () in
    let typ = Il.Ast.BoolT in
    BoolV (Sets.VSet.cardinal set = List.length values) $$$ { vid; typ }
  in
  value

(* dec $partition_<X>(X*, nat) : (X*, X* ) *)

let partition_ (at : region) (targs : targ list) (values_input : value list) :
    value =
  let typ = Extract.one at targs in
  let value_list, value_len = Extract.two at values_input in
  let values = Value.get_list value_list in
  let len = value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let values_left, values_right =
    values
    |> List.mapi (fun idx value -> (idx, value))
    |> List.partition (fun (idx, _) -> idx < len)
  in
  let value_left =
    let vid = Value.fresh () in
    let typ = Il.Ast.IterT (typ, Il.Ast.List) in
    ListV (List.map snd values_left) $$$ { vid; typ }
  in
  let value_right =
    let vid = Value.fresh () in
    let typ = Il.Ast.IterT (typ, Il.Ast.List) in
    ListV (List.map snd values_right) $$$ { vid; typ }
  in
  let value =
    let vid = Value.fresh () in
    let typ =
      Il.Ast.TupleT
        [ value_left.note.typ $ no_region; value_right.note.typ $ no_region ]
    in
    TupleV [ value_left; value_right ] $$$ { vid; typ }
  in
  value

(* dec $assoc_<X, Y>(X, (X, Y)* ) : Y? *)

let assoc_ (at : region) (targs : targ list) (values_input : value list) : value
    =
  let _typ_key, typ_value = Extract.two at targs in
  let value, value_list = Extract.two at values_input in
  let values =
    Value.get_list value_list
    |> List.map (fun value ->
           match value.it with
           | TupleV [ value_key; value_value ] -> (value_key, value_value)
           | _ -> assert false)
  in
  let value_opt =
    List.fold_left
      (fun value_found (value_key, value_value) ->
        match value_found with
        | Some _ -> value_found
        | None when Value.compare value value_key = 0 -> Some value_value
        | None -> None)
      None values
  in
  let value =
    let vid = Value.fresh () in
    let typ = Il.Ast.IterT (typ_value, Il.Ast.Opt) in
    OptV value_opt $$$ { vid; typ }
  in
  value
