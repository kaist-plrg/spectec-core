open Xl
open Sl.Ast
module Typ = Runtime_dynamic_sl.Typ
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
open Util.Source

(* Option monad *)

let ( let* ) x f = Option.bind x f

(* Helpers for wrapping values *)

let wrap_value (typ : typ') (value_opt : value' option) : value option =
  Option.map (fun value -> value $$$ { vid = -1; typ }) value_opt

(* Generate random values from a type *)

let rec gen_from_typ (depth : int) (tdenv : TDEnv.t) (typ : typ) : value option
    =
  if depth <= 0 then None else gen_from_typ' depth tdenv typ

and gen_from_typ' (depth : int) (tdenv : TDEnv.t) (typ : typ) : value option =
  let depth = depth - 1 in
  match typ.it with
  | BoolT ->
      [ BoolV true; BoolV false ] |> Rand.random_select |> wrap_value typ.it
  | NumT `NatT ->
      [
        NumV (`Nat (Bigint.of_int 1));
        NumV (`Nat (Bigint.of_int 4));
        NumV (`Nat (Bigint.of_int 6));
        NumV (`Nat (Bigint.of_int 8));
      ]
      |> Rand.random_select |> wrap_value typ.it
  | NumT `IntT ->
      [
        NumV (`Int (Bigint.of_int (-2)));
        NumV (`Int (Bigint.of_int 0));
        NumV (`Int (Bigint.of_int 2));
        NumV (`Int (Bigint.of_int 3));
      ]
      |> Rand.random_select |> wrap_value typ.it
  | TextT -> [ TextV "a"; TextV "b" ] |> Rand.random_select |> wrap_value typ.it
  | VarT (tid, targs) -> (
      let td = TDEnv.find_opt tid tdenv in
      match td with
      | Some (tparams, typdef) -> (
          let theta = List.combine tparams targs |> TDEnv.of_list in
          match typdef.it with
          | PlainT typ -> typ |> Typ.subst_typ theta |> gen_from_typ depth tdenv
          | StructT typfields ->
              let atoms, typs = List.split typfields in
              let* values =
                typs |> Typ.subst_typs theta |> gen_from_typs depth tdenv
              in
              let valuefields = List.combine atoms values in
              StructV valuefields |> Option.some |> wrap_value typ.it
          | VariantT typcases ->
              let choices =
                List.map
                  (fun typcase () ->
                    let mixop, typs = typcase.it in
                    let typs = Typ.subst_typs theta typs in
                    let* values = gen_from_typs depth tdenv typs in
                    CaseV (mixop, values) |> Option.some |> wrap_value typ.it)
                  typcases
              in
              let* choice = Rand.random_select choices in
              choice ())
      | None -> None)
  | TupleT typs_inner ->
      let* values_inner = gen_from_typs depth tdenv typs_inner in
      TupleV values_inner |> Option.some |> wrap_value typ.it
  | IterT (_, Opt) when depth = 0 ->
      OptV None |> Option.some |> wrap_value typ.it
  | IterT (typ_inner, Opt) ->
      let choices =
        [
          (fun () -> OptV None |> Option.some |> wrap_value typ.it);
          (fun () ->
            let* value_inner = gen_from_typ depth tdenv typ_inner in
            OptV (Some value_inner) |> Option.some |> wrap_value typ.it);
        ]
      in
      let* choice = Rand.random_select choices in
      choice ()
  | IterT (_, List) when depth = 0 ->
      ListV [] |> Option.some |> wrap_value typ.it
  | IterT (typ_inner, List) ->
      let len = Random.int 3 in
      let* values_inner =
        List.init len (fun _ -> typ_inner) |> gen_from_typs depth tdenv
      in
      ListV values_inner |> Option.some |> wrap_value typ.it
  | FuncT -> None

and gen_from_typs (depth : int) (tdenv : TDEnv.t) (typs : typ list) :
    value list option =
  if depth <= 0 then None
  else
    List.fold_left
      (fun values_opt typ ->
        let* values = values_opt in
        let* value = gen_from_typ depth tdenv typ in
        Some (values @ [ value ]))
      (Some []) typs

(* FBitT to FIntT conversion *)

let convert_fbit_to_fint (value : value) : value option =
  let typ = value.note.typ in
  match value.it with
  | CaseV ([ [ { it = Atom "FBitT"; _ } ]; [] ], [ value_expr ]) ->
      CaseV ([ [ Atom.Atom "FIntT" $ no_region ]; [] ], [ value_expr ])
      |> Option.some |> wrap_value typ
  | _ -> None

(* Entry point for mutation *)

let mutate (tdenv : TDEnv.t) (value : value) : value option =
  let mutations =
    [
      (fun () -> gen_from_typ (Random.int 5) tdenv (value.note.typ $ no_region));
      (fun () -> convert_fbit_to_fint value);
    ]
  in
  let* mutation = Rand.random_select mutations in
  mutation ()
