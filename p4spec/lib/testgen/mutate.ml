open Sl.Ast
module Typ = Runtime_dynamic_sl.Typ
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
open Util.Source

(* Option monad *)

let ( let* ) x f = Option.bind x f

let bind_list (opt_list : value option list) : value list option =
  if opt_list |> List.exists Option.is_none then None
  else opt_list |> List.map Option.get |> Option.some

(* Generate random values from a type *)

let rec gen_from_typ (depth : int) (tdenv : TDEnv.t) (typ : typ) : value option
    =
  if depth <= 0 then None else gen_from_typ' depth tdenv typ

and gen_from_typ' (depth : int) (tdenv : TDEnv.t) (typ : typ) : value option =
  let depth = depth - 1 in
  let value_opt =
    match typ.it with
    | BoolT -> [ BoolV true; BoolV false ] |> Rand.random_select
    | NumT `NatT ->
        [
          NumV (`Nat (Bigint.of_int 1));
          NumV (`Nat (Bigint.of_int 4));
          NumV (`Nat (Bigint.of_int 6));
          NumV (`Nat (Bigint.of_int 8));
        ]
        |> Rand.random_select
    | NumT `IntT ->
        [
          NumV (`Int (Bigint.of_int (-2)));
          NumV (`Int (Bigint.of_int 0));
          NumV (`Int (Bigint.of_int 2));
          NumV (`Int (Bigint.of_int 3));
        ]
        |> Rand.random_select
    | TextT -> [ TextV "a"; TextV "b" ] |> Rand.random_select
    | VarT (tid, targs) -> (
        let td = TDEnv.find_opt tid tdenv in
        match td with
        | Some (tparams, typdef) -> (
            let theta = List.combine tparams targs |> TDEnv.of_list in
            match typdef.it with
            | PlainT typ ->
                let typ = Typ.subst_typ theta typ in
                gen_from_typ depth tdenv typ |> Option.map it
            | StructT typfields ->
                let atoms, typs = List.split typfields in
                let* values =
                  typs |> Typ.subst_typs theta |> gen_from_typs depth tdenv
                in
                StructV (List.combine atoms values) |> Option.some
            | VariantT typcases ->
                let nottyps =
                  List.map
                    (fun typcase ->
                      let mixop, typs = typcase.it in
                      let typs = Typ.subst_typs theta typs in
                      (mixop, typs) $ typcase.at)
                    typcases
                in
                nottyps
                |> List.map (gen_from_nottyp depth tdenv)
                |> List.filter Option.is_some |> List.map Option.get
                |> Rand.random_select)
        | None -> None)
    | TupleT typs_inner ->
        let* values_inner = gen_from_typs depth tdenv typs_inner in
        TupleV values_inner |> Option.some
    | IterT (_, Opt) when depth = 0 -> OptV None |> Option.some
    | IterT (typ_inner, Opt) ->
        let i = Random.int 2 in
        if i = 0 then OptV None |> Option.some
        else
          let* value_inner = gen_from_typ depth tdenv typ_inner in
          OptV (Some value_inner) |> Option.some
    | IterT (_, List) when depth = 0 -> ListV [] |> Option.some
    | IterT (typ_inner, List) ->
        let len_max = 3 in
        let len = Random.int len_max in
        let* values_inner =
          List.init len (fun _ -> gen_from_typ depth tdenv typ_inner)
          |> bind_list
        in
        ListV values_inner |> Option.some
    | FuncT -> None
  in
  Option.map (fun value -> value $$$ { vid = -1; typ = typ.it }) value_opt

and gen_from_typs (depth : int) (tdenv : TDEnv.t) (typs : typ list) :
    value list option =
  if depth <= 0 then None
  else List.map (gen_from_typ depth tdenv) typs |> bind_list

and gen_from_nottyp (depth : int) (tdenv : TDEnv.t) (nottyp : nottyp) :
    value' option =
  let mixop, typs = nottyp.it in
  let* values = gen_from_typs depth tdenv typs in
  CaseV (mixop, values) |> Option.some

(* Entry point for mutation *)

let mutate (tdenv : TDEnv.t) (value : value) : value option =
  let depth = Random.int 5 in
  let typ = value.note.typ $ no_region in
  gen_from_typ depth tdenv typ
