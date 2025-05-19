open Xl
open Sl.Ast
module Value = Runtime_dynamic_sl.Value
module Typ = Runtime_dynamic_sl.Typ
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
module Dep = Runtime_testgen.Dep
open Config
open Domain.Lib
open Util.Source

(* Kinds of mutations *)

type kind = GenFromTyp | MutateList | MixopGroup

let string_of_kind = function
  | GenFromTyp -> "GenFromTyp"
  | MutateList -> "MutateList"
  | MixopGroup -> "MixopGroup"

(* Option monad *)

let ( let* ) = Option.bind

(* Helpers for wrapping values *)

let wrap_value (typ : typ') (value : value') : value =
  value $$$ { vid = -1; typ }

let wrap_value_opt (typ : typ') (value_opt : value' option) : value option =
  Option.map (wrap_value typ) value_opt

(* Type-driven mutation *)

let rec gen_from_typ (depth : int) (tdenv : TDEnv.t) (texts : value' list)
    (typ : typ) : value option =
  if depth <= 0 then None else gen_from_typ' depth tdenv texts typ

and gen_from_typ' (depth : int) (tdenv : TDEnv.t) (texts : value' list)
    (typ : typ) : value option =
  let depth = depth - 1 in
  match typ.it with
  | BoolT ->
      [ BoolV true; BoolV false ] |> Rand.random_select |> wrap_value_opt typ.it
  | NumT `NatT ->
      [
        NumV (`Nat (Bigint.of_int 1));
        NumV (`Nat (Bigint.of_int 4));
        NumV (`Nat (Bigint.of_int 6));
        NumV (`Nat (Bigint.of_int 8));
      ]
      |> Rand.random_select |> wrap_value_opt typ.it
  | NumT `IntT ->
      [
        NumV (`Int (Bigint.of_int (-2)));
        NumV (`Int (Bigint.of_int 0));
        NumV (`Int (Bigint.of_int 2));
        NumV (`Int (Bigint.of_int 3));
      ]
      |> Rand.random_select |> wrap_value_opt typ.it
  | TextT -> texts |> Rand.random_select |> wrap_value_opt typ.it
  | VarT (tid, targs) -> (
      let td = TDEnv.find_opt tid tdenv in
      match td with
      | Some (tparams, typdef) -> (
          let theta = List.combine tparams targs |> TDEnv.of_list in
          match typdef.it with
          | PlainT typ ->
              typ |> Typ.subst_typ theta |> gen_from_typ depth tdenv texts
          | StructT typfields ->
              let atoms, typs = List.split typfields in
              let* values =
                typs |> Typ.subst_typs theta |> gen_from_typs depth tdenv texts
              in
              let valuefields = List.combine atoms values in
              StructV valuefields |> Option.some |> wrap_value_opt typ.it
          | VariantT typcases ->
              let nottyps' = List.map it typcases in
              let nottyps' =
                List.map
                  (fun (mixop, typs) ->
                    let typs = Typ.subst_typs theta typs in
                    (mixop, typs))
                  nottyps'
              in
              let expand_nottyp' nottyp' =
                let mixop, typs = nottyp' in
                let* values = gen_from_typs depth tdenv texts typs in
                CaseV (mixop, values) |> Option.some
              in
              (* filters out failures *)
              List.map expand_nottyp' nottyps'
              |> List.filter Option.is_some |> List.map Option.get
              |> Rand.random_select |> wrap_value_opt typ.it)
      | None -> None)
  | TupleT typs_inner ->
      let* values_inner = gen_from_typs depth tdenv texts typs_inner in
      TupleV values_inner |> Option.some |> wrap_value_opt typ.it
  | IterT (_, Opt) when depth = 0 ->
      OptV None |> Option.some |> wrap_value_opt typ.it
  | IterT (typ_inner, Opt) ->
      let choices : value' option list =
        [
          OptV None |> Option.some;
          (let* value_inner = gen_from_typ depth tdenv texts typ_inner in
           OptV (Some value_inner) |> Option.some);
        ]
      in
      let* choice =
        choices |> List.filter Option.is_some |> Rand.random_select
      in
      choice |> wrap_value_opt typ.it
  | IterT (_, List) when depth = 0 ->
      ListV [] |> Option.some |> wrap_value_opt typ.it
  | IterT (typ_inner, List) ->
      let len = Random.int 3 in
      let* values_inner =
        List.init len (fun _ -> typ_inner) |> gen_from_typs depth tdenv texts
      in
      ListV values_inner |> Option.some |> wrap_value_opt typ.it
  | FuncT -> None

and gen_from_typs (depth : int) (tdenv : TDEnv.t) (texts : value' list)
    (typs : typ list) : value list option =
  if depth <= 0 then None
  else
    List.fold_left
      (fun values_opt typ ->
        let* values = values_opt in
        let* value = gen_from_typ depth tdenv texts typ in
        Some (values @ [ value ]))
      (Some []) typs

let mutate_type_driven (tdenv : TDEnv.t) (texts : value' list) (value : value) :
    (kind * value) option =
  let typ = value.note.typ $ no_region in
  let depth = Random.int 4 + 1 in
  let value_opt = gen_from_typ depth tdenv texts typ in
  Option.map (fun value -> (GenFromTyp, value)) value_opt

(* Constructor mutation *)

let mutate_mixop (mixopenv : mixopenv) (value : value) : (kind * value) option =
  let typ = value.note.typ in
  match typ with
  | VarT (id, _) -> (
      match value.it with
      | CaseV (mixop, values) ->
          let* groups = MixopEnv.find_opt id.it mixopenv in
          let mixop_set_set =
            MixopSetSet.filter
              (fun mixop_set ->
                MixopSet.exists
                  (fun mixop' -> Mixop.compare mixop' mixop = 0)
                  mixop_set)
              groups
          in
          let* mixop_set =
            if MixopSetSet.cardinal mixop_set_set = 0 then None
            else mixop_set_set |> MixopSetSet.choose |> Option.some
          in
          let* mixop =
            mixop_set
            |> MixopSet.filter (fun mixop' -> Mixop.compare mixop' mixop <> 0)
            |> MixopSet.elements |> Rand.random_select
          in
          let value = CaseV (mixop, values) |> wrap_value typ in
          (MixopGroup, value) |> Option.some
      | _ -> assert false)
  | _ -> assert false

(* List mutations *)

let rec shuffle_list' (value : value) : value =
  let typ = value.note.typ in
  match value.it with
  | BoolV _ | NumV _ | TextV _ -> value.it |> wrap_value typ
  | StructV valuefields ->
      let atoms, values = List.split valuefields in
      let values_shuffled = List.map shuffle_list' values in
      let valuefields_shuffled = List.combine atoms values_shuffled in
      StructV valuefields_shuffled |> wrap_value typ
  | CaseV (mixop, values) ->
      let values_shuffled = List.map shuffle_list' values in
      CaseV (mixop, values_shuffled) |> wrap_value typ
  | TupleV values ->
      let values_shuffled = List.map shuffle_list' values in
      TupleV values_shuffled |> wrap_value typ
  | OptV None -> value.it |> wrap_value typ
  | OptV (Some value) ->
      let value_shuffled = shuffle_list' value in
      OptV (Some value_shuffled) |> wrap_value typ
  | ListV values ->
      let values_shuffled = Rand.shuffle values in
      ListV values_shuffled |> wrap_value typ
  | FuncV _ -> value.it |> wrap_value typ

let shuffle_list (value : value) : value option =
  let value_shuffled = shuffle_list' value in
  if Value.eq value value_shuffled then None else Some value_shuffled

let rec duplicate_list' (value : value) : value =
  let typ = value.note.typ in
  match value.it with
  | BoolV _ | NumV _ | TextV _ -> value.it |> wrap_value typ
  | StructV valuefields ->
      let atoms, values = List.split valuefields in
      let values_duplicated = List.map duplicate_list' values in
      let valuefields_duplicated = List.combine atoms values_duplicated in
      StructV valuefields_duplicated |> wrap_value typ
  | CaseV (mixop, values) ->
      let values_duplicated = List.map duplicate_list' values in
      CaseV (mixop, values_duplicated) |> wrap_value typ
  | TupleV values ->
      let values_duplicated = List.map duplicate_list' values in
      TupleV values_duplicated |> wrap_value typ
  | OptV None -> value.it |> wrap_value typ
  | OptV (Some value) ->
      let value_duplicated = duplicate_list' value in
      OptV (Some value_duplicated) |> wrap_value typ
  | ListV values -> (
      match Rand.random_select values with
      | Some value ->
          let values = value :: values in
          ListV values |> wrap_value typ
      | None -> value.it |> wrap_value typ)
  | FuncV _ -> value.it |> wrap_value typ

let duplicate_list (value : value) : value option =
  let value_duplicated = duplicate_list' value in
  if Value.eq value value_duplicated then None else Some value_duplicated

let rec shrink_list' (value : value) : value =
  let typ = value.note.typ in
  match value.it with
  | BoolV _ | NumV _ | TextV _ -> value.it |> wrap_value typ
  | StructV valuefields ->
      let atoms, values = List.split valuefields in
      let values_shrinked = List.map shrink_list' values in
      let valuefields_shrinked = List.combine atoms values_shrinked in
      StructV valuefields_shrinked |> wrap_value typ
  | CaseV (mixop, values) ->
      let values_shrinked = List.map shrink_list' values in
      CaseV (mixop, values_shrinked) |> wrap_value typ
  | TupleV values ->
      let values_shrinked = List.map shrink_list' values in
      TupleV values_shrinked |> wrap_value typ
  | OptV None -> value.it |> wrap_value typ
  | OptV (Some value) ->
      let value_shrinked = shrink_list' value in
      OptV (Some value_shrinked) |> wrap_value typ
  | ListV [] -> value.it |> wrap_value typ
  | ListV values ->
      let size = Random.int (List.length values) in
      let values = Rand.random_sample size values in
      ListV values |> wrap_value typ
  | FuncV _ -> value.it |> wrap_value typ

let shrink_list (value : value) : value option =
  let value_shrinked = shrink_list' value in
  if Value.eq value value_shrinked then None else Some value_shrinked

let mutate_list (value : value) : (kind * value) option =
  let wrap_kind (value_opt : value option) : (kind * value) option =
    Option.map (fun value -> (MutateList, value)) value_opt
  in
  let mutations_list =
    [
      (fun () -> shuffle_list value |> wrap_kind);
      (fun () -> duplicate_list value |> wrap_kind);
      (fun () -> shrink_list value |> wrap_kind);
    ]
  in
  let* mutation = Rand.random_select mutations_list in
  mutation ()

let mutate_node (tdenv : TDEnv.t) (groups : mixopenv) (texts : value' list)
    (value : value) : (kind * value) option =
  match value.it with
  | ListV _ ->
      let* mutation =
        [
          (fun () -> mutate_list value);
          (fun () -> mutate_type_driven tdenv texts value);
        ]
        |> Rand.random_select
      in
      mutation ()
  | CaseV (_, _) ->
      let* mutation =
        [
          (fun () -> mutate_mixop groups value);
          (fun () -> mutate_type_driven tdenv texts value);
        ]
        |> Rand.random_select
      in
      mutation ()
  | _ -> mutate_type_driven tdenv texts value

let mutate_walk (tdenv : TDEnv.t) (groups : mixopenv) (texts : value' list)
    (value : value) : (kind * value) option =
  (* Compute the best path to a leaf node in the value subtree *)
  let key_max = ref min_float in
  let path_best = ref [] in
  let rec traverse (path : int list) (value : value) (depth : int) : unit =
    let weight = 1.0 /. (float_of_int (depth + 1) ** 3.0) in
    let u = Random.float 1.0 in
    let key = u ** (1.0 /. weight) in
    if key > !key_max then (
      key_max := key;
      path_best := List.rev path);
    match value.it with
    | BoolV _ | NumV _ | TextV _ | OptV _ | FuncV _ -> ()
    | StructV valuefields ->
        List.iteri
          (fun idx (_, value) -> traverse (idx :: path) value (depth + 1))
          valuefields
    | CaseV (_, values) | TupleV values | ListV values ->
        List.iteri
          (fun idx value -> traverse (idx :: path) value (depth + 1))
          values
  in
  traverse [] value 0;
  let kind_found = ref None in

  (* Rebuild the value tree with a new value at the best path *)
  let rec rebuild (path : int list) (value : value) : value option =
    let typ = value.note.typ in
    match (path, value) with
    | [], value ->
        let* kind, value = mutate_node tdenv groups texts value in
        kind_found := kind |> Option.some;
        value |> Option.some
    | idx :: path, value -> (
        match value.it with
        | BoolV _ | NumV _ | TextV _ | OptV _ | FuncV _ ->
            value.it |> wrap_value typ |> Option.some
        | StructV valuefields ->
            let atoms, values = List.split valuefields in
            let* values = rebuilds path idx values in
            let valuefields = List.combine atoms values in
            StructV valuefields |> wrap_value typ |> Option.some
        | CaseV (mixop, values) ->
            let* values = rebuilds path idx values in
            CaseV (mixop, values) |> wrap_value typ |> Option.some
        | TupleV values ->
            let* values = rebuilds path idx values in
            TupleV values |> wrap_value typ |> Option.some
        | ListV values ->
            let* values = rebuilds path idx values in
            ListV values |> wrap_value typ |> Option.some)
  and rebuilds rest i (values_inner : value list) : value list option =
    values_inner
    |> List.mapi (fun j value ->
           if j = i then rebuild rest value else Some value)
    |> List.fold_left
         (fun values_opt value ->
           let* values = values_opt in
           let* value = value in
           Some (values @ [ value ]))
         (Some [])
  in
  let* value = rebuild !path_best value in
  let* kind = !kind_found in
  Some (kind, value)

(* Find parent node, if any, in the dependency graph *)

let find_parent (graph : Dep.Graph.t) (vid_source : vid) : vid option =
  let find_expand (graph : Dep.Graph.t) (v : vid) : vid list =
    match Dep.Graph.G.find_opt graph.edges v with
    | None -> []
    | Some edges ->
        Dep.Edges.E.fold
          (fun (label, target_vid) () acc ->
            match label with Dep.Edges.Expand -> target_vid :: acc | _ -> acc)
          edges []
  in
  let parents = find_expand graph vid_source in
  assert (List.length parents <= 1);
  parents |> Rand.random_select

(* Entry point for mutation *)

let mutate (tdenv : TDEnv.t) (groups : mixopenv) (texts : value' list)
    (graph : Dep.Graph.t) (vid_source : vid) : (kind * value * value) option =
  (* Expand the node randomly *)
  let expansions =
    [
      (fun () -> find_parent graph vid_source);
      (fun () -> vid_source |> Option.some);
    ]
  in
  let expansion = Rand.random_select expansions |> Option.get in
  let vid_source =
    match expansion () with Some vid -> vid | None -> vid_source
  in
  (* Reassemble the node *)
  let value_source = Dep.Graph.reassemble_node graph VIdMap.empty vid_source in
  (* Mutate the node *)
  let* kind, value_mutated = mutate_walk tdenv groups texts value_source in
  (kind, value_source, value_mutated) |> Option.some

let mutates (fuel_mutate : int) (tdenv : TDEnv.t) (groups : mixopenv)
    (graph : Dep.Graph.t) (vid_program : vid) (vid_source : vid) :
    (kind * value * value) list =
  (* Collect the text pool *)
  let texts =
    List.init (vid_program + 1) Fun.id
    |> List.filter_map (fun vid ->
           let* mirror, _ = Dep.Graph.find_node graph vid in
           match mirror.it with TextN text -> Some (TextV text) | _ -> None)
  in
  let texts = texts @ [ TextV "lazy"; TextV "fox" ] in
  (* Do mutations *)
  List.init fuel_mutate (fun _ -> mutate tdenv groups texts graph vid_source)
  |> List.filter_map Fun.id
