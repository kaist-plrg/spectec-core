open Il.Ast
open Error
open Util.Source

(* Initializer *)

let init () : unit = Fresh.ctr := 0

(* Builtin calls *)

module Funcs = Map.Make (String)

let funcs =
  Funcs.empty
  (* Nats *)
  |> Funcs.add "sum" (fun at targs values_input ->
         Nats.sum at targs values_input)
  |> Funcs.add "max" (fun at targs values_input ->
         Nats.max at targs values_input)
  |> Funcs.add "min" (fun at targs values_input ->
         Nats.min at targs values_input)
  (* Texts *)
  |> Funcs.add "int_to_text" (fun at targs values_input ->
         Texts.int_to_text at targs values_input)
  |> Funcs.add "strip_prefix" (fun at targs values_input ->
         Texts.strip_prefix at targs values_input)
  |> Funcs.add "strip_suffix" (fun at targs values_input ->
         Texts.strip_suffix at targs values_input)
  (* Lists *)
  |> Funcs.add "rev_" (fun at targs values_input ->
         Lists.rev_ at targs values_input)
  |> Funcs.add "concat_" (fun at targs values_input ->
         Lists.concat_ at targs values_input)
  |> Funcs.add "distinct_" (fun at targs values_input ->
         Lists.distinct_ at targs values_input)
  (* Sets *)
  |> Funcs.add "intersect_set" (fun at targs values_input ->
         Sets.intersect_set at targs values_input)
  |> Funcs.add "union_set" (fun at targs values_input ->
         Sets.union_set at targs values_input)
  |> Funcs.add "unions_set" (fun at targs values_input ->
         Sets.unions_set at targs values_input)
  |> Funcs.add "diff_set" (fun at targs values_input ->
         Sets.diff_set at targs values_input)
  |> Funcs.add "sub_set" (fun at targs values_input ->
         Sets.is_subset at targs values_input)
  |> Funcs.add "eq_set" (fun at targs values_input ->
         Sets.eq_set at targs values_input)
  (* Maps *)
  |> Funcs.add "find_map_opt" (fun at targs values_input ->
         Maps.find_map_opt at targs values_input)
  |> Funcs.add "find_maps_opt" (fun at targs values_input ->
         Maps.find_maps_opt at targs values_input)
  |> Funcs.add "add_map" (fun at targs values_input ->
         Maps.add_map at targs values_input)
  |> Funcs.add "adds_map" (fun at targs values_input ->
         Maps.adds_map at targs values_input)
  |> Funcs.add "update_map" (fun at targs values_input ->
         Maps.update_map at targs values_input)
  (* Fresh type id *)
  |> Funcs.add "fresh_tid" (fun at targs values_input ->
         Fresh.fresh_tid at targs values_input)
  (* Numerics *)
  |> Funcs.add "shl" (fun at targs values_input ->
         Numerics.shl at targs values_input)
  |> Funcs.add "shr" (fun at targs values_input ->
         Numerics.shr at targs values_input)
  |> Funcs.add "shr_arith" (fun at targs values_input ->
         Numerics.shr_arith at targs values_input)
  |> Funcs.add "pow2" (fun at targs values_input ->
         Numerics.pow2 at targs values_input)
  |> Funcs.add "to_int" (fun at targs values_input ->
         Numerics.to_int at targs values_input)
  |> Funcs.add "to_bitstr" (fun at targs values_input ->
         Numerics.to_bitstr at targs values_input)
  |> Funcs.add "bneg" (fun at targs values_input ->
         Numerics.bneg at targs values_input)
  |> Funcs.add "band" (fun at targs values_input ->
         Numerics.band at targs values_input)
  |> Funcs.add "bxor" (fun at targs values_input ->
         Numerics.bxor at targs values_input)
  |> Funcs.add "bor" (fun at targs values_input ->
         Numerics.bor at targs values_input)
  |> Funcs.add "bitacc" (fun at targs values_input ->
         Numerics.bitacc at targs values_input)

let is_builtin (id : id) : bool = Funcs.mem id.it funcs

let invoke (id : id) (targs : targ list) (args : value list) : value =
  let func = Funcs.find_opt id.it funcs in
  check (Option.is_some func) id.at
    (Format.asprintf "implementation for builtin %s is missing" id.it);
  let func = Option.get func in
  func id.at targs args
