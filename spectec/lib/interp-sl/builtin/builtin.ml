open Sl.Ast
open Error
open Util.Source

(* Initializer *)

let init () : unit = Fresh.ctr := 0

(* Builtin calls *)

module Funcs = Map.Make (String)

let funcs =
  Funcs.empty
  (* Nats *)
  |> Funcs.add "sum" Nats.sum
  |> Funcs.add "max" Nats.max |> Funcs.add "min" Nats.min
  (* Texts *)
  |> Funcs.add "int_to_text" Texts.int_to_text
  |> Funcs.add "strip_prefix" Texts.strip_prefix
  |> Funcs.add "strip_suffix" Texts.strip_suffix
  (* Lists *)
  |> Funcs.add "rev_" Lists.rev_
  |> Funcs.add "concat_" Lists.concat_
  |> Funcs.add "distinct_" Lists.distinct_
  |> Funcs.add "partition_" Lists.partition_
  |> Funcs.add "assoc_" Lists.assoc_
  (* Sets *)
  |> Funcs.add "intersect_set" Sets.intersect_set
  |> Funcs.add "union_set" Sets.union_set
  |> Funcs.add "unions_set" Sets.unions_set
  |> Funcs.add "diff_set" Sets.diff_set
  |> Funcs.add "sub_set" Sets.sub_set
  |> Funcs.add "eq_set" Sets.eq_set
  (* Maps *)
  |> Funcs.add "find_map" Maps.find_map
  |> Funcs.add "find_maps" Maps.find_maps
  |> Funcs.add "add_map" Maps.add_map
  |> Funcs.add "adds_map" Maps.adds_map
  |> Funcs.add "update_map" Maps.update_map
  (* Fresh type id *)
  |> Funcs.add "fresh_tid" Fresh.fresh_tid
  (* Numerics *)
  |> Funcs.add "shl" Numerics.shl
  |> Funcs.add "shr" Numerics.shr
  |> Funcs.add "shr_arith" Numerics.shr_arith
  |> Funcs.add "pow2" Numerics.pow2
  |> Funcs.add "to_int" Numerics.to_int
  |> Funcs.add "to_bitstr" Numerics.to_bitstr
  |> Funcs.add "bneg" Numerics.bneg
  |> Funcs.add "band" Numerics.band
  |> Funcs.add "bxor" Numerics.bxor
  |> Funcs.add "bor" Numerics.bor
  |> Funcs.add "bitacc" Numerics.bitacc

let is_builtin (id : id) : bool = Funcs.mem id.it funcs

let invoke (ctx : Ctx.t) (id : id) (targs : targ list) (args : value list) :
    value =
  let func = Funcs.find_opt id.it funcs in
  check (Option.is_some func) id.at
    (Format.asprintf "implementation for builtin %s is missing" id.it);
  let func = Option.get func in
  func ctx id.at targs args
