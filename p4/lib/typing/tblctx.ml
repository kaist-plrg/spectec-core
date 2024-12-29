module Value = Runtime_static.Value
module Numerics = Runtime_static.Numerics
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
open Util.Source
open Util.Error

let error_no_info = error_checker_no_info

type pt = { values : int list; init : bool; delta : int; largest_wins : bool }
type et = { size : int; const : bool }

(* Mode for matching table entry

   - NoPri: does not require priority and is not lpm
   - NoPriLpm: does not require priority and is lpm (holds the max prefix)
   - Pri: requires priority and is not lpm
   - PriLpm: requires priority and is lpm *)

type mode = NoPri | NoPriLpm of int | Pri | PriLpm

type t = {
  keys : (Type.t * Il.Ast.match_kind') list;
  actions : (Il.Ast.var' * Types.param list * Il.Ast.arg list) list;
  priorities : pt;
  entries : et;
  mode : mode;
}

(* State per table entry

   - Lpm: the entry is lpm and holds the prefix length
   - NoLpm: the entry is not lpm *)

type state = Lpm of int | NoLpm

(* Constructors *)

let empty_pt = { values = []; init = false; delta = 1; largest_wins = true }
let empty_et = { size = 0; const = true }

let empty =
  {
    keys = [];
    actions = [];
    priorities = empty_pt;
    entries = empty_et;
    mode = NoPri;
  }

let get_lpm_prefix value_mask =
  let rec get_lpm_prefix' value_mask prefix =
    match (value_mask : Value.t) with
    | FBitV (width, _) when Bigint.(width = zero) -> prefix
    | FBitV (width, value) ->
        let two = Bigint.(one + one) in
        let width_next = Bigint.(width - one) in
        let value_next = Bigint.(value / two) in
        let value_mask_next = Numerics.bit_of_raw_int value_next width_next in
        if Bigint.(value % two <> zero) then
          get_lpm_prefix' value_mask_next (prefix + 1)
        else if prefix = 0 then get_lpm_prefix' value_mask_next 0
        else
          Format.asprintf "(get_lpm_prefix) %a is an invalid lpm mask\n"
            Value.pp value_mask
          |> error_no_info
    | _ ->
        Format.asprintf "(get_lpm_prefix) %a is an invalid lpm mask\n" Value.pp
          value_mask
        |> error_no_info
  in
  let prefix = get_lpm_prefix' value_mask 0 in
  Lpm prefix

(* Setters and adders *)

let add_key table_key table_ctx =
  { table_ctx with keys = table_ctx.keys @ [ table_key ] }

let add_action table_action table_ctx =
  { table_ctx with actions = table_ctx.actions @ [ table_action ] }

let add_priority priority table_ctx =
  {
    table_ctx with
    priorities =
      {
        table_ctx.priorities with
        values = table_ctx.priorities.values @ [ priority ];
      };
  }

let set_priority_init init table_ctx =
  { table_ctx with priorities = { table_ctx.priorities with init } }

let set_priority_delta delta table_ctx =
  { table_ctx with priorities = { table_ctx.priorities with delta } }

let set_largest_priority_wins largest_wins table_ctx =
  { table_ctx with priorities = { table_ctx.priorities with largest_wins } }

let set_entries_const const table_ctx =
  { table_ctx with entries = { table_ctx.entries with const } }

let set_entries_size size table_ctx =
  { table_ctx with entries = { table_ctx.entries with size } }

let set_mode mode table_ctx = { table_ctx with mode }

(* Updaters *)

let update_mode (match_kind : string) (typ_key : Type.t) table_ctx =
  match (match_kind, table_ctx.mode) with
  | "lpm", NoPri ->
      let prefix_max = Type.get_width typ_key in
      set_mode (NoPriLpm prefix_max) table_ctx
  | "lpm", Pri -> set_mode PriLpm table_ctx
  | "lpm", _ -> "(update_mode) too many lpms" |> error_no_info
  | ("range" | "ternary" | "optional"), NoPri -> set_mode Pri table_ctx
  | ("range" | "ternary" | "optional"), NoPriLpm _ -> set_mode PriLpm table_ctx
  | _ -> table_ctx

let update_state (state_prev : state) (state_curr : state) =
  match (state_prev, state_curr) with
  | NoLpm, Lpm _ -> state_curr
  | Lpm _, NoLpm -> state_prev
  | NoLpm, NoLpm -> NoLpm
  | Lpm _, Lpm _ -> "(update_state) too many lpms" |> error_no_info

(* Finders *)

let find_action table_ctx var =
  List.fold_left
    (fun table_action (var_action, params_action, args_action) ->
      match table_action with
      | Some _ -> table_action
      | None ->
          if Il.Eq.eq_var' var.it var_action then
            Some (params_action, args_action)
          else table_action)
    None table_ctx.actions

let find_last_priority table_ctx =
  let len = List.length table_ctx.priorities.values - 1 in
  List.nth table_ctx.priorities.values len
