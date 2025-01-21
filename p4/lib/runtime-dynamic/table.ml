module L = Lang.Ast
open Il.Ast
module P = Il.Pp
open Util.Pp
open Util.Source
open Util.Error

let error_no_info = error_inst_no_info

type t = {
  keys : table_key list;
  actions : table_action list;
  action_default : bool * table_action;
  entries : bool * table_entry list;
  customs : table_custom list;
}

(* Pretty-printing *)

let pp ?(level = 0) fmt table =
  let table_action_default_const, table_action_default = table.action_default in
  let table_entries_const, table_entries = table.entries in
  F.fprintf fmt
    "{\n\
     %skeys :\n\
     %a\n\
     %sactions :\n\
     %a\n\
     %saction_default : %s%a\n\
     %sentries : %s\n\
     %a\n\
     %scustoms : %a\n\
     %s}"
    (indent (level + 1))
    (pp_list ~level:(level + 2) P.pp_table_key ~sep:Nl)
    table.keys
    (indent (level + 1))
    (pp_list ~level:(level + 2) P.pp_table_action ~sep:Nl)
    table.actions
    (indent (level + 1))
    (if table_action_default_const then "const " else "")
    (P.pp_table_action ~level:(level + 2))
    table_action_default
    (indent (level + 1))
    (if table_entries_const then "const" else "")
    (pp_list ~level:(level + 2) P.pp_table_entry ~sep:Nl)
    table_entries
    (indent (level + 1))
    (pp_list ~level:(level + 2) P.pp_table_custom ~sep:Nl)
    table.customs (indent level)

(* Initialization *)

let init_table_keys (table : table) : table_key list =
  List.filter_map
    (function L.KeyP table_keys -> Some table_keys.it | _ -> None)
    table
  |> function
  | [] -> []
  | [ table_keys ] -> table_keys
  | _ ->
      "(init_table_keys) a table should have at most one key property"
      |> error_no_info

let init_table_actions (table : table) : table_action list =
  List.filter_map
    (function L.ActionP table_actions -> Some table_actions.it | _ -> None)
    table
  |> function
  | [ table_actions ] -> table_actions
  | _ ->
      "(init_table_actions) a table should have exactly one action property"
      |> error_no_info

let init_table_default (table : table) : bool * table_action =
  List.filter_map
    (function L.DefaultP table_default -> Some table_default.it | _ -> None)
    table
  |> function
  | [] ->
      let var_action = L.Top ("NoAction" $ no_info) $ no_info in
      let table_action_default = (var_action, [], [], [], []) $ no_info in
      (false, table_action_default)
  | [ (table_action, table_action_const) ] -> (table_action_const, table_action)
  | _ ->
      "(init_table_default) a table should have at most one default property"
      |> error_no_info

let init_table_entries (table : table) : bool * table_entry list =
  List.filter_map
    (function L.EntryP table_entries -> Some table_entries.it | _ -> None)
    table
  |> function
  | [] -> (false, [])
  | [ (table_entries, table_entries_const) ] ->
      (table_entries_const, table_entries)
  | _ ->
      "(init_table_entries) a table should have at most one entries property"
      |> error_no_info

let init_table_customs (table : table) : table_custom list =
  List.filter_map
    (function L.CustomP table_custom -> Some table_custom | _ -> None)
    table

let init (table : table) : t =
  let keys = init_table_keys table in
  let actions = init_table_actions table in
  let action_default = init_table_default table in
  let entries = init_table_entries table in
  let customs = init_table_customs table in
  { keys; actions; action_default; entries; customs }

(* Adders *)

let add_entry (keysets : keyset list) (table_action : table_action)
    (priority : value option) (table : t) : t =
  let entry = (keysets, table_action, priority, false, []) $ no_info in
  let table_entries_const, table_entries = table.entries in
  let table_entries = table_entries @ [ entry ] in
  { table with entries = (table_entries_const, table_entries) }

let add_default (table_action : table_action) (table : t) : t =
  { table with action_default = (false, table_action) }
