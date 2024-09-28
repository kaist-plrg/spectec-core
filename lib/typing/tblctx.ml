module Types = Runtime.Types
module Type = Types.Type
open Util.Source

type t = {
  keys : (Type.t * Il.Ast.match_kind') list;
  actions : (Il.Ast.var' * Types.param list * Il.Ast.arg list) list;
}

let empty = { keys = []; actions = [] }

let add_key table_key table_ctx =
  { table_ctx with keys = table_ctx.keys @ [ table_key ] }

let add_action table_action table_ctx =
  { table_ctx with actions = table_ctx.actions @ [ table_action ] }

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
