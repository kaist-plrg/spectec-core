module Types = Runtime.Types
module Type = Types.Type
open Util.Source

type t = {
  keys : (Type.t * Il.Ast.match_kind') list;
  actions : (Il.Ast.var' * Types.param list * Il.Ast.arg list) list;
}

let empty = { keys = []; actions = [] }

let add_key table_key table_ctx =
  { table_ctx with keys = table_key :: table_ctx.keys }

let add_action table_action table_ctx =
  { table_ctx with actions = table_action :: table_ctx.actions }

let find_action table_ctx var =
  let actions =
    List.map
      (fun (var, params, args) -> (var, (params, args)))
      table_ctx.actions
  in
  List.assoc_opt var.it actions
