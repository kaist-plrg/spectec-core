open Syntax.Ast
open Runtime.Context
open Util.Source
module R = Runtime

(* Logic for match-action table *)

let match_action (ctx : Ctx.t) (_keys : (R.Value.t * match_kind) list)
    (actions : table_action list) (_entries : table_entry list)
    (default : table_default option) (_custom : table_custom list) =
  let path, _ = ctx.id in
  let id = List.rev path |> List.hd in
  (* Determine the action to be run *)
  (* Always give the default action for now *)
  let action =
    match default with
    | Some { it = action, _; _ } -> Some action
    | None -> None
  in
  (* Calling an apply method on a table instance returns a value with
     a struct type with three fields. This structure is synthesized
     by the compiler automatically. (14.2.2) *)
  let value =
    let hit = R.Value.BoolV (Option.is_some action) in
    let miss = R.Value.BoolV (Option.is_none action) in
    (* For enum values without an underlying type the default value is
       the first value that appears in the enum type declaration. (7.3) *)
    let action_run =
      let action_run =
        Option.value ~default:(List.hd actions) action
        |> it
        |> (fun (id, _, _) -> id)
        |> Format.asprintf "%a" Syntax.Pp.pp_var
      in
      R.Value.EnumFieldV ("action_list(" ^ id ^ ")", action_run)
    in
    R.Value.StructV [ ("hit", hit); ("miss", miss); ("action_run", action_run) ]
  in
  (action, value)
