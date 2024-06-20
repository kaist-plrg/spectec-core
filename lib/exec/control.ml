open Syntax.Ast
open Runtime.Base
open Runtime.Context

(* Logic for match-action table *)

let match_action (_ctx : Ctx.t) (_keys : (Value.t * mtchkind) list)
    (actions : table_action list) (_entries : table_entry list)
    (default : table_default option) (_custom : table_custom list) =
  (* Determine the action to be run *)
  (* Always give the default action for now *)
  let action =
    match default with Some (action, _) -> Some action | None -> None
  in
  (* Calling an apply method on a table instance returns a value with
     a struct type with three fields. This structure is synthesized
     by the compiler automatically. (14.2.2) *)
  let value =
    let hit = Value.BoolV (Option.is_some action) in
    let miss = Value.BoolV (Option.is_none action) in
    (* For enum values without an underlying type the default value is
       the first value that appears in the enum type declaration. (7.3) *)
    let action_run =
      let action_run =
        Option.value ~default:(List.hd actions) action
        |> fst
        |> Format.asprintf "%a" Syntax.Print.print_var
      in
      Value.EnumFieldV action_run
    in
    Value.StructV [ ("hit", hit); ("miss", miss); ("action_run", action_run) ]
  in
  (action, value)
