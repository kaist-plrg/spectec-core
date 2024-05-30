open Syntax.Ast
open Runtime.Context

(* Logic for match-action table *)

let match_action (ctx : Ctx.t)
    (key : table_key list) (_actions : table_action list)
    (_entries : table_entry list) (default : table_default option)
    (_custom : table_custom list) =
  let _key =
    List.map (fun (expr, mtch) -> (Eval.eval_expr ctx expr, mtch)) key
  in
  (* Return the default action for now *)
  match default with
  | Some (action, _) -> Some action
  | None -> None
