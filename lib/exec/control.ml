open Syntax.Ast
open Runtime

(* Models the control plane
   1. Match against the given keys to find an appropriate action
   2. Provide directionless arguments to the action *)

let match_action (_keys : (Value.t * string) list) (_actions : Table.action_ref list) (default : Table.action_ref option) =
  match default with
  | Some { name; args; _ } -> Some (name, args)
  | None -> None
