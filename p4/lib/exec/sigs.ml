module F = Format
module L = Lang.Ast
module Value = Runtime_value.Value
module LValue = Runtime_dynamic.Lvalue
open Util.Pp

module Sig = struct
  type 'a t = Cont of 'a | Reject of Value.t | Exit

  let pp fmt = function
    | Cont _ -> F.fprintf fmt "Cont"
    | Reject _ -> F.fprintf fmt "Reject"
    | Exit -> F.fprintf fmt "Exit"
end

module SSig = struct
  type t =
    | Cont
    | Ret of Value.t option
    | Trans of [ `Accept | `Reject of Value.t | `State of L.state_label' ]
    | Exit

  let pp fmt = function
    | Cont -> F.fprintf fmt "Cont"
    | Ret _ -> F.fprintf fmt "Ret"
    | Trans `Accept -> F.fprintf fmt "Accept"
    | Trans (`Reject _) -> F.fprintf fmt "Reject"
    | Trans (`State _) -> F.fprintf fmt "State"
    | Exit -> F.fprintf fmt "Exit"
end
