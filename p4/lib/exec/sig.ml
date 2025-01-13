module L = Lang.Ast
module Value = Runtime_static.Value

type t =
  | Cont
  | Ret of Value.t option
  | Trans of [ `Accept | `Reject of Value.t | `State of L.state_label' ]
  | Exit
