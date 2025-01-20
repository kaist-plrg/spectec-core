module L = Lang.Ast
module Value = Runtime_static.Vdomain.Value

type t =
  | Cont
  | Ret of Value.t option
  | Trans of [ `Accept | `Reject of Value.t | `State of L.state_label' ]
  | Exit

let pp fmt = function
  | Cont -> Format.fprintf fmt "Cont"
  | Ret None -> Format.fprintf fmt "Ret"
  | Ret (Some value) -> Format.fprintf fmt "Ret %a" (Value.pp ~level:0) value
  | Trans `Accept -> Format.fprintf fmt "Accept"
  | Trans (`Reject value) ->
      Format.fprintf fmt "Reject %a" (Value.pp ~level:0) value
  | Trans (`State label) ->
      Format.fprintf fmt "State %a" Lang.Pp.pp_state_label' label
  | Exit -> Format.fprintf fmt "Exit"
