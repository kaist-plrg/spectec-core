module F = Format
module Value = Runtime_static.Vdomain.Value
module LValue = Runtime_dynamic.Lvalue

type t = Cont of Ctx.t * LValue.t option list | Reject of Value.t | Exit

let pp fmt = function
  | Cont _ -> F.fprintf fmt "Cont"
  | Reject value -> F.fprintf fmt "Reject %a" (Value.pp ~level:0) value
  | Exit -> F.fprintf fmt "Exit"
