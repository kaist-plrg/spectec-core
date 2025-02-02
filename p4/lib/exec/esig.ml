module F = Format
module Value = Runtime_static.Vdomain.Value
open Util.Pp

type t =
  | Cont of [ `Single of Value.t | `Multiple of Value.t list ]
  | Reject of Value.t
  | Exit

let pp fmt = function
  | Cont (`Single value) -> F.fprintf fmt "Cont %a" (Value.pp ~level:0) value
  | Cont (`Multiple values) ->
      F.fprintf fmt "Cont [ %a ]" (pp_list ~level:0 Value.pp ~sep:Nl) values
  | Reject value -> F.fprintf fmt "Reject %a" (Value.pp ~level:0) value
  | Exit -> F.fprintf fmt "Exit"
