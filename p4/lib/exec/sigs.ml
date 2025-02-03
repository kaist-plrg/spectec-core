module L = Lang.Ast
module Value = Runtime_static.Vdomain.Value
module LValue = Runtime_dynamic.Lvalue
open Util.Pp

module ESig = struct
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
end

module Sig = struct
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
end

module DSig = struct
  type t = Cont | Reject of Value.t | Exit

  let pp fmt = function
    | Cont -> F.fprintf fmt "Cont"
    | Reject value -> F.fprintf fmt "Reject %a" (Value.pp ~level:0) value
    | Exit -> F.fprintf fmt "Exit"
end

module CSig = struct
  type t = Cont of Ctx.t * LValue.t option list | Reject of Value.t | Exit

  let pp fmt = function
    | Cont _ -> F.fprintf fmt "Cont"
    | Reject value -> F.fprintf fmt "Reject %a" (Value.pp ~level:0) value
    | Exit -> F.fprintf fmt "Exit"
end
