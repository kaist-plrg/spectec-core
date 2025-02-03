module F = Format
module L = Lang.Ast
module Value = Runtime_static.Vdomain.Value
module LValue = Runtime_dynamic.Lvalue
open Util.Pp

module ASig = struct
  type t = Cont of LValue.t option | Reject of Value.t | Exit

  let pp fmt = function
    | Cont _ -> F.fprintf fmt "ACont"
    | Reject _ -> F.fprintf fmt "AReject"
    | Exit -> F.fprintf fmt "AExit"
end

module LSig = struct
  type t = Cont of LValue.t | Reject of Value.t | Exit

  let pp fmt = function
    | Cont _ -> F.fprintf fmt "LCont"
    | Reject _ -> F.fprintf fmt "LReject"
    | Exit -> F.fprintf fmt "LExit"
end

module ESig = struct
  type t =
    | Cont of [ `Single of Value.t | `Multiple of Value.t list ]
    | Reject of Value.t
    | Exit

  let pp fmt = function
    | Cont (`Single _) -> F.fprintf fmt "ECont"
    | Cont (`Multiple _) -> F.fprintf fmt "EConts"
    | Reject _ -> F.fprintf fmt "EReject"
    | Exit -> F.fprintf fmt "EExit"
end

module Sig = struct
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

module DSig = struct
  type t = Cont | Reject of Value.t | Exit

  let pp fmt = function
    | Cont -> F.fprintf fmt "DCont"
    | Reject _ -> F.fprintf fmt "DReject"
    | Exit -> F.fprintf fmt "DExit"
end

module CSig = struct
  type t = Cont of Ctx.t * LValue.t option list | Reject of Value.t | Exit

  let pp fmt = function
    | Cont _ -> F.fprintf fmt "CCont"
    | Reject _ -> F.fprintf fmt "CReject"
    | Exit -> F.fprintf fmt "CExit"
end
