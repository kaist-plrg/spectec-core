open Syntax.Ast
open Domain
open Base

(* Visibility of functions,
   defined earlier than Func to resolve recursion *)

module FVis = MakeVis (Var)

(* Runtime representation of functions *)

module Func = struct
  type t =
    | MethodF of {
        bscope : VVis.t * FVis.t;
        tparams : string list;
        params : param list;
        body : block;
      }
    | StateF of {
        bscope : VVis.t * FVis.t;
        tparams : string list;
        params : param list;
        body : block;
      }
    | ActionF of {
        bscope : VVis.t * FVis.t;
        tparams : string list;
        params : param list;
        body : block;
      }
    | TableF of { bscope : VVis.t * FVis.t }
    | ExternF

  let pp fmt = function
    | MethodF _ -> Format.fprintf fmt "method"
    | StateF _ -> Format.fprintf fmt "state"
    | ActionF _ -> Format.fprintf fmt "action"
    | TableF _ -> Format.fprintf fmt "table"
    | ExternF -> Format.fprintf fmt "extern"
end

(* Environment map function names to functions *)

module FEnv = MakeEnv (Var) (Func)
