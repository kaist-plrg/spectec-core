open Syntax.Ast
open Base_env

type t =
  | MethodF of {
      bscope: Scope.t;
      fscope: Scope.t;
      tparams: string list;
      params: param list;
      body: block;
    }
  | StateF of {
      bscope: Scope.t;
      fscope: Scope.t;
      tparams: string list;
      params: param list;
      body: block;
    }
  | ActionF of {
      bscope: Scope.t;
      fscope: Scope.t;
      tparams: string list;
      params: param list;
      body: block;
    }
  | TableF of {
      bscope: Scope.t;
      fscope: Scope.t;
    }
  | ExternF

let pp fmt = function
  | MethodF _ -> Format.fprintf fmt "method"
  | StateF _ -> Format.fprintf fmt "state"
  | ActionF _ -> Format.fprintf fmt "action"
  | TableF _ -> Format.fprintf fmt "table"
  | ExternF -> Format.fprintf fmt "extern"
