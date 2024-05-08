open Surface.Ast
open Scope

type t =
  | CCPackage of { params : Parameter.t list; tparams : string list }
  | CCParser of {
      tdenv : TDEnv.t;
      genv : Env.t;
      sto : Sto.t;
      params : Parameter.t list;
      tparams : string list;
      cparams : Parameter.t list;
      locals : Declaration.t list;
      states : Parser.state list;
    }
  | CCControl of {
      tdenv : TDEnv.t;
      genv : Env.t;
      sto : Sto.t;
      params : Parameter.t list;
      tparams : string list;
      cparams : Parameter.t list;
      locals : Declaration.t list;
      apply : Block.t;
    }
  | CCExtern of {
      tdenv : TDEnv.t;
      genv : Env.t;
      sto : Sto.t;
      tparams : string list;
      cparams : Parameter.t list;
      methods : MethodPrototype.t list;
    }

let pp fmt _cclos = Format.fprintf fmt "cclos"
