open Syntax.Ast
open Utils
module Env = Scope.Env
module Heap = Scope.Heap

(* (TODO) maybe consider wrapping them inside modules later,
   but at the cost of having to write mutually-recursive modules
   in clumsy way. *)

(* Values *)

type value =
  | VBool of bool
  | VAInt of Bigint.t
  | VInt of { value : Bigint.t; width : Bigint.t }
  | VBit of { value : Bigint.t; width : Bigint.t }
  | VString of string
  | VError
  | VTuple of value list
  | VStruct of { entries : (string * value) list }
  | VHeader of { valid : bool; entries : (string * value) list }
  (* Reference type *)
  (* (TODO) A hack for now, representing pointers to objects *)
  | VRef of Path.t

(* Objects *)
and obj =
  | OPackage of { tdenv : tdenv; genv : env; tsto : tsto; vsto : vsto }
  | OParser of { tdenv : tdenv; tsto : tsto; vsto : vsto; funcs : func list }
  | OControl of { tdenv : tdenv; tsto : tsto; vsto : vsto; funcs : func list }
  | OExtern of { tdenv : tdenv; tsto : tsto; vsto : vsto; funcs : func list }
  | OTable of { genv : env; lenv : env; properties : Table.property list }
  | OFunction
  | OValueSet

(* Types *)
and typ =
  (* Base types *)
  | TBool
  | TAInt
  | TInt of { width : Bigint.t }
  | TBit of { width : Bigint.t }
  | TVBit of { width : Bigint.t }
  | TArray of { typ : typ; size : Bigint.t }
  | TString
  | TError
  | TTuple of typ list
  | TEnum of { entries : string list }
  | TSEnum of { typ : typ; entries : string list }
  | THeader of { entries : (string * typ) list }
  | TUnion of { entries : (string * typ) list }
  | TStruct of { entries : (string * typ) list }
  | TName of { name : string }
  | TNewType of { name : string }
  (* Reference type *)
  (* (TODO) A hack for now, representing objects *)
  | TRef

(* Functions *)
and func =
  | FNormal of {
      name : string;
      params : Parameter.t list;
      genv : env;
      lenv : env;
      body : Statement.t list;
    }
  (* (TODO) Consider refactoring this into FNormal,
     by merging Parser.transition into Statement.t *)
  | FParser of {
      name : string;
      params : Parameter.t list;
      genv : env;
      lenv : env;
      body : Statement.t list;
      transition : Parser.transition;
    }
  | FExtern of { name : string; params : Parameter.t list; genv : env }

(* Constructor closure *)
and cclos =
  | CCPackage of { params : Parameter.t list; tparams : string list }
  | CCParser of {
      tdenv : tdenv;
      genv : env;
      tsto : tsto;
      vsto : vsto;
      params : Parameter.t list;
      tparams : string list;
      cparams : Parameter.t list;
      locals : Declaration.t list;
      states : Parser.state list;
    }
  | CCControl of {
      tdenv : tdenv;
      genv : env;
      tsto : tsto;
      vsto : vsto;
      params : Parameter.t list;
      tparams : string list;
      cparams : Parameter.t list;
      locals : Declaration.t list;
      apply : Block.t;
    }
  | CCExtern of {
      tdenv : tdenv;
      genv : env;
      tsto : tsto;
      vsto : vsto;
      tparams : string list;
      cparams : Parameter.t list;
      methods : MethodPrototype.t list;
    }

(* Environments and Stores *)
and tdenv = typ Env.t
and ccenv = cclos Env.t
and ienv = obj Env.t
and env = Loc.t Env.t
and vsto = value Heap.t
and tsto = typ Heap.t
