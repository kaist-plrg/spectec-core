open Domain.Dom
open El.Ast
open Util.Source
module TEnv = MakeIdEnv (Type)
module TDEnv = MakeTIdEnv (Typedef)

type t = { tenv : TEnv.t; tdenv : TDEnv.t }

(* Constructors *)

let empty = { tenv = TEnv.empty; tdenv = TDEnv.empty }

(* Adders *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typedef.t) : t =
  { ctx with tdenv = TDEnv.add_nodup tid td ctx.tdenv }

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left
    (fun ctx tparam -> add_typdef ctx tparam.it Typedef.Param)
    ctx tparams

(* Finders *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typedef.t option =
  TDEnv.find_opt tid ctx.tdenv

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typedef.t) : t =
  if find_typdef_opt ctx tid |> Option.is_none then assert false;
  { ctx with tdenv = TDEnv.add tid td ctx.tdenv }
