open Domain.Lib
open Envs

type t = {
  (* Input hints for rules *)
  hints : Hint.HEnv.t;
  (* Free identifiers over the entire definition *)
  frees : IdSet.t;
  (* Bound variables so far *)
  bounds : VEnv.t;
}

(* Constructors *)

let init (ctx : Ctx.t) : t =
  let hints = REnv.map (fun (_, hint, _) -> hint) ctx.renv in
  let frees = ctx.frees in
  let bounds = ctx.venv in
  { hints; frees; bounds }

(* Adders *)

let add_free (dctx : t) (id : Id.t) =
  let frees = IdSet.add id dctx.frees in
  { dctx with frees }

(* Finders *)

let find_hint (dctx : t) (id : Id.t) = Hint.HEnv.find id dctx.hints
