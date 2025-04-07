open Domain.Lib
open Runtime_static.Envs

(* Context for dataflow analysis *)

type t = {
  (* Input hints for rules *)
  hints : HEnv.t;
  (* Free identifiers over the entire definition *)
  frees : IdSet.t;
  (* Bound variables so far *)
  bounds : VEnv.t;
  (* Typedefs so far *)
  tdenv : TDEnv.t;
}

(* Constructors *)

let init (ctx : Ctx.t) : t =
  let hints = REnv.map (fun (_, hint, _) -> hint) ctx.renv in
  let frees = ctx.frees in
  let bounds = ctx.venv in
  let tdenv = ctx.tdenv in
  { hints; frees; bounds; tdenv }

(* Promoter *)

let promote (ctx : Ctx.t) (dctx : t) (venv : VEnv.t) : Ctx.t =
  let frees = dctx.frees in
  let venv = VEnv.union (fun _ -> assert false) ctx.venv venv in
  { ctx with frees; venv }

(* Adders *)

let add_free (dctx : t) (id : Id.t) =
  let frees = IdSet.add id dctx.frees in
  { dctx with frees }

(* Finders *)

let find_hint (dctx : t) (id : Id.t) = HEnv.find id dctx.hints
let find_typdef (dctx : t) (tid : TId.t) = TDEnv.find tid dctx.tdenv
