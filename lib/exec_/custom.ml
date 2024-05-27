open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Cclos
open Runtime_.Context

(* Imagine we have a "driver" context (Ctx.t) that,
   i) can see all global variables, and
   ii) can see all objects
   Where the arguments to the "apply" methods of programmable blocks are
   stored in the object-local scope of the "driver" context,
   so that the mutations to the arguments are visible to the "driver" context
*)

let init (env_glob : env_glob) =
  (* Initialize the context *)
  let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
  let env_loc = (TDEnv.empty, []) in
  let ctx = Ctx.init env_glob env_obj env_loc in
  (* Add "b" to the object environment *)
  let typ = Runtime_.Base.Type.BitT (Bigint.of_int 32) in
  let value = Runtime_.Base.Value.BitV (Bigint.of_int 32, Bigint.of_int 42) in
  Ctx.add_var_obj "b" typ value ctx

let make_args (args : Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

let drive_proto (gctx : GCtx.t) (ctx : Ctx.t) =
  let path = [ "main"; "_p" ] in
  let obj_proto = GCtx.find_obj path gctx |> Option.get in
  let targs = [] in
  let args = make_args [ "b" ] in
  Interp.interp_method_call ctx obj_proto "apply" targs args

let drive (_ccenv : CCEnv.t) (gctx : GCtx.t) =
  Interp.init gctx;
  let ctx = init gctx.glob in
  Format.printf "Initial context\n%a@\n" Ctx.pp ctx;
  let ctx = drive_proto gctx ctx in
  Format.printf "After calling main._p\n%a@\n" Ctx.pp ctx;
  ()
