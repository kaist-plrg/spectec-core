open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Cclos
open Runtime.Context

let init (ctx : Ctx.t) =
  (* Add "b" to the object environment *)
  let typ = Type.BitT (Bigint.of_int 32) in
  let value = Value.BitV (Bigint.of_int 32, Bigint.of_int 42) in
  Ctx.add_var_obj "b" typ value ctx

let make_args (args : Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

let drive_proto (ctx : Ctx.t) =
  let value = Value.RefV [ "main"; "_p" ] in
  let targs = [] in
  let args = make_args [ "b" ] in
  Interp.interp_method_call ctx value "apply" targs args |> fst

let drive (_ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
  let ctx = init ctx in
  Interp.init sto;
  Format.printf "Initial context\n%a@\n" Ctx.pp ctx;
  let ctx = drive_proto ctx in
  Format.printf "After calling main._p\n%a@\n" Ctx.pp ctx;
  ()
