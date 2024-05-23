open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Object
open Runtime_.Context

let init (env_glob: env_glob) =
  (* Initialize the context *)
  let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
  let env_loc = (TDEnv.empty, []) in
  let sto_loc = Sto.empty in
  let ctx = Ctx.init env_glob env_obj env_loc sto_loc in
  (* Add "b" to the object environment *)
  let typ = Runtime_.Base.Type.BitT (Bigint.of_int 32) in
  let value = Runtime_.Base.Value.BitV (Bigint.of_int 32, Bigint.of_int 42) in
  Ctx.add_var_obj "b" typ value ctx

let make_args (args: Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

let drive_proto (path: Path.t) (sto_obj: Sto.t) (ctx: Ctx.t) =
  let obj_proto = Sto.find (path @ [ "_p" ]) sto_obj |> Option.get in
  let targs = [] in
  let args = make_args [ "b" ] in
  Interp.interp_method_call ctx obj_proto "apply" targs args

let drive (sto_glob: Sto.t) (ictx_glob: ICtx.t) =
  let env_glob = ictx_glob.glob in
  Interp.init env_glob;
  let path = [ "main" ] in
  let obj_main = Sto.find path sto_glob |> Option.get in
  let sto_obj =
    match obj_main with
    | PackageO { sto_obj } -> sto_obj
    | _ -> assert false
  in
  let ctx = init env_glob in
  Format.printf "Ctx = %a@." Ctx.pp ctx;
  let ctx = drive_proto path sto_obj ctx in
  Format.printf "Got Ctx = %a@." Ctx.pp ctx;
  ()
