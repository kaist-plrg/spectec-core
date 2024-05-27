open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Cclos
open Runtime_.Object
open Runtime_.Context

let path = []

let init_instantiate_packet_in (ccenv: CCEnv.t) (ctx: Ctx.t) =
  let cclos_packet_in = CCEnv.find "packet_in" ccenv |> Option.get in
  let ictx = ctx_to_ictx ctx in
  let obj_packet_in =
    Instance_.Instantiate.instantiate_from_cclos ccenv ictx path cclos_packet_in [] []
  in
  Ctx.add_obj (path @ [ "packet" ]) obj_packet_in ctx

let init_ctx (_ccenv: CCEnv.t) (sto_glob: Sto.t) (ictx_glob : ICtx.t) =
  (* Initialize the context *)
  let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
  let env_loc = (TDEnv.empty, []) in
  let sto_loc = sto_glob in
  let ctx = Ctx.init ictx_glob.glob env_obj env_loc sto_loc in
  (* Add "packet" to the object environment and store *)
  let ctx = init_instantiate_packet_in _ccenv ctx in
  let ctx =
    let typ = Type.RefT in
    let value = Value.RefV [ "packet" ] in
    Ctx.add_var_obj "packet" typ value ctx
  in
  (* Add "hdr" to the object environment *)
  let ctx =
    let typ = Ctx.find_td "headers" ctx |> Option.get in
    let value = Runtime_.Ops.eval_default_value typ in
    Ctx.add_var_obj "hdr" typ value ctx
  in
  (* Add "meta" to the object environment *)
  let ctx =
    let typ = Ctx.find_td "metadata" ctx |> Option.get in
    let value = Runtime_.Ops.eval_default_value typ in
    Ctx.add_var_obj "meta" typ value ctx
  in
  (* Add "standard_metadata" to the object environment *)
  let ctx =
    let typ = Ctx.find_td "standard_metadata_t" ctx |> Option.get in
    let value = Runtime_.Ops.eval_default_value typ in
    Ctx.add_var_obj "standard_metadata" typ value ctx
  in
  ctx

let make_args (args : Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

let drive_parser_impl (ctx: Ctx.t) =
  let obj_main = Sto.find [ "main" ] ctx.sto |> Option.get in
  let sto_main =
    match obj_main with PackageO { sto_obj } -> sto_obj | _ -> assert false
  in
  let obj_parser_impl = Sto.find [ "main"; "p" ] sto_main |> Option.get in
  let targs = [] in
  let args =
    make_args [ "packet"; "hdr"; "meta"; "standard_metadata" ]
  in
  Interp.interp_method_call ctx obj_parser_impl "apply" targs args

let drive (ccenv : CCEnv.t) (sto_glob : Sto.t) (ictx_glob : ICtx.t) =
  Interp.init ictx_glob.glob;
  let ctx = init_ctx ccenv sto_glob ictx_glob in
  let _ctx = drive_parser_impl ctx in
  ()
