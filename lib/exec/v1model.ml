open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Cclos
open Runtime.Context
open Runtime.Signal
open Driver

let make_func (path : string list) (func : string) =
  let base, members =
    match path with [] -> assert false | base :: members -> (base, members)
  in
  let expr =
    List.fold_left
      (fun acc member -> ExprAccE (acc, member))
      (VarE (Bare base)) members
  in
  ExprAccE (expr, func)

let make_args (args : Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

module Make (Interp : INTERP) : ARCH = struct
  type extern = PacketIn of Core.PacketIn.t | PacketOut of Core.PacketOut.t

  let pp_extern fmt = function
    | PacketIn pkt_in -> Core.PacketIn.pp fmt pkt_in
    | PacketOut pkt_out -> Core.PacketOut.pp fmt pkt_out

  module EM = Map.Make (String)

  let externs = ref EM.empty

  let init_instantiate_packet_in (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
    let cclos_packet_in = CCEnv.find "packet_in" ccenv |> Option.get in
    let ictx = ICtx.init ctx.env_glob ctx.env_obj in
    let path = [ "packet_in" ] in
    let sto =
      Instance.Instantiate.instantiate_from_cclos ccenv sto ictx path
        cclos_packet_in [] []
    in
    sto

  let init_instantiate_packet_out (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t)
      =
    let cclos_packet_out = CCEnv.find "packet_out" ccenv |> Option.get in
    let ictx = ICtx.init ctx.env_glob ctx.env_obj in
    let path = [ "packet_out" ] in
    let sto =
      Instance.Instantiate.instantiate_from_cclos ccenv sto ictx path
        cclos_packet_out [] []
    in
    sto

  let init_var (ctx : Ctx.t) (tname : string) (vname : string) =
    let typ = Ctx.find_td tname ctx |> Option.get in
    let value = Runtime.Ops.eval_default_value typ in
    Ctx.add_var_obj vname typ value ctx

  let init (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
    (* Add "packet_in" and "packet_out" to the store and object environment *)
    let sto = init_instantiate_packet_in ccenv sto ctx in
    let ctx =
      let typ = Type.RefT in
      let value = Value.RefV [ "packet_in" ] in
      Ctx.add_var_obj "packet_in" typ value ctx
    in
    let sto = init_instantiate_packet_out ccenv sto ctx in
    let ctx =
      let typ = Type.RefT in
      let value = Value.RefV [ "packet_out" ] in
      Ctx.add_var_obj "packet_out" typ value ctx
    in
    (* Add "hdr" to the object environment *)
    let ctx = init_var ctx "headers" "hdr" in
    (* Add "meta" to the object environment *)
    let ctx = init_var ctx "metadata" "meta" in
    (* Add "standard_metadata" to the object environment *)
    let ctx = init_var ctx "standard_metadata_t" "standard_metadata" in
    (sto, ctx)

  let drive_p (ctx : Ctx.t) =
    let func = make_func [ "main"; "p" ] "apply" in
    let targs = [] in
    let args = make_args [ "packet_in"; "hdr"; "meta"; "standard_metadata" ] in
    Format.printf "\nBefore %a call\n%a@." Syntax.Print.print_expr func
      Ctx.pp_var ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_vr (ctx : Ctx.t) =
    let func = make_func [ "main"; "vr" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta" ] in
    Format.printf "\nBefore %a call\n%a@." Syntax.Print.print_expr func
      Ctx.pp_var ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_ig (ctx : Ctx.t) =
    let func = make_func [ "main"; "ig" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta"; "standard_metadata" ] in
    Format.printf "\nBefore %a call\n%a@." Syntax.Print.print_expr func
      Ctx.pp_var ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_eg (ctx : Ctx.t) =
    let func = make_func [ "main"; "eg" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta"; "standard_metadata" ] in
    Format.printf "\nBefore %a call\n%a@." Syntax.Print.print_expr func
      Ctx.pp_var ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_ck (ctx : Ctx.t) =
    let func = make_func [ "main"; "ck" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta" ] in
    Format.printf "\nBefore %a call\n%a@." Syntax.Print.print_expr func
      Ctx.pp_var ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_dep (ctx : Ctx.t) =
    let func = make_func [ "main"; "dep" ] "apply" in
    let targs = [] in
    let args = make_args [ "packet_out"; "hdr" ] in
    Format.printf "\nBefore %a call\n%a@." Syntax.Print.print_expr func
      Ctx.pp_var ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_pipe (ctx : Ctx.t) =
    ctx |> drive_p |> drive_vr |> drive_ig |> drive_eg |> drive_ck |> drive_dep

  let drive_stf (ctx : Ctx.t) (stf : Stf.Ast.stmt) =
    match stf with
    | Stf.Ast.Packet (_port, packet) ->
        (* (TODO) Update input port *)
        (* Update packet_in and out *)
        let pkt_in = PacketIn (Core.PacketIn.init packet) in
        externs := EM.add "packet_in" pkt_in !externs;
        let pkt_out = PacketOut Core.PacketOut.init in
        externs := EM.add "packet_out" pkt_out !externs;
        drive_pipe ctx
    | Stf.Ast.Expect (_port, Some packet) ->
        (* Check packet_out *)
        let pkt_out = EM.find "packet_out" !externs in
        Format.printf "Expected %s\n" packet;
        Format.printf "Actual %a@." pp_extern pkt_out;
        ctx
    | _ -> ctx

  let drive (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t)
      (stf : Stf.Ast.stmt list) =
    let sto, ctx = init ccenv sto ctx in
    Interp.init sto;
    let ctx = List.fold_left (fun ctx stf -> drive_stf ctx stf) ctx stf in
    Format.printf "\nFinal v1model driver context\n%a@." Ctx.pp_var ctx;
    ()

  let interp_extern (ctx : Ctx.t) =
    match ctx.id with
    | [ "packet_in" ], "extract" -> (
        match EM.find "packet_in" !externs with
        | PacketIn pkt_in ->
            let ctx, pkt_in = Core.PacketIn.extract ctx pkt_in in
            externs := EM.add "packet_in" (PacketIn pkt_in) !externs;
            (Sig.Ret None, ctx)
        | _ -> assert false)
    | [ "packet_out" ], "emit" -> (
        match EM.find "packet_out" !externs with
        | PacketOut pkt_out ->
            let ctx, pkt_out = Core.PacketOut.emit ctx pkt_out in
            externs := EM.add "packet_out" (PacketOut pkt_out) !externs;
            (Sig.Ret None, ctx)
        | _ -> assert false)
    | [], "verify_checksum" ->
        Hash.verify_checksum ctx |> fun ctx -> (Sig.Ret None, ctx)
    | [], "update_checksum" ->
        Hash.update_checksum ctx |> fun ctx -> (Sig.Ret None, ctx)
    | _ ->
        let path, func = ctx.id in
        Format.eprintf "Unknown builtin extern method %a.%a@." Path.pp path
          Var.pp func;
        assert false
end
