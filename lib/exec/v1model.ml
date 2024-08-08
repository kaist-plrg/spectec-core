open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Cclos
open Runtime.Context
open Runtime.Signal
open Util.Source
open Driver

let make_func (path : string list) (func : string) =
  let base, members =
    match path with [] -> assert false | base :: members -> (base, members)
  in
  let expr =
    List.fold_left
      (fun acc member -> ExprAccE (acc, member $ no_info) $ no_info)
      (VarE (Bare (base $ no_info) $ no_info) $ no_info)
      members
  in
  ExprAccE (expr, func $ no_info) $ no_info

let make_args (args : Id.t list) =
  List.map
    (fun arg ->
      ExprA (VarE (Bare (arg $ no_info) $ no_info) $ no_info) $ no_info)
    args

module Make (Interp : INTERP) : ARCH = struct
  (* Configuration *)

  let config = ref Config.default
  let configure (_config : Config.t) = config := _config

  (* Extern objects *)

  type extern = PacketIn of Core.PacketIn.t | PacketOut of Core.PacketOut.t

  let pp_extern fmt = function
    | PacketIn pkt_in -> Core.PacketIn.pp fmt pkt_in
    | PacketOut pkt_out -> Core.PacketOut.pp fmt pkt_out

  module Externs = Map.Make (String)

  let externs = ref Externs.empty

  (* Initialization and pipeline driver *)

  let init_instantiate_packet_in (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
    let cclos_packet_in = CCEnv.find ("packet_in", []) ccenv in
    let ictx = ICtx.init ctx.env_glob ctx.env_obj in
    let path = [ "packet_in" ] in
    let sto =
      Instance.Instantiate.instantiate_from_cclos ccenv sto ictx path
        cclos_packet_in [] []
    in
    sto

  let init_instantiate_packet_out (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t)
      =
    let cclos_packet_out = CCEnv.find ("packet_out", []) ccenv in
    let ictx = ICtx.init ctx.env_glob ctx.env_obj in
    let path = [ "packet_out" ] in
    let sto =
      Instance.Instantiate.instantiate_from_cclos ccenv sto ictx path
        cclos_packet_out [] []
    in
    sto

  let init_var (ctx : Ctx.t) (tid : Id.t) (vid : Id.t) =
    let typ = Ctx.find_td tid ctx in
    let value = Runtime.Ops.eval_default_value typ in
    Ctx.add_var_obj vid value ctx

  let init (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
    (* Add "packet_in" and "packet_out" to the store and object environment *)
    let sto = init_instantiate_packet_in ccenv sto ctx in
    let ctx =
      let value = Value.RefV [ "packet_in" ] in
      Ctx.add_var_obj "packet_in" value ctx
    in
    let sto = init_instantiate_packet_out ccenv sto ctx in
    let ctx =
      let value = Value.RefV [ "packet_out" ] in
      Ctx.add_var_obj "packet_out" value ctx
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
    if !config.debug then
      Format.printf "\nBefore %a call\n%a@." Syntax.Pp.pp_expr func Ctx.pp ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_vr (ctx : Ctx.t) =
    let func = make_func [ "main"; "vr" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta" ] in
    if !config.debug then
      Format.printf "\nBefore %a call\n%a@." Syntax.Pp.pp_expr func Ctx.pp ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_ig (ctx : Ctx.t) =
    let func = make_func [ "main"; "ig" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta"; "standard_metadata" ] in
    if !config.debug then
      Format.printf "\nBefore %a call\n%a@." Syntax.Pp.pp_expr func Ctx.pp ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_eg (ctx : Ctx.t) =
    let func = make_func [ "main"; "eg" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta"; "standard_metadata" ] in
    if !config.debug then
      Format.printf "\nBefore %a call\n%a@." Syntax.Pp.pp_expr func Ctx.pp ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_ck (ctx : Ctx.t) =
    let func = make_func [ "main"; "ck" ] "apply" in
    let targs = [] in
    let args = make_args [ "hdr"; "meta" ] in
    if !config.debug then
      Format.printf "\nBefore %a call\n%a@." Syntax.Pp.pp_expr func Ctx.pp ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_dep (ctx : Ctx.t) =
    let func = make_func [ "main"; "dep" ] "apply" in
    let targs = [] in
    let args = make_args [ "packet_out"; "hdr" ] in
    if !config.debug then
      Format.printf "\nBefore %a call\n%a@." Syntax.Pp.pp_expr func Ctx.pp ctx;
    Interp.interp_call ctx func targs args |> snd

  let drive_pipe (ctx : Ctx.t) =
    ctx |> drive_p |> drive_vr |> drive_ig |> drive_eg |> drive_ck |> drive_dep

  let drive_stf (ctx : Ctx.t) (stf : Stf.Ast.stmt) =
    match stf with
    | Stf.Ast.Packet (port, packet) ->
        (* TODO : Should we need to check range of port value? *)
        let nine = Bigint.of_int 9 in
        let port = int_of_string port |> Bigint.of_int in
        let port = Value.BitV (nine, port) in
        let update_port field = match field with 
          | "ingress_port", _ -> "ingress_port", port
          | id, mem -> id, mem 
        in
        let std_meta = Ctx.find_var "standard_metadata" ctx in
        let std_meta = match std_meta with
          | StructV fields -> Value.StructV (List.map update_port fields)
          | _ -> failwith "standard_metadata changed to other type"
        in
        let ctx = Ctx.update_var "standard_metadata" std_meta ctx in
        (* Update packet_in and out *)
        let pkt_in = PacketIn (Core.PacketIn.init packet) in
        externs := Externs.add "packet_in" pkt_in !externs;
        let pkt_out = PacketOut Core.PacketOut.init in
        externs := Externs.add "packet_out" pkt_out !externs;
        drive_pipe ctx
    | Stf.Ast.Expect (port, Some packet) ->
        (* Check packet_out *)
        let port = int_of_string port in
        let std_meta = Ctx.find_var "standard_metadata" ctx in
        let port' = Value.access_field "egress_spec" std_meta 
          |> Value.get_num |> Bigint.to_int |> Option.get in
        let pkt_out = Externs.find "packet_out" !externs in
        let expected = String.uppercase_ascii packet in
        let actual = Format.asprintf "%a" pp_extern pkt_out in
        let result = if port = port' && Stf.Compare.equals actual expected 
          then "PASS" else "FAIL" in
        Format.printf "[%s] Expected %s, Port : %i / Actual %s, Port : %i\n" 
          result expected port actual port';
        ctx
    | _ -> ctx

  let drive (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t)
      (stf : Stf.Ast.stmt list) =
    let sto, ctx = init ccenv sto ctx in
    Interp.init sto;
    let ctx = List.fold_left (fun ctx stf -> drive_stf ctx stf) ctx stf in
    if !config.debug then
      Format.printf "\nFinal v1model driver context\n%a@." Ctx.pp ctx;
    ()

  let interp_extern (ctx : Ctx.t) =
    match ctx.id with
    | [ "packet_in" ], ("extract", [ "hdr" ]) -> (
        match Externs.find "packet_in" !externs with
        | PacketIn pkt_in ->
            let ctx, pkt_in = Core.PacketIn.extract ctx pkt_in in
            externs := Externs.add "packet_in" (PacketIn pkt_in) !externs;
            (Sig.Ret None, ctx)
        | _ -> assert false)
    | ( [ "packet_in" ],
        ("extract", [ "variableSizeHeader"; "variableFieldSizeInBits" ]) ) -> (
        match Externs.find "packet_in" !externs with
        | PacketIn pkt_in ->
            let ctx, pkt_in = Core.PacketIn.extract_var ctx pkt_in in
            externs := Externs.add "packet_in" (PacketIn pkt_in) !externs;
            (Sig.Ret None, ctx)
        | _ -> assert false)
    | [ "packet_in" ], ("lookahead", []) -> (
        match Externs.find "packet_in" !externs with
        | PacketIn pkt_in ->
            let ctx, value = Core.PacketIn.lookahead ctx pkt_in in
            (Sig.Ret (Some value), ctx)
        | _ -> assert false)
    | [ "packet_in" ], ("advance", [ "sizeInBits" ]) -> (
        match Externs.find "packet_in" !externs with
        | PacketIn pkt_in ->
            let ctx, pkt_in = Core.PacketIn.advance ctx pkt_in in
            externs := Externs.add "packet_in" (PacketIn pkt_in) !externs;
            (Sig.Ret None, ctx)
        | _ -> assert false)
    | [ "packet_in" ], ("length", []) -> (
        match Externs.find "packet_in" !externs with
        | PacketIn pkt_in ->
            let ctx, value = Core.PacketIn.length ctx pkt_in in
            (Sig.Ret (Some value), ctx)
        | _ -> assert false)
    | [ "packet_out" ], ("emit", [ "hdr" ]) -> (
        match Externs.find "packet_out" !externs with
        | PacketOut pkt_out ->
            let ctx, pkt_out = Core.PacketOut.emit ctx pkt_out in
            externs := Externs.add "packet_out" (PacketOut pkt_out) !externs;
            (Sig.Ret None, ctx)
        | _ -> assert false)
    | [], ("hash", [ "result"; "algo"; "base"; "data"; "max" ]) ->
        Hash.hash ctx |> fun ctx -> (Sig.Ret None, ctx)
    | [], ("verify_checksum", [ "condition"; "data"; "checksum"; "algo" ]) ->
        Hash.verify_checksum ctx |> fun ctx -> (Sig.Ret None, ctx)
    | [], ("update_checksum", [ "condition"; "data"; "checksum"; "algo" ]) ->
        Hash.update_checksum ctx |> fun ctx -> (Sig.Ret None, ctx)
    | [], ("debug", [ "data" ]) ->
        Format.eprintf "debug: %a\n" Value.pp (Ctx.find_var_loc "data" ctx);
        (Sig.Ret None, ctx)
    | _ ->
        let path, fvar = ctx.id in
        Format.eprintf "Unknown builtin extern method %a.%a@." Path.pp path
          FId.pp fvar;
        assert false
end
