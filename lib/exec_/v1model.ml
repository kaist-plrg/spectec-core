open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Cclos
open Runtime_.Context

let path = []

let ethernet_header_bits () : bool array =
  [|
    (* Destination MAC Address (48 bits): 01:23:45:67:89:AB *)
    false; false; false; false; false; false; false; true;
    false; false; false; false; true; false; true; true;
    false; false; true; false; false; false; false; true;
    false; true; true; false; true; true; true; false;
    true; false; false; false; true; false; false; true;
    true; false; true; true; false; true; false; true;
    (* Source MAC Address (48 bits): CD:EF:01:23:45:67 *)
    true; true; false; false; true; true; false; true;
    true; true; true; true; false; false; false; true;
    false; false; false; false; false; true; false; true;
    false; false; false; false; false; true; true; false;
    false; true; true; false; false; false; false; true;
    false; true; true; false; true; true; true; false;
    (* Ethertype (16 bits): 0800 (IPv4) *)
    false; false; false; false; true; false; false; false;
    false; false; false; false; false; false; false; false;
  |]

let ipv4_header_bits () : bool array =
  [|
    (* Version (4 bits): 0100 *)
    false; true; false; false;
    (* IHL (4 bits): 0101 *)
    false; true; false; true;
    (* Type of Service (8 bits): 00000000 *)
    false; false; false; false; false; false; false; false;
    (* Total Length (16 bits): 0000011000101000 (1576 in decimal) *)
    false; false; false; false; false; true; true; false;
    false; false; true; false; true; false; false; false;
    (* Identification (16 bits): 1100100100110100 *)
    true; true; false; false; true; false; false; true;
    false; false; true; true; false; true; false; false;
    (* Flags (3 bits): 010 *)
    false; true; false;
    (* Fragment Offset (13 bits): 0000000000000 *)
    false; false; false; false; false; false; false; false;
    false; false; false; false; false;
    (* Time to Live (8 bits): 01000101 (69 in decimal) *)
    false; true; false; false; false; false; true; false;
    (* Protocol (8 bits): 00000110 (6 in decimal, TCP) *)
    false; false; false; false; false; true; true; false;
    (* Header Checksum (16 bits): 1000111110101111 *)
    true; false; false; false; true; true; true; true;
    true; false; true; false; true; true; true; true;
    (* Source Address (32 bits): 11000000101010000000000100000001 (192.168.1.1) *)
    true; true; false; false; false; false; false; false;
    true; false; true; false; false; false; false; false;
    false; false; false; false; false; false; false; true;
    false; false; false; false; false; false; false; true;
    (* Destination Address (32 bits): 11000000101010000000000100000010 (192.168.1.2) *)
    true; true; false; false; false; false; false; false;
    true; false; true; false; false; false; false; false;
    false; false; false; false; false; false; false; true;
    false; false; false; false; false; false; false; true;
  |]

let pkt () =
  let bits = Array.append (ethernet_header_bits ()) (ipv4_header_bits ())  in
  Core.Packet.init bits

let init_instantiate_packet_in (ccenv : CCEnv.t) (gctx : GCtx.t) =
  let cclos_packet_in = CCEnv.find "packet_in" ccenv |> Option.get in
  let ictx = ICtx.init gctx.glob (TDEnv.empty, Env.empty, FEnv.empty) in
  let sto = gctx.sto in
  let path = [ "packet" ] in
  let sto =
    Instance_.Instantiate.instantiate_from_cclos ccenv sto ictx path
      cclos_packet_in [] []
  in
  ({ gctx with sto }, pkt ())

let init (ccenv : CCEnv.t) (gctx : GCtx.t) =
  (* Initialize the context *)
  let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
  let env_loc = (TDEnv.empty, []) in
  let ctx = Ctx.init gctx.glob env_obj env_loc in
  (* Add "packet" to the store and object environment *)
  let gctx, _pkt = init_instantiate_packet_in ccenv gctx in
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
  (gctx, ctx)

let make_args (args : Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

let drive_parser_impl (gctx : GCtx.t) (ctx : Ctx.t) =
  let path = [ "main"; "p" ] in
  let obj_parser_impl = GCtx.find_obj path gctx |> Option.get in
  let targs = [] in
  let args = make_args [ "packet"; "hdr"; "meta"; "standard_metadata" ] in
  Interp.interp_method_call ctx obj_parser_impl "apply" targs args

let drive (ccenv : CCEnv.t) (gctx : GCtx.t) =
  let gctx, ctx = init ccenv gctx in
  Interp.init gctx;
  Format.printf "Initial v1model driver context\n%a@." Ctx.pp ctx;
  let ctx = drive_parser_impl gctx ctx in
  Format.printf "\nAfter parser_impl call\n%a@." Ctx.pp ctx;
  ()
