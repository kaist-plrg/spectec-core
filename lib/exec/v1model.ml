open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Cclos
open Runtime.Context

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
  |] [@@ocamlformat "disable"]

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
  |] [@@ocamlformat "disable"]

let pkt () =
  let bits = Array.append (ethernet_header_bits ()) (ipv4_header_bits ()) in
  Core.Packet.init bits

let init_instantiate_packet_in (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
  let cclos_packet_in = CCEnv.find "packet_in" ccenv |> Option.get in
  let ictx = ICtx.init ctx.env_glob ctx.env_obj in
  let path = [ "packet" ] in
  let sto =
    Instance.Instantiate.instantiate_from_cclos ccenv sto ictx path
      cclos_packet_in [] []
  in
  (sto, pkt ())

let init (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
  (* Add "packet" to the store and object environment *)
  let sto, _pkt = init_instantiate_packet_in ccenv sto ctx in
  let ctx =
    let typ = Type.RefT in
    let value = Value.RefV [ "packet" ] in
    Ctx.add_var_obj "packet" typ value ctx
  in
  (* Add "hdr" to the object environment *)
  let ctx =
    let typ = Ctx.find_td "headers" ctx |> Option.get in
    let value = Runtime.Ops.eval_default_value typ in
    Ctx.add_var_obj "hdr" typ value ctx
  in
  (* Add "meta" to the object environment *)
  let ctx =
    let typ = Ctx.find_td "metadata" ctx |> Option.get in
    let value = Runtime.Ops.eval_default_value typ in
    Ctx.add_var_obj "meta" typ value ctx
  in
  (* Add "standard_metadata" to the object environment *)
  let ctx =
    let typ = Ctx.find_td "standard_metadata_t" ctx |> Option.get in
    let value = Runtime.Ops.eval_default_value typ in
    Ctx.add_var_obj "standard_metadata" typ value ctx
  in
  (sto, ctx)

let make_args (args : Var.t list) =
  List.map (fun arg -> ExprA (VarE (Bare arg))) args

let drive_parser_impl (ctx : Ctx.t) =
  let value = Value.RefV [ "main"; "p" ] in
  let targs = [] in
  let args = make_args [ "packet"; "hdr"; "meta"; "standard_metadata" ] in
  Interp.interp_method_call ctx value "apply" targs args |> snd

let drive_ingress (ctx : Ctx.t) =
  let value = Value.RefV [ "main"; "ig" ] in
  let targs = [] in
  let args = make_args [ "hdr"; "meta"; "standard_metadata" ] in
  Interp.interp_method_call ctx value "apply" targs args |> snd

let drive_egress (ctx : Ctx.t) =
  let value = Value.RefV [ "main"; "eg" ] in
  let targs = [] in
  let args = make_args [ "hdr"; "meta"; "standard_metadata" ] in
  Interp.interp_method_call ctx value "apply" targs args |> snd

let drive (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
  let sto, ctx = init ccenv sto ctx in
  Interp.init sto;
  Format.printf "Initial v1model driver context\n%a@." Ctx.pp ctx;
  let ctx = drive_parser_impl ctx in
  Format.printf "\nAfter parser_impl call\n%a@." Ctx.pp ctx;
  let ctx = drive_ingress ctx in
  Format.printf "\nAfter ingress call\n%a@." Ctx.pp ctx;
  let ctx = drive_egress ctx in
  Format.printf "\nAfter egress call\n%a@." Ctx.pp ctx;
  ()
