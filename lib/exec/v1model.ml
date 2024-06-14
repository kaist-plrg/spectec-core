open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Object
open Runtime.Cclos
open Runtime.Context
open Runtime.Signal
open Driver

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

let sandbox_header_bits () : bool array =
  [|
    (* 01 *) false; false; false; false; false; false; false; true;
    (* 02 *) false; false; false; false; false; false; true; false;
    (* 00 *) false; false; false; false; false; false; false; false;
    (* 03 *) false; false; false; false; false; false; true; true;
    (* 00 *) false; false; false; false; false; false; false; false;
    (* 04 *) false; false; false; false; false; true; false; false;
    (* 01 *) false; false; false; false; false; false; false; true;
    (* AA *) true; false; true; false; true; false; true; false;
    (* BB *) true; false; true; true; true; false; true; true;
    (* 4F *) false; true; false; false; true; true; true; true;
    (* 48 *) false; true; false; false; true; false; false; false;
    (* 20 *) false; false; true; false; false; false; false; false;
    (* 4d *) false; true; false; false; true; true; false; false;
    (* 59 *) false; true; true; false; true; false; false; true;
    (* 59 *) false; true; true; false; true; false; false; true;
    (* 20 *) false; false; true; false; false; false; false; false;
    (* 49 *) false; true; false; false; true; false; false; true;
    (* 20 *) false; false; true; false; false; false; false; false;
    (* 4b *) false; true; false; false; true; false; true; true;
    (* 4e *) false; true; false; false; true; true; true; false;
    (* 4f *) false; true; false; false; true; true; true; true;
    (* 57 *) false; true; false; true; false; true; true; true;
    (* 20 *) false; false; true; false; false; false; false; false;
    (* 57 *) false; true; false; true; false; true; true; true;
    (* 48 *) false; true; false; false; true; false; false; false;
    (* 59 *) false; true; false; true; true; false; false; true;
  |] [@@ocamlformat "disable"]

let pkt_in () =
  (*let bits = Array.append (ethernet_header_bits ()) (ipv4_header_bits ()) in*)
  let bits = sandbox_header_bits () in
  Core.PacketIn.init bits

let pkt_out () = Core.PacketOut.init

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
    externs := EM.add "packet_in" (PacketIn (pkt_in ())) !externs;
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
    externs := EM.add "packet_out" (PacketOut (pkt_out ())) !externs;
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

  let drive (ccenv : CCEnv.t) (sto : Sto.t) (ctx : Ctx.t) =
    let sto, ctx = init ccenv sto ctx in
    EM.find "packet_in" !externs
    |> Format.printf "\nInput packet %a\n" pp_extern;
    Interp.init sto;
    let ctx =
      ctx |> drive_p |> drive_vr |> drive_ig |> drive_eg |> drive_ck
      |> drive_dep
    in
    Format.printf "\nFinal v1model driver context\n%a@." Ctx.pp_var ctx;
    EM.find "packet_out" !externs
    |> Format.printf "\nOutput packet %a\n" pp_extern;
    ()

  (* (TODO) how to figure out on which extern object the mthd is called?
     currently it assumes that "extract" is called on PacketIn, ... *)
  let interp_extern (sign : Sig.t) (ctx : Ctx.t) (mthd : string) =
    match sign with
    | Ret _ | Exit -> (sign, ctx)
    | Cont -> (
        match mthd with
        | "extract" -> (
            match EM.find "packet_in" !externs with
            | PacketIn pkt_in ->
                let ctx, pkt_in = Core.PacketIn.extract ctx pkt_in in
                externs := EM.add "packet_in" (PacketIn pkt_in) !externs;
                (sign, ctx)
            | _ -> assert false)
        | "emit" -> (
            match EM.find "packet_out" !externs with
            | PacketOut pkt_out ->
                let ctx, pkt_out = Core.PacketOut.emit ctx pkt_out in
                externs := EM.add "packet_out" (PacketOut pkt_out) !externs;
                (sign, ctx)
            | _ -> assert false)
        | "verify_checksum" -> Hash.verify_checksum ctx |> fun ctx -> (sign, ctx)
        | "update_checksum" -> Hash.update_checksum ctx |> fun ctx -> (sign, ctx)
        | _ ->
            Format.eprintf "Unknown builtin extern method %s@." mthd;
            assert false)
end
