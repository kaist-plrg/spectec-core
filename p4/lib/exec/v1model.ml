module F = Format
open Domain.Dom
open Driver
module Ctk = Runtime_static.Ctk
module Value = Runtime_static.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Envs_static = Runtime_static.Envs
module Numerics = Runtime_static.Numerics
module Func = Runtime_dynamic.Func
module Obj = Runtime_dynamic.Object
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto
open Util.Source
open Util.Error

let error_no_info = error_interp_no_info

(* (TODO) Inserts VoidT, shouldn't matter in dynamics but not a good practice either *)
let no_info_expr = (no_info, Il.Ast.{ typ = Types.VoidT; ctk = Ctk.DYN })

let make_expr_base (path : OId.t) =
  let base, members =
    match path with base :: members -> (base, members) | _ -> assert false
  in
  let var_base = Lang.Ast.Current (base $ no_info) $ no_info in
  let expr_base = Il.Ast.VarE { var = var_base } $$ no_info_expr in
  List.fold_left
    (fun expr_base member ->
      let member = member $ no_info in
      Il.Ast.ExprAccE { expr_base; member } $$ no_info_expr)
    expr_base members

let make_arg (arg : Id.t) =
  let var_arg = Lang.Ast.Current (arg $ no_info) $ no_info in
  let expr_arg = Il.Ast.VarE { var = var_arg } $$ no_info_expr in
  Lang.Ast.ExprA expr_arg $ no_info

let make_call (path : OId.t) (func : Id.t) (args : Id.t list) =
  let expr_base = make_expr_base path in
  let args = List.map make_arg args in
  (expr_base, func $ no_info, args)

module Make (Interp : INTERP) : ARCH = struct
  (* Extern objects *)

  module Externs = Map.Make (Id)

  type extern = PacketIn of Core.PacketIn.t | PacketOut of Core.PacketOut.t

  let externs = ref Externs.empty

  let get_pkt_in () =
    match Externs.find "packet_in" !externs with
    | PacketIn pkt_in -> pkt_in
    | _ -> assert false

  let get_pkt_out () =
    match Externs.find "packet_out" !externs with
    | PacketOut pkt_out -> pkt_out
    | _ -> assert false

  (* Initializer:
      instantiate packet_in/out and
      construct global hdr, meta, and standard_metadata values *)

  let init_instantiate_packet_in (ctx : Ctx.t) (sto : Sto.t) : Ctx.t * Sto.t =
    let _, cons, _ = CEnv.find_func ("packet_in", []) ctx.global.cenv in
    let sto, obj =
      let ctx_inst =
        let cenv = ctx.global.cenv in
        let fenv = ctx.global.fenv in
        let venv = ctx.global.venv in
        { Instance.Ctx.empty with global = { cenv; fenv; venv } }
      in
      Instance.Instantiate.do_instantiate Instance.Ctx.Global ctx_inst sto cons
        [] [] []
    in
    let oid = [ "packet_in" ] in
    let value = Value.RefV oid in
    let ctx = Ctx.add_value Ctx.Global "packet_in" value ctx in
    let sto = Sto.add oid obj sto in
    (ctx, sto)

  let init_instantiate_packet_out (ctx : Ctx.t) (sto : Sto.t) : Ctx.t * Sto.t =
    let _, cons, _ = CEnv.find_func ("packet_out", []) ctx.global.cenv in
    let sto, obj =
      let ctx_inst =
        let cenv = ctx.global.cenv in
        let fenv = ctx.global.fenv in
        let venv = ctx.global.venv in
        { Instance.Ctx.empty with global = { cenv; fenv; venv } }
      in
      Instance.Instantiate.do_instantiate Instance.Ctx.Global ctx_inst sto cons
        [] [] []
    in
    let oid = [ "packet_out" ] in
    let value = Value.RefV oid in
    let ctx = Ctx.add_value Ctx.Global "packet_out" value ctx in
    let sto = Sto.add oid obj sto in
    (ctx, sto)

  let init_var (ctx : Ctx.t) (id : Id.t) (typ : Type.t) : Ctx.t =
    let value = Numerics.eval_default typ in
    let ctx = Ctx.add_value Ctx.Global id value ctx in
    ctx

  let init_vars (ctx : Ctx.t) (sto : Sto.t) : Ctx.t =
    (* (TODO) A better way to get the necessary types? *)
    let typ_hdr, typ_meta, typ_std_meta =
      let obj_main_p = Sto.find [ "main"; "p" ] sto in
      let func_main_p_apply =
        match obj_main_p with
        | Obj.ParserO (_, fenv) -> FEnv.find_func_by_name "apply" fenv
        | _ -> assert false
      in
      let params = Func.get_params func_main_p_apply in
      match params with
      | _
        :: { it = _, _, typ_hdr, _, _; _ }
        :: { it = _, _, typ_meta, _, _; _ }
        :: { it = _, _, typ_std_meta, _, _; _ }
        :: _ ->
          (typ_hdr.it, typ_meta.it, typ_std_meta.it)
      | _ -> assert false
    in
    let ctx = init_var ctx "hdr" typ_hdr in
    let ctx = init_var ctx "meta" typ_meta in
    let ctx = init_var ctx "standard_metadata" typ_std_meta in
    ctx

  let init (ctx : Ctx.t) (sto : Sto.t) : Ctx.t * Sto.t =
    let ctx, sto = init_instantiate_packet_in ctx sto in
    let ctx, sto = init_instantiate_packet_out ctx sto in
    let ctx = init_vars ctx sto in
    (ctx, sto)

  (* Extern interpreter *)

  let eval_extern_func_call (_ctx : Ctx.t) (fid : FId.t) : Ctx.t * Sig.t =
    match fid with
    | _ ->
        Format.asprintf "(eval_extern) unknown extern: %a" FId.pp fid
        |> error_no_info

  let eval_extern_method_call (ctx : Ctx.t) (oid : OId.t) (fid : FId.t) :
      Ctx.t * Sig.t =
    match (oid, fid) with
    | [ "packet_in" ], ("extract", [ ("hdr", false) ]) ->
        let packet_in = get_pkt_in () in
        let ctx, pkt_in = Core.PacketIn.extract ctx packet_in in
        externs := Externs.add "packet_in" (PacketIn pkt_in) !externs;
        (ctx, Sig.Ret None)
    | ( [ "packet_in" ],
        ( "extract",
          [ ("variableSizeHeader", false); ("variableFieldSizeInBits", false) ]
        ) ) ->
        let packet_in = get_pkt_in () in
        let ctx, pkt_in = Core.PacketIn.extract_varsize ctx packet_in in
        externs := Externs.add "packet_in" (PacketIn pkt_in) !externs;
        (ctx, Sig.Ret None)
    | [ "packet_in" ], ("lookahead", []) ->
        let packet_in = get_pkt_in () in
        let hdr = Core.PacketIn.lookahead ctx packet_in in
        (ctx, Sig.Ret (Some hdr))
    | [ "packet_in" ], ("advance", [ ("sizeInBits", false) ]) ->
        let packet_in = get_pkt_in () in
        let pkt_in = Core.PacketIn.advance ctx packet_in in
        externs := Externs.add "packet_in" (PacketIn pkt_in) !externs;
        (ctx, Sig.Ret None)
    | [ "packet_in" ], ("length", []) ->
        let packet_in = get_pkt_in () in
        let len = Core.PacketIn.length packet_in in
        (ctx, Sig.Ret (Some len))
    | [ "packet_out" ], ("emit", [ ("hdr", false) ]) ->
        let packet_out = get_pkt_out () in
        let ctx, pkt_out = Core.PacketOut.emit ctx packet_out in
        externs := Externs.add "packet_out" (PacketOut pkt_out) !externs;
        (ctx, Sig.Ret None)
    | _ ->
        Format.asprintf "(eval_extern) unknown extern: %a.%a" OId.pp oid FId.pp
          fid
        |> error_no_info

  (* Pipeline driver *)

  type port = int
  type packet = string
  type result = port * packet

  let drive_p (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "p" ] in
      let func = "apply" in
      let args = [ "packet_in"; "hdr"; "meta"; "standard_metadata" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_vr (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "vr" ] in
      let func = "apply" in
      let args = [ "hdr"; "meta" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_ig (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "ig" ] in
      let func = "apply" in
      let args = [ "hdr"; "meta"; "standard_metadata" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_eg (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "eg" ] in
      let func = "apply" in
      let args = [ "hdr"; "meta"; "standard_metadata" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_ck (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "ck" ] in
      let func = "apply" in
      let args = [ "hdr"; "meta" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_dep (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "dep" ] in
      let func = "apply" in
      let args = [ "packet_out"; "hdr" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_pipe (ctx : Ctx.t) (port_in : port) (packet_in : packet) : result =
    (* Update ingress port *)
    let value_std_meta =
      let value_port = Value.FBitV (Bigint.of_int 9, Bigint.of_int port_in) in
      let value_std_meta = Ctx.find_value Ctx.Global "standard_metadata" ctx in
      Value.update_struct_field value_std_meta "ingress_port" value_port
    in
    let ctx =
      Ctx.update_value Ctx.Global "standard_metadata" value_std_meta ctx
    in
    (* Create input packet *)
    let pkt_in = PacketIn (Core.PacketIn.init packet_in) in
    externs := Externs.add "packet_in" pkt_in !externs;
    let pkt_out = PacketOut (Core.PacketOut.init ()) in
    externs := Externs.add "packet_out" pkt_out !externs;
    (* Execute packet processing pipeline *)
    let ctx = ctx |> drive_p in
    let pkt_payload =
      get_pkt_in () |> F.asprintf "%a" Core.PacketIn.pp_remaining
    in
    let ctx =
      ctx |> drive_vr |> drive_ig |> drive_eg |> drive_ck |> drive_dep
    in
    (* Check egress port *)
    let port_out =
      let value_std_meta = Ctx.find_value Ctx.Global "standard_metadata" ctx in
      let _, fields = Value.get_struct value_std_meta in
      List.assoc "egress_spec" fields |> Value.get_num |> Bigint.to_int_exn
    in
    (* Check output packet *)
    let packet_out = get_pkt_out () |> F.asprintf "%a" Core.PacketOut.pp in
    let packet_out = packet_out ^ pkt_payload in
    (port_out, packet_out)

  let drive_stf_stmt (ctx : Ctx.t) (pass : bool) (queue_packet : result list)
      (queue_expect : result list) (stmt_stf : Stf.Ast.stmt) =
    let compare_packet packet_out packet_expect : bool =
      let to_list s = List.init (String.length s) (String.get s) in
      let packet_out = to_list packet_out in
      let packet_expect = to_list packet_expect in
      List.length packet_out = List.length packet_expect
      && List.fold_left2
           (fun same o e -> same && (e = '*' || o = e))
           true packet_out packet_expect
    in
    let compare (port_out, packet_out) (port_expect, packet_expect) : bool =
      let pass =
        port_out = port_expect && compare_packet packet_out packet_expect
      in
      if pass then
        F.printf "[PASS] Expected: %d %s / Got: %d %s\n" port_expect
          packet_expect port_out packet_out
      else
        F.printf "[FAIL] Expected: %d %s / Got: %d %s\n" port_expect
          packet_expect port_out packet_out;
      pass
    in
    match stmt_stf with
    | Stf.Ast.Packet (port_in, packet_in) -> (
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let port_out, packet_out = drive_pipe ctx port_in packet_in in
        match queue_expect with
        | [] ->
            let queue_packet = queue_packet @ [ (port_out, packet_out) ] in
            (ctx, pass, queue_packet, queue_expect)
        | (port_expect, packet_expect) :: queue_expect ->
            let pass =
              compare (port_out, packet_out) (port_expect, packet_expect)
              && pass
            in
            (ctx, pass, queue_packet, queue_expect))
    | Stf.Ast.Expect (port_expect, Some packet_expect) -> (
        let port_expect = int_of_string port_expect in
        let packet_expect = String.uppercase_ascii packet_expect in
        match queue_packet with
        | [] ->
            ( ctx,
              pass,
              queue_packet,
              queue_expect @ [ (port_expect, packet_expect) ] )
        | (port_out, packet_out) :: queue_packet ->
            let pass =
              compare (port_out, packet_out) (port_expect, packet_expect)
              && pass
            in
            (ctx, pass, queue_packet, queue_expect))
    | _ ->
        Format.asprintf "(drive_stf_stmt) unknown stf stmt: %a"
          Stf.Print.print_stmt stmt_stf
        |> error_no_info

  let drive_stf_stmts (ctx : Ctx.t) (stmts_stf : Stf.Ast.stmt list) : bool =
    let _, pass, _, _ =
      List.fold_left
        (fun (ctx, pass, queue_packet, queue_expect) stmt_stf ->
          drive_stf_stmt ctx pass queue_packet queue_expect stmt_stf)
        (ctx, true, [], []) stmts_stf
    in
    pass

  let drive (cenv : CEnv.t) (fenv : FEnv.t) (venv : VEnv.t) (sto : Sto.t)
      (_stmts_stf : Stf.Ast.stmt list) : bool =
    let ctx = { Ctx.empty with global = { cenv; fenv; venv } } in
    let ctx, sto = init ctx sto in
    Interp.init sto;
    drive_stf_stmts ctx _stmts_stf
end
