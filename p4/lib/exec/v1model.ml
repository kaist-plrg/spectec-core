open Driver
module Value = Runtime_static.Value
module Envs = Runtime_dynamic.Envs
module CEnv = Envs.CEnv
module Sto = Envs.Sto

module Make (Interp : INTERP) : ARCH = struct
  let init_instantiate_packet_in (ctx_gt : Instance.Ctx.gt) (ctx : Ctx.t)
      (sto : Sto.t) : Ctx.t * Sto.t =
    let cons, _ = CEnv.find_overloaded ("packet_in", []) ctx_gt.cenv in
    let sto, obj =
      let ctx_inst = { Instance.Ctx.empty with global = ctx_gt } in
      Instance.Instantiate.do_instantiate Instance.Ctx.Global ctx_inst sto cons
        [] [] []
    in
    let oid = [ "packet_in" ] in
    let value = Value.RefV oid in
    let ctx = Ctx.add_value Ctx.Global "packet_in" value ctx in
    let sto = Sto.add oid obj sto in
    (ctx, sto)

  let init_instantiate_packet_out (ctx_gt : Instance.Ctx.gt) (ctx : Ctx.t)
      (sto : Sto.t) : Ctx.t * Sto.t =
    let cons, _ = CEnv.find_overloaded ("packet_out", []) ctx_gt.cenv in
    let sto, obj =
      let ctx_inst = { Instance.Ctx.empty with global = ctx_gt } in
      Instance.Instantiate.do_instantiate Instance.Ctx.Global ctx_inst sto cons
        [] [] []
    in
    let oid = [ "packet_out" ] in
    let value = Value.RefV oid in
    let ctx = Ctx.add_value Ctx.Global "packet_out" value ctx in
    let sto = Sto.add oid obj sto in
    (ctx, sto)

  let init (ctx_gt : Instance.Ctx.gt) (ctx : Ctx.t) (sto : Sto.t) :
      Ctx.t * Sto.t =
    let ctx, sto = init_instantiate_packet_in ctx_gt ctx sto in
    let ctx, sto = init_instantiate_packet_out ctx_gt ctx sto in
    (ctx, sto)

  let interp_extern (_ctx : Ctx.t) : Ctx.t * Sig.t = failwith "TODO"

  (* let drive_stf (ctx : Ctx.t) (stmt_stf : Stf.Ast.stmt) = *)
  (*   match stmt_stf with *)
  (*   | Stf.Ast.Packet (port, packet) -> *)
  (*       (1* TODO : Should we need to check range of port value? *1) *)
  (*       let nine = Bigint.of_int 9 in *)
  (*       let port = int_of_string port |> Bigint.of_int in *)
  (*       let port = Value.FBitV (nine, port) in *)
  (*       let update_port field = *)
  (*         match field with *)
  (*         | "ingress_port", _ -> ("ingress_port", port) *)
  (*         | id, mem -> (id, mem) *)
  (*       in *)
  (*       let std_meta = Ctx.find_var "standard_metadata" ctx in *)
  (*       let std_meta = *)
  (*         match std_meta with *)
  (*         | StructV fields -> Value.StructV (List.map update_port fields) *)
  (*         | _ -> failwith "standard_metadata changed to other type" *)
  (*       in *)
  (*       let ctx = Ctx.update_var "standard_metadata" std_meta ctx in *)
  (*       (1* Update packet_in and out *1) *)
  (*       let pkt_in = PacketIn (Core.PacketIn.init packet) in *)
  (*       externs := Externs.add "packet_in" pkt_in !externs; *)
  (*       let pkt_out = PacketOut Core.PacketOut.init in *)
  (*       externs := Externs.add "packet_out" pkt_out !externs; *)
  (*       drive_pipe ctx *)
  (*   | Stf.Ast.Expect (port, Some packet) -> *)
  (*       (1* Check packet_out *1) *)
  (*       let port = int_of_string port in *)
  (*       let std_meta = Ctx.find_var "standard_metadata" ctx in *)
  (*       let port' = *)
  (*         Value.access_field "egress_spec" std_meta *)
  (*         |> Value.get_num |> Bigint.to_int |> Option.get *)
  (*       in *)
  (*       let pkt_out = Externs.find "packet_out" !externs in *)
  (*       let expected = String.uppercase_ascii packet in *)
  (*       let actual = Format.asprintf "%a" pp_extern pkt_out in *)
  (*       let result = *)
  (*         if port = port' && Stf.Compare.equals actual expected then "PASS" *)
  (*         else "FAIL" *)
  (*       in *)
  (*       Format.printf "[%s] Expected %s, Port : %i / Actual %s, Port : %i\n" *)
  (*         result expected port actual port'; *)
  (*       ctx *)
  (*   | _ -> ctx *)

  let drive (ctx_gt : Instance.Ctx.gt) (sto : Sto.t)
      (_stmts_stf : Stf.Ast.stmt list) : unit =
    let ctx =
      { Ctx.empty with global = { venv = ctx_gt.venv; fenv = ctx_gt.fenv } }
    in
    let _ctx, sto = init ctx_gt ctx sto in
    Format.printf "%a\n" (Sto.pp ~level:0) sto;
    Interp.init sto;
    (* let _ctx = List.fold_left (fun ctx stf -> drive_stf ctx stf) ctx stf in *)
    ()
end
