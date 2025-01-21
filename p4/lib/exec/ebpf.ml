module F = Format
open Domain.Dom
module Ctk = Runtime_static.Ctk
module Value = Runtime_static.Vdomain.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module TypeDef = Types.TypeDef
module Envs_static = Runtime_static.Envs
module Numerics = Runtime_static.Numerics
module Table = Runtime_dynamic.Table
module Func = Runtime_dynamic.Func
module Obj = Runtime_dynamic.Object
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module Theta = Envs_dynamic.Theta
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto
open Driver
open Util.Error

let error = error_driver

(* eBPF extern objects *)

module CounterArray = struct
  type t = int list

  let pp fmt _carr = F.fprintf fmt "CounterArray"

  (* A counter array is a dense or sparse array of unsigned 32-bit values, visible to the
     control-plane as an EBPF map (array or hash).
     Each counter is addressed by a 32-bit index.
     Counters can only be incremented by the data-plane, but they can be read or
     reset by the control-plane.

     Allocate an array of counters.
     @param max_index  Maximum counter index supported.
     @param sparse     The counter array is supposed to be sparse.

     CounterArray(bit<32> max_index, bool sparse); *)
  let init (max_index : Value.t) (sparse : Value.t) : t =
    let max_index = max_index |> Value.get_num |> Bigint.to_int_exn in
    let _sparse = sparse |> Value.get_bool in
    List.init max_index (fun _ -> 0)

  (* Increment counter with specified index.

     void increment(in bit<32> index); *)
  let increment (ctx : Ctx.t) carr : Ctx.t * Sig.t * t =
    let index_target =
      Ctx.find_value Ctx.Local "index" ctx |> Value.get_num |> Bigint.to_int_exn
    in
    let carr =
      List.mapi
        (fun idx count -> if idx = index_target then count + 1 else count)
        carr
    in
    (ctx, Sig.Ret None, carr)

  (* Add value to counter with specified index.

     void add(in bit<32> index, in bit<32> value) *)
  let add (ctx : Ctx.t) carr : Ctx.t * Sig.t * t =
    let index_target =
      Ctx.find_value Ctx.Local "index" ctx |> Value.get_num |> Bigint.to_int_exn
    in
    let value =
      Ctx.find_value Ctx.Local "value" ctx |> Value.get_num |> Bigint.to_int_exn
    in
    let carr =
      List.mapi
        (fun idx count -> if idx = index_target then count + value else count)
        carr
    in
    (ctx, Sig.Ret None, carr)
end

module Make (Interp : INTERP) : ARCH = struct
  (* Extern objects *)

  module Externs = Map.Make (Id)

  type extern = PacketIn of Core.PacketIn.t | CounterArray of CounterArray.t

  let _pp_extern fmt extern =
    match extern with
    | PacketIn pkt_in -> F.fprintf fmt "PacketIn %a" Core.PacketIn.pp pkt_in
    | CounterArray carr -> F.fprintf fmt "CounterArray %a" CounterArray.pp carr

  let externs = ref Externs.empty

  (* Initializer:
      instantiate packet_in,
      construct global hdr, meta, and standard_metadata values *)

  let init_instantiate_packet_in (ctx : Ctx.t) (sto : Sto.t) : Ctx.t * Sto.t =
    let _, cons, _ = CEnv.find_func ("packet_in", []) ctx.global.cenv in
    let sto, obj =
      let ctx_inst =
        let cenv = ctx.global.cenv in
        let tdenv = ctx.global.tdenv in
        let fenv = ctx.global.fenv in
        let venv = ctx.global.venv in
        { Instance.Ctx.empty with global = { cenv; tdenv; fenv; venv } }
      in
      Instance.Instantiate.do_instantiate Instance.Ctx.Global ctx_inst sto cons
        [] [] []
    in
    let oid = [ "packet_in" ] in
    let value = Value.RefV oid in
    let ctx = Ctx.add_value Ctx.Global "packet_in" value ctx in
    let sto = Sto.add oid obj sto in
    (ctx, sto)

  let init_counter_arrays (sto : Sto.t) : unit =
    Sto.iter
      (fun oid obj ->
        match obj with
        | Obj.ExternO ("CounterArray", _, venv, _) ->
            let max_index = VEnv.find "max_index" venv in
            let sparse = VEnv.find "sparse" venv in
            let carr = CounterArray.init max_index sparse in
            let id = String.concat "." oid in
            externs := Externs.add id (CounterArray carr) !externs
        | _ -> ())
      sto

  let init_var (ctx : Ctx.t) (id : Id.t) (typ : Type.t) : Ctx.t =
    let value = Numerics.eval_default typ in
    let ctx = Ctx.add_value Ctx.Global id value ctx in
    ctx

  let init_vars (ctx : Ctx.t) (sto : Sto.t) : Ctx.t =
    let typ_hdr =
      let obj_main = Sto.find [ "main" ] sto in
      match obj_main with
      | Obj.PackageO (theta, _) -> Theta.find "H" theta
      | _ -> assert false
    in
    let ctx = init_var ctx "hdr" typ_hdr in
    init_var ctx "accept" Types.BoolT

  let init (ctx : Ctx.t) (sto : Sto.t) : Ctx.t * Sto.t =
    let ctx, sto = init_instantiate_packet_in ctx sto in
    init_counter_arrays sto;
    let ctx = init_vars ctx sto in
    (ctx, sto)

  (* Extern interpreter *)

  let eval_extern_func_call (ctx : Ctx.t) (fid : FId.t) : Ctx.t * Sig.t =
    let fname, args = fid in
    match (fname, args) with
    (* core.p4 *)
    | "verify", [ ("check", false); ("toSignal", false) ] -> Core.verify ctx
    | _ -> F.asprintf "(TODO: eval_extern_func_call) %a" FId.pp fid |> error

  let eval_extern_method_call (ctx : Ctx.t) (oid : OId.t) (fid : FId.t) :
      Ctx.t * Sig.t =
    let id = String.concat "." oid in
    let extern = Externs.find id !externs in
    match (extern, fid) with
    | PacketIn pkt_in, ("extract", [ ("hdr", false) ]) ->
        let ctx, sign, pkt_in = Core.PacketIn.extract ctx pkt_in in
        externs := Externs.add id (PacketIn pkt_in) !externs;
        (ctx, sign)
    | ( PacketIn pkt_in,
        ( "extract",
          [ ("variableSizeHeader", false); ("variableFieldSizeInBits", false) ]
        ) ) ->
        let ctx, sign, pkt_in = Core.PacketIn.extract_varsize ctx pkt_in in
        externs := Externs.add id (PacketIn pkt_in) !externs;
        (ctx, sign)
    | PacketIn pkt_in, ("lookahead", []) ->
        let sign = Core.PacketIn.lookahead ctx pkt_in in
        (ctx, sign)
    | PacketIn pkt_in, ("advance", [ ("sizeInBits", false) ]) ->
        let pkt_in = Core.PacketIn.advance ctx pkt_in in
        externs := Externs.add id (PacketIn pkt_in) !externs;
        (ctx, Sig.Ret None)
    | PacketIn pkt_in, ("length", []) ->
        let len = Core.PacketIn.length pkt_in in
        (ctx, Sig.Ret (Some len))
    | CounterArray carr, ("increment", [ ("index", false) ]) ->
        let ctx, sign, carr = CounterArray.increment ctx carr in
        externs := Externs.add id (CounterArray carr) !externs;
        (ctx, sign)
    | CounterArray carr, ("add", [ ("index", false); ("value", false) ]) ->
        let ctx, sign, carr = CounterArray.add ctx carr in
        externs := Externs.add id (CounterArray carr) !externs;
        (ctx, sign)
    | _ ->
        F.asprintf "(TODO: eval_extern_method_call) %a.%a" OId.pp oid FId.pp fid
        |> error

  (* Pipeline driver *)

  let drive_prs (ctx : Ctx.t) : Ctx.t * Sig.t =
    let expr_base, func, args =
      let oid = [ "main"; "prs" ] in
      let func = "apply" in
      let args = [ "packet_in"; "hdr" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args

  let drive_filt (ctx : Ctx.t) : Ctx.t =
    let expr_base, func, args =
      let oid = [ "main"; "filt" ] in
      let func = "apply" in
      let args = [ "hdr"; "accept" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args |> fst

  let drive_pipe (ctx : Ctx.t) (port_in : port) (packet_in : packet) :
      result option =
    let ( let* ) = Option.bind in
    (* Create input packet *)
    let pkt_in = PacketIn (Core.PacketIn.init packet_in) in
    externs := Externs.add "packet_in" pkt_in !externs;
    (* Execute packet processing pipeline *)
    (* Execute the parser block *)
    let ctx, sign = ctx |> drive_prs in
    let* ctx =
      match sign with Trans (`Reject _value) -> None | _ -> Some ctx
    in
    (* Execute the filter block *)
    let ctx = drive_filt ctx in
    let accept = Ctx.find_value Ctx.Global "accept" ctx |> Value.get_bool in
    (* Extract output packet *)
    if accept then Some (port_in, packet_in) else None
end
