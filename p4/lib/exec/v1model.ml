module F = Format
open Domain.Dom
module Ctk = Runtime_static.Ctk
module Value = Runtime_static.Vdomain.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Envs_static = Runtime_static.Envs
module Numerics = Runtime_static.Numerics
module Table = Runtime_dynamic.Table
module Func = Runtime_dynamic.Func
module Obj = Runtime_dynamic.Object
module Envs_dynamic = Runtime_dynamic.Envs
module TEnv = Envs_dynamic.TEnv
module VEnv = Envs_dynamic.VEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto
open Driver
open Util.Source
open Util.Error

let error_no_info = error_interp_no_info

(* V1Model extern objects *)

module Counter = struct
  type t =
    | Packets of Bigint.t list
    | Bytes of Bigint.t list
    | PacketsAndBytes of (Bigint.t * Bigint.t) list

  let pp fmt _ctr = F.fprintf fmt "Counter"

  let init (size : Value.t) (typ : Value.t) : t =
    let size = size |> Value.get_num |> Bigint.to_int_exn in
    match typ with
    | EnumFieldV ("CounterType", "packets") ->
        Packets (List.init size (fun _ -> Bigint.zero))
    | EnumFieldV ("CounterType", "bytes") ->
        Bytes (List.init size (fun _ -> Bigint.zero))
    | EnumFieldV ("CounterType", "packets_and_bytes") ->
        PacketsAndBytes (List.init size (fun _ -> (Bigint.zero, Bigint.zero)))
    | _ -> assert false

  (* count() causes the counter state with the specified index to be
      read, modified, and written back, atomically relative to the
      processing of other packets, updating the packet count, byte
      count, or both, depending upon the CounterType of the counter
      instance used when it was constructed.

      @param index The index of the counter state in the array to be
                   updated, normally a value in the range [0,
                   size-1].  If index >= size, no counter state will be
                   updated.

     void count(in bit<32> index); *)
  let count (ctx : Ctx.t) (len : Bigint.t) ctr : Ctx.t * Sig.t * t =
    let index_target =
      Ctx.find_value Ctx.Local "index" ctx |> Value.get_num |> Bigint.to_int_exn
    in
    let ctr =
      match ctr with
      | Packets counts ->
          let counts =
            List.mapi
              (fun idx count ->
                if idx = index_target then Bigint.(count + one) else count)
              counts
          in
          Packets counts
      | Bytes counts ->
          let counts =
            List.mapi
              (fun idx count ->
                if idx = index_target then Bigint.(count + len) else count)
              counts
          in
          Bytes counts
      | PacketsAndBytes packets_and_bytes ->
          let packets_and_bytes =
            List.mapi
              (fun idx (count_packets, count_bytes) ->
                if idx = index_target then
                  (Bigint.(count_packets + one), Bigint.(count_bytes + len))
                else (count_packets, count_bytes))
              packets_and_bytes
          in
          PacketsAndBytes packets_and_bytes
    in
    let sign = Sig.Ret None in
    (ctx, sign, ctr)
end

module Register = struct
  type t = Type.t * Value.t list

  let pp fmt _reg = F.fprintf fmt "Register"

  let init (typ : Type.t) (size : Value.t) : t =
    let value_default = Numerics.eval_default typ in
    let size = size |> Value.get_num |> Bigint.to_int_exn in
    let values = List.init size (fun _ -> value_default) in
    (typ, values)

  (* read() reads the state of the register array stored at the
     specified index, and returns it as the value written to the
     result parameter.

     @param index The index of the register array element to be
                  read, normally a value in the range [0, size-1].
     @param result Only types T that are bit<W> are currently
                  supported.  When index is in range, the value of
                  result becomes the value read from the register
                  array element.  When index >= size, the final
                  value of result is not specified, and should be
                  ignored by the caller.

     void read(out T result, in bit<32> index); *)
  let read (ctx : Ctx.t) reg : Ctx.t * Sig.t * t =
    let typ, values = reg in
    let index_target =
      Ctx.find_value Ctx.Local "index" ctx |> Value.get_num |> Bigint.to_int_exn
    in
    let value =
      if index_target < List.length values then List.nth values index_target
      else Numerics.eval_default typ
    in
    let ctx = Ctx.update_value Ctx.Local "result" value ctx in
    let sign = Sig.Ret None in
    let reg = (typ, values) in
    (ctx, sign, reg)

  (* write() writes the state of the register array at the specified
     index, with the value provided by the value parameter.

     If you wish to perform a read() followed later by a write() to
     the same register array element, and you wish the
     read-modify-write sequence to be atomic relative to other
     processed packets, then there may be parallel implementations
     of the v1model architecture for which you must execute them in
     a P4_16 block annotated with an @atomic annotation.  See the
     P4_16 language specification description of the @atomic
     annotation for more details.

     @param index The index of the register array element to be
                  written, normally a value in the range [0,
                  size-1].  If index >= size, no register state will
                  be updated.
     @param value Only types T that are bit<W> are currently
                  supported.  When index is in range, this
                  parameter's value is written into the register
                  array element specified by index.
     void write(in bit<32> index, in T value); *)
  let write (ctx : Ctx.t) reg : Ctx.t * Sig.t * t =
    let typ, values = reg in
    let index_target =
      Ctx.find_value Ctx.Local "index" ctx |> Value.get_num |> Bigint.to_int_exn
    in
    let value_target = Ctx.find_value Ctx.Local "value" ctx in
    let values =
      List.mapi
        (fun idx value -> if idx = index_target then value_target else value)
        values
    in
    let sign = Sig.Ret None in
    let reg = (typ, values) in
    (ctx, sign, reg)
end

(* V1Model Pipeline *)

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

  type extern =
    | PacketIn of Core.PacketIn.t
    | PacketOut of Core.PacketOut.t
    | Counter of Counter.t
    | Register of Register.t

  let pp_extern fmt extern =
    match extern with
    | PacketIn pkt_in -> F.fprintf fmt "PacketIn %a" Core.PacketIn.pp pkt_in
    | PacketOut pkt_out ->
        F.fprintf fmt "PacketOut %a" Core.PacketOut.pp pkt_out
    | Counter ctr -> F.fprintf fmt "Counter %a" Counter.pp ctr
    | Register reg -> F.fprintf fmt "Register %a" Register.pp reg

  let externs = ref Externs.empty

  let get_pkt_in () =
    match Externs.find "packet_in" !externs with
    | PacketIn pkt_in -> pkt_in
    | _ -> assert false

  let get_pkt_out () =
    match Externs.find "packet_out" !externs with
    | PacketOut pkt_out -> pkt_out
    | _ -> assert false

  (* Configurations *)

  let drop_spec = Value.FBitV (Bigint.of_int 9, Bigint.of_int 511)

  (* Initializer:
      instantiate packet_in/out,
      initializer counters if any,
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

  let init_counters (sto : Sto.t) : unit =
    Sto.iter
      (fun oid obj ->
        match obj with
        | Obj.ExternO ("counter", _, venv, _) ->
            let size = VEnv.find "size" venv in
            let typ = VEnv.find "type" venv in
            let ctr = Counter.init size typ in
            let id = String.concat "." oid in
            externs := Externs.add id (Counter ctr) !externs
        | _ -> ())
      sto

  let init_registers (sto : Sto.t) : unit =
    Sto.iter
      (fun oid obj ->
        match obj with
        | Obj.ExternO ("register", tenv, venv, _) ->
            let typ = TEnv.find "T" tenv in
            let size = VEnv.find "size" venv in
            let reg = Register.init typ size in
            let id = String.concat "." oid in
            externs := Externs.add id (Register reg) !externs
        | _ -> ())
      sto

  let init_var (ctx : Ctx.t) (id : Id.t) (typ : Type.t) : Ctx.t =
    let value = Numerics.eval_default typ in
    let ctx = Ctx.add_value Ctx.Global id value ctx in
    ctx

  let init_vars (ctx : Ctx.t) (sto : Sto.t) : Ctx.t =
    (* (TODO) A better way to get the necessary types? *)
    let typ_hdr, typ_meta, typ_std_meta =
      let obj_main_p = Sto.find [ "main"; "p" ] sto in
      let params =
        match obj_main_p with
        | Obj.ParserO (_, params, _, _) -> params
        | _ -> assert false
      in
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
    init_counters sto;
    init_registers sto;
    let ctx = init_vars ctx sto in
    (ctx, sto)

  (* Extern interpreter *)

  (* mark_to_drop(standard_metadata) is a primitive action that modifies
     standard_metadata.egress_spec to an implementation-specific special
     value that in some cases causes the packet to be dropped at the end
     of ingress or egress processing.  It also assigns 0 to
     standard_metadata.mcast_grp.  Either of those metadata fields may
     be changed by executing later P4 code, after calling
     mark_to_drop(), and this can change the resulting behavior of the
     packet to do something other than drop.

     extern void mark_to_drop(inout standard_metadata_t standard_metadata); *)
  let eval_extern_mark_to_drop (ctx : Ctx.t) : Ctx.t * Sig.t =
    let value_std_meta = Ctx.find_value Ctx.Local "standard_metadata" ctx in
    let value_std_meta =
      Value.update_struct_field value_std_meta "egress_spec" drop_spec
    in
    let value_std_meta =
      Value.update_struct_field value_std_meta "mcast_grp"
        (FBitV (Bigint.of_int 16, Bigint.of_int 0))
    in
    let ctx =
      Ctx.update_value Ctx.Local "standard_metadata" value_std_meta ctx
    in
    (ctx, Sig.Ret None)

  (* Calculate a hash function of the value specified by the data
     parameter.  The value written to the out parameter named result
     will always be in the range [base, base+max-1] inclusive, if max >=
     1.  If max=0, the value written to result will always be base.

     Note that the types of all of the parameters may be the same as, or
     different from, each other, and thus their bit widths are allowed
     to be different.

     @param O          Must be a type bit<W>
     @param D          Must be a tuple type where all the fields are bit-fields
                       (type bit<W> or int<W>) or varbits.
     @param T          Must be a type bit<W>
     @param M          Must be a type bit<W>

     extern void hash<O, T, D, M>(out O result, in HashAlgorithm algo,
                                  in T base, in D data, in M max); *)

  let eval_extern_hash (ctx : Ctx.t) : Ctx.t * Sig.t =
    let base = Ctx.find_value Ctx.Local "base" ctx |> Value.get_num in
    let max = Ctx.find_value Ctx.Local "max" ctx |> Value.get_num in
    let values =
      Ctx.find_value Ctx.Local "data" ctx |> Value.get_tuple_or_seq
    in
    let algo = Ctx.find_value Ctx.Local "algo" ctx in
    let result =
      match algo with
      | EnumFieldV ("HashAlgorithm", algo) ->
          Hash.compute_checksum algo values |> Hash.adjust base max
      | _ -> assert false
    in
    let value_result =
      let typ_result = Ctx.find_typ Ctx.Local "O" ctx in
      let value_result = Value.IntV result in
      Numerics.eval_cast typ_result value_result
    in
    let ctx = Ctx.update_value Ctx.Local "result" value_result ctx in
    (ctx, Sig.Ret None)

  (* Verifies the checksum of the supplied data.  If this method detects
     that a checksum of the data is not correct, then the value of the
     standard_metadata checksum_error field will be equal to 1 when the
     packet begins ingress processing.

     Calling verify_checksum is only supported in the VerifyChecksum
     control.

     @param T          Must be a tuple type where all the tuple elements
                       are of type bit<W>, int<W>, or varbit<W>.  The
                       total length of the fields must be a multiple of
                       the output size.
     @param O          Checksum type; must be bit<X> type.
     @param condition  If 'false' the verification always succeeds.
     @param data       Data whose checksum is verified.
     @param checksum   Expected checksum of the data; note that it must
                       be a left-value.
     @param algo       Algorithm to use for checksum (not all algorithms
                       may be supported).  Must be a compile-time
                       constant.

     extern void verify_checksum<T, O>(in bool condition, in T data,
                                       in O checksum, HashAlgorithm algo);

     verify_checksum_with_payload is identical in all ways to
     verify_checksum, except that it includes the payload of the packet
     in the checksum calculation.  The payload is defined as "all bytes
     of the packet which were not parsed by the parser".

     Calling verify_checksum_with_payload is only supported in the
     VerifyChecksum control.

     extern void verify_checksum_with_payload<T, O>(in bool condition, in T data,
                                                    in O checksum, HashAlgorithm algo); *)
  let eval_extern_verify_checksum ~(payload : bool) (ctx : Ctx.t) :
      Ctx.t * Sig.t =
    let condition =
      Ctx.find_value Ctx.Local "condition" ctx |> Value.get_bool
    in
    let values =
      Ctx.find_value Ctx.Local "data" ctx |> Value.get_tuple_or_seq
    in
    let values =
      if payload then
        let bytes_payload =
          get_pkt_in () |> Core.PacketIn.get_remaining_bytes |> Array.to_list
        in
        let values_payload =
          List.map
            (fun byte -> Value.FBitV (Bigint.of_int 8, byte))
            bytes_payload
        in
        values @ values_payload
      else values
    in
    let checksum_expect =
      Ctx.find_value Ctx.Local "checksum" ctx |> Value.get_num
    in
    let algo = Ctx.find_value Ctx.Local "algo" ctx in
    if not condition then (ctx, Sig.Ret None)
    else
      let checksum_compute =
        match algo with
        | EnumFieldV ("HashAlgorithm", algo) ->
            Hash.compute_checksum algo values
        | _ -> assert false
      in
      let verified = Bigint.(checksum_expect = checksum_compute) in
      let ctx =
        if not verified then
          let value_checksum_error =
            Value.FBitV (Bigint.of_int 1, Bigint.one)
          in
          let value_std_meta =
            Ctx.find_value Ctx.Global "standard_metadata" ctx
          in
          let value_std_meta =
            Value.update_struct_field value_std_meta "checksum_error"
              value_checksum_error
          in
          Ctx.update_value Ctx.Global "standard_metadata" value_std_meta ctx
        else ctx
      in
      (ctx, Sig.Ret None)

  (* Computes the checksum of the supplied data and writes it to the
     checksum parameter.

     Calling update_checksum is only supported in the ComputeChecksum
     control.

     @param T          Must be a tuple type where all the tuple elements
                       are of type bit<W>, int<W>, or varbit<W>.  The
                       total length of the fields must be a multiple of
                       the output size.
     @param O          Output type; must be bit<X> type.
     @param condition  If 'false' the checksum parameter is not changed
     @param data       Data whose checksum is computed.
     @param checksum   Checksum of the data.
     @param algo       Algorithm to use for checksum (not all algorithms
                       may be supported).  Must be a compile-time
                       constant.

     extern void update_checksum<T, O>(in bool condition, in T data,
                                       inout O checksum, HashAlgorithm algo);

     update_checksum_with_payload is identical in all ways to
     update_checksum, except that it includes the payload of the packet
     in the checksum calculation.  The payload is defined as "all bytes
     of the packet which were not parsed by the parser".

     Calling update_checksum_with_payload is only supported in the
     ComputeChecksum control.

     extern void update_checksum_with_payload<T, O>(in bool condition, in T data,
                                                    inout O checksum, HashAlgorithm algo); *)
  let eval_extern_update_checksum ~(payload : bool) (ctx : Ctx.t) :
      Ctx.t * Sig.t =
    let condition =
      Ctx.find_value Ctx.Local "condition" ctx |> Value.get_bool
    in
    let values =
      Ctx.find_value Ctx.Local "data" ctx |> Value.get_tuple_or_seq
    in
    let values =
      if payload then
        let bytes_payload =
          get_pkt_in () |> Core.PacketIn.get_remaining_bytes |> Array.to_list
        in
        let values_payload =
          List.map
            (fun byte -> Value.FBitV (Bigint.of_int 8, byte))
            bytes_payload
        in
        values @ values_payload
      else values
    in
    let algo = Ctx.find_value Ctx.Local "algo" ctx in
    if not condition then (ctx, Sig.Ret None)
    else
      let checksum_compute =
        match algo with
        | EnumFieldV ("HashAlgorithm", algo) ->
            Hash.compute_checksum algo values
        | _ -> assert false
      in
      let value_checksum =
        let typ_checksum = Ctx.find_typ Ctx.Local "O" ctx in
        let value_checksum = Value.IntV checksum_compute in
        Numerics.eval_cast typ_checksum value_checksum
      in
      let ctx = Ctx.update_value Ctx.Local "checksum" value_checksum ctx in
      (ctx, Sig.Ret None)

  let eval_extern_func_call (ctx : Ctx.t) (fid : FId.t) : Ctx.t * Sig.t =
    let fname, args = fid in
    match (fname, args) with
    (* core.p4 *)
    | "verify", [ ("check", false); ("toSignal", false) ] -> Core.verify ctx
    (* v1model.p4 *)
    | "mark_to_drop", [ ("standard_metadata", false) ] ->
        eval_extern_mark_to_drop ctx
    | ( "hash",
        [
          ("result", false);
          ("algo", false);
          ("base", false);
          ("data", false);
          ("max", false);
        ] ) ->
        eval_extern_hash ctx
    | ( ("verify_checksum" | "verify_checksum_with_payload"),
        [
          ("condition", false);
          ("data", false);
          ("checksum", false);
          ("algo", false);
        ] ) ->
        let payload = fname = "verify_checksum_with_payload" in
        eval_extern_verify_checksum ~payload ctx
    | ( ("update_checksum" | "update_checksum_with_payload"),
        [
          ("condition", false);
          ("data", false);
          ("checksum", false);
          ("algo", false);
        ] ) ->
        let payload = fname = "update_checksum_with_payload" in
        eval_extern_update_checksum ~payload ctx
    | _ ->
        Format.asprintf "(eval_extern) unknown extern: %a" FId.pp fid
        |> error_no_info

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
    | PacketOut pkt_out, ("emit", [ ("hdr", false) ]) ->
        let ctx, pkt_out = Core.PacketOut.emit ctx pkt_out in
        externs := Externs.add id (PacketOut pkt_out) !externs;
        (ctx, Sig.Ret None)
    | Counter ctr, ("count", [ ("index", false) ]) ->
        let pkt_in = get_pkt_in () in
        let len = pkt_in.len |> Bigint.of_int in
        let ctx, sign, ctr = Counter.count ctx len ctr in
        externs := Externs.add id (Counter ctr) !externs;
        (ctx, sign)
    | Register reg, ("read", [ ("result", false); ("index", false) ]) ->
        let ctx, sign, reg = Register.read ctx reg in
        externs := Externs.add id (Register reg) !externs;
        (ctx, sign)
    | Register reg, ("write", [ ("index", false); ("value", false) ]) ->
        let ctx, sign, reg = Register.write ctx reg in
        externs := Externs.add id (Register reg) !externs;
        (ctx, sign)
    | _ ->
        Format.asprintf "(TODO: eval_extern_method_call) %a.%a" pp_extern extern
          FId.pp fid
        |> error_no_info

  (* Table operations *)

  let find_table (id_table : Stf.Ast.name) : OId.t * Obj.t =
    let oid_table =
      match String.split_on_char '.' id_table with
      | [] -> assert false
      | [ id_unqualified ] ->
          !Interp.sto |> Sto.bindings |> List.map fst
          |> List.find (fun oid -> oid |> List.rev |> List.hd = id_unqualified)
      | oid_table -> oid_table
    in
    let obj_table = Sto.find oid_table !Interp.sto in
    (oid_table, obj_table)

  let find_table_key (id_key : Stf.Ast.name) (table : Table.t) :
      int * Il.Ast.table_key =
    let table_key_indexed =
      table.keys
      |> List.mapi (fun idx key -> (idx, key))
      |> List.filter_map (fun (idx, table_key) ->
             let expr_key, _match_kind, annos = table_key.it in
             let match_anno_name =
               List.exists
                 (fun anno ->
                   match anno.it with
                   | Lang.Ast.TextN ({ it = "name"; _ }, [ name ]) ->
                       let name =
                         String.sub name.it 1 (String.length name.it - 2)
                       in
                       name = id_key
                   | _ -> false)
                 annos
             in
             let match_expr_key =
               let id_key = Stf.Print.convert_dollar_to_brackets id_key in
               F.asprintf "%a" (Il.Pp.pp_expr ~level:0) expr_key = id_key
             in
             if match_anno_name || match_expr_key then Some (idx, table_key)
             else None)
    in
    match table_key_indexed with
    | [ (idx, table_key) ] -> (idx, table_key)
    | _ ->
        F.asprintf "(TODO: find_table_key) %a" Stf.Print.print_name id_key
        |> error_no_info

  let find_table_action (id_action : Stf.Ast.name) (table : Table.t) :
      Il.Ast.table_action =
    let id_action =
      match String.split_on_char '.' id_action with
      | [] -> assert false
      | [ id_unqualified ] -> id_unqualified
      (* (TODO) Should check that the prefix of the qualified name is valid *)
      | oid_action -> oid_action |> List.rev |> List.hd
    in
    let table_action =
      List.filter_map
        (fun table_action ->
          let var_action, _, _, _ = table_action.it in
          match var_action.it with
          | Lang.Ast.Top id | Lang.Ast.Current id ->
              if id.it = id_action then Some table_action else None)
        table.actions
    in
    match table_action with
    | [ table_action ] -> table_action
    | _ ->
        F.asprintf "(find_table_action) no matching action found for %a"
          Stf.Print.print_name id_action
        |> error_no_info

  let make_exact_key (num_key : Stf.Ast.number) =
    let num_key = num_key |> int_of_string |> Bigint.of_int in
    let value_key = Value.IntV num_key $ no_info in
    Il.Ast.(
      ValueE { value = value_key }
      $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))

  let make_ternary_key (num_key : Stf.Ast.number) =
    let hex = String.starts_with ~prefix:"0x" num_key in
    check hex "(TODO: make_ternary_key) non-hexadecimal number";
    let num_key = String.sub num_key 2 (String.length num_key - 2) in
    let base =
      String.init (String.length num_key) (fun i ->
          if num_key.[i] = '*' then '0' else num_key.[i])
    in
    let base = "0x" ^ base |> int_of_string |> Bigint.of_int in
    let value_base = Value.IntV base $ no_info in
    let expr_base =
      Il.Ast.(
        ValueE { value = value_base }
        $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))
    in
    let mask =
      String.init (String.length num_key) (fun i ->
          if num_key.[i] = '*' then '0' else 'F')
    in
    let mask = "0x" ^ mask |> int_of_string |> Bigint.of_int in
    let value_mask = Value.IntV mask $ no_info in
    let expr_mask =
      Il.Ast.(
        ValueE { value = value_mask }
        $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))
    in
    Il.Ast.(
      MaskE { expr_base; expr_mask }
      $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))

  let add_table_entry (id_table : Stf.Ast.name) (keys : Stf.Ast.mtch list)
      (action : Stf.Ast.action) (priority : int option) : unit =
    let oid_table, obj_table = find_table id_table in
    let id_table, table = Obj.get_table obj_table in
    let keysets =
      List.map
        (fun (id_key, mtchkind) ->
          let idx, table_key = find_table_key id_key table in
          let expr_key, match_kind, _annos = table_key.it in
          let typ_key = expr_key.note.typ in
          match mtchkind with
          | Stf.Ast.Num num_key ->
              let expr_key =
                match match_kind.it with
                | "exact" -> make_exact_key num_key
                | "ternary" -> make_ternary_key num_key
                | _ ->
                    F.asprintf "(TODO: add_table_entry) %a"
                      Stf.Print.print_mtchkind mtchkind
                    |> error_no_info
              in
              let typ_key_set = Types.SetT typ_key in
              let expr_key =
                Il.Ast.(
                  CastE { typ = typ_key_set $ no_info; expr = expr_key }
                  $$ (no_info, { typ = typ_key_set; ctk = Ctk.DYN }))
              in
              let keyset = Lang.Ast.ExprK expr_key $ no_info in
              (idx, keyset)
          | _ ->
              F.asprintf "(TODO: add_table_entry) %a" Stf.Print.print_mtchkind
                mtchkind
              |> error_no_info)
        keys
      |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
      |> List.map snd
    in
    let action =
      let id_action, args_action_supplied = action in
      let table_action = find_table_action id_action table in
      let var_action, args_action, annos, params_control = table_action.it in
      let module PMap = Map.Make (String) in
      let pmap_control =
        List.map it params_control
        |> List.mapi (fun idx (id, _, typ, _, _) -> (idx, id.it, typ.it))
        |> List.fold_left
             (fun pmap (idx, id, typ) -> PMap.add id (idx, typ) pmap)
             PMap.empty
      in
      let args_action_supplied =
        args_action_supplied
        |> List.map (fun arg_action_supplied ->
               (* (TODO) Should check the validity of the argument name *)
               let id_arg, num_arg = arg_action_supplied in
               let idx, typ = PMap.find id_arg pmap_control in
               let num_arg = num_arg |> int_of_string |> Bigint.of_int in
               let value_arg = Value.IntV num_arg $ no_info in
               let expr_arg =
                 Il.Ast.ValueE { value = value_arg } $$ no_info_expr
               in
               let expr_arg =
                 Il.Ast.(
                   CastE { typ = typ $ no_info; expr = expr_arg }
                   $$ (no_info, { typ; ctk = Ctk.DYN }))
               in
               let arg = Lang.Ast.ExprA expr_arg $ no_info in
               (idx, arg))
        |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
        |> List.map snd
      in
      let args_action = args_action @ args_action_supplied in
      (var_action, args_action, annos, []) $ no_info
    in
    (* (TODO) Should insert proper priority value *)
    let priority =
      Option.map
        (fun priority -> Value.IntV (Bigint.of_int priority) $ no_info)
        priority
    in
    let table = Table.add_entry keysets action priority table in
    let obj_table = Obj.TableO (id_table, table) in
    Interp.update oid_table obj_table

  (* Pipeline driver *)

  type port = int
  type packet = string
  type result = port * packet

  let compare_packet packet_out packet_expect : bool =
    let to_list s = List.init (String.length s) (String.get s) in
    let packet_out = to_list packet_out in
    let packet_expect = to_list packet_expect in
    List.length packet_out = List.length packet_expect
    && List.fold_left2
         (fun same o e -> same && (e = '*' || o = e))
         true packet_out packet_expect

  let compare_result (port_out, packet_out) (port_expect, packet_expect) : bool
      =
    let pass =
      port_out = port_expect && compare_packet packet_out packet_expect
    in
    if pass then
      F.printf "[PASS] Expected: %d %s / Got: %d %s\n" port_expect packet_expect
        port_out packet_out
    else
      F.printf "[FAIL] Expected: %d %s / Got: %d %s\n" port_expect packet_expect
        port_out packet_out;
    pass

  let drive_p (ctx : Ctx.t) : Ctx.t * Sig.t =
    let expr_base, func, args =
      let oid = [ "main"; "p" ] in
      let func = "apply" in
      let args = [ "packet_in"; "hdr"; "meta"; "standard_metadata" ] in
      make_call oid func args
    in
    Interp.eval_method_call Ctx.Global ctx expr_base func [] args

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

  let drive_pipe (ctx : Ctx.t) (port_in : port) (packet_in : packet) :
      result option =
    let ( let* ) = Option.bind in
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
    (* Execute the parser block *)
    let ctx, sign = ctx |> drive_p in
    let pkt_payload =
      get_pkt_in () |> F.asprintf "%a" Core.PacketIn.pp_remaining
    in
    let ctx =
      match sign with
      | Trans (`Reject value) ->
          let value_std_meta =
            let value_std_meta =
              Ctx.find_value Ctx.Global "standard_metadata" ctx
            in
            Value.update_struct_field value_std_meta "parser_error" value
          in
          Ctx.update_value Ctx.Global "standard_metadata" value_std_meta ctx
      | _ -> ctx
    in
    (* Execute the checksum verification block *)
    let ctx = ctx |> drive_vr in
    (* Execute the ingress block *)
    let ctx = ctx |> drive_ig in
    let drop =
      let value_std_meta = Ctx.find_value Ctx.Global "standard_metadata" ctx in
      Value.get_struct_field value_std_meta "egress_spec" = drop_spec
    in
    let* ctx = if drop then None else Some ctx in
    (* Execute the egress block if not dropped *)
    let ctx = ctx |> drive_eg in
    let drop =
      let value_std_meta = Ctx.find_value Ctx.Global "standard_metadata" ctx in
      Value.get_struct_field value_std_meta "egress_spec" = drop_spec
    in
    let* ctx = if drop then None else Some ctx in
    (* Execute the checksum computation block if not dropped *)
    let ctx = ctx |> drive_ck in
    (* Execute the deparser block if not dropped *)
    let ctx = ctx |> drive_dep in
    (* Check egress port *)
    let port_out =
      let value_std_meta = Ctx.find_value Ctx.Global "standard_metadata" ctx in
      let _, fields = Value.get_struct value_std_meta in
      List.assoc "egress_spec" fields |> Value.get_num |> Bigint.to_int_exn
    in
    (* Check output packet *)
    let packet_out = get_pkt_out () |> F.asprintf "%a" Core.PacketOut.pp in
    let packet_out = packet_out ^ pkt_payload in
    Some (port_out, packet_out)

  let drive_stf_stmt (ctx : Ctx.t) (pass : bool) (queue_packet : result list)
      (queue_expect : result list) (stmt_stf : Stf.Ast.stmt) :
      Ctx.t * bool * result list * result list =
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in) -> (
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let result_out = drive_pipe ctx port_in packet_in in
        match result_out with
        | None -> (ctx, pass, queue_packet, queue_expect)
        | Some (port_out, packet_out) -> (
            match queue_expect with
            | [] ->
                let queue_packet = queue_packet @ [ (port_out, packet_out) ] in
                (ctx, pass, queue_packet, queue_expect)
            | (port_expect, packet_expect) :: queue_expect ->
                let pass =
                  compare_result (port_out, packet_out)
                    (port_expect, packet_expect)
                  && pass
                in
                (ctx, pass, queue_packet, queue_expect)))
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
              compare_result (port_out, packet_out) (port_expect, packet_expect)
              && pass
            in
            (ctx, pass, queue_packet, queue_expect))
    (* Table operations *)
    | Stf.Ast.Add (id_table, priority, keys, action, _) ->
        add_table_entry id_table keys action priority;
        (ctx, pass, queue_packet, queue_expect)
    (* Timing *)
    | Stf.Ast.Wait -> (ctx, pass, queue_packet, queue_expect)
    | _ ->
        Format.asprintf "(drive_stf_stmt) unknown stf stmt: %a"
          Stf.Print.print_stmt stmt_stf
        |> error_no_info

  let drive_stf_stmts (ctx : Ctx.t) (stmts_stf : Stf.Ast.stmt list) : bool =
    let _, pass, queue_packet, queue_expect =
      List.fold_left
        (fun (ctx, pass, queue_packet, queue_expect) stmt_stf ->
          drive_stf_stmt ctx pass queue_packet queue_expect stmt_stf)
        (ctx, true, [], []) stmts_stf
    in
    let pass = pass && queue_packet = [] && queue_expect = [] in
    if queue_packet <> [] then (
      F.printf "[FAIL] Remaining packets to be matched:\n";
      List.iteri
        (fun idx (port, packet) -> F.printf "(%d) %d %s\n" idx port packet)
        queue_packet);
    if queue_expect <> [] then (
      F.printf "[FAIL] Expected packets to be output:\n";
      List.iteri
        (fun idx (port, packet) -> F.printf "(%d) %d %s\n" idx port packet)
        queue_expect);
    pass

  let drive (cenv : CEnv.t) (fenv : FEnv.t) (venv : VEnv.t) (sto : Sto.t)
      (stmts_stf : Stf.Ast.stmt list) : bool =
    let ctx = { Ctx.empty with global = { cenv; fenv; venv } } in
    let ctx, sto = init ctx sto in
    Interp.init sto;
    drive_stf_stmts ctx stmts_stf
end
