(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

open Runtime.Context
module R = Runtime

[@@@ocamlformat "disable"]
let crc16_table = [|
  0x0000;0x1021;0x2042;0x3063;0x4084;0x50a5;0x60c6;0x70e7;
  0x8108;0x9129;0xa14a;0xb16b;0xc18c;0xd1ad;0xe1ce;0xf1ef;
  0x1231;0x0210;0x3273;0x2252;0x52b5;0x4294;0x72f7;0x62d6;
  0x9339;0x8318;0xb37b;0xa35a;0xd3bd;0xc39c;0xf3ff;0xe3de;
  0x2462;0x3443;0x0420;0x1401;0x64e6;0x74c7;0x44a4;0x5485;
  0xa56a;0xb54b;0x8528;0x9509;0xe5ee;0xf5cf;0xc5ac;0xd58d;
  0x3653;0x2672;0x1611;0x0630;0x76d7;0x66f6;0x5695;0x46b4;
  0xb75b;0xa77a;0x9719;0x8738;0xf7df;0xe7fe;0xd79d;0xc7bc;
  0x48c4;0x58e5;0x6886;0x78a7;0x0840;0x1861;0x2802;0x3823;
  0xc9cc;0xd9ed;0xe98e;0xf9af;0x8948;0x9969;0xa90a;0xb92b;
  0x5af5;0x4ad4;0x7ab7;0x6a96;0x1a71;0x0a50;0x3a33;0x2a12;
  0xdbfd;0xcbdc;0xfbbf;0xeb9e;0x9b79;0x8b58;0xbb3b;0xab1a;
  0x6ca6;0x7c87;0x4ce4;0x5cc5;0x2c22;0x3c03;0x0c60;0x1c41;
  0xedae;0xfd8f;0xcdec;0xddcd;0xad2a;0xbd0b;0x8d68;0x9d49;
  0x7e97;0x6eb6;0x5ed5;0x4ef4;0x3e13;0x2e32;0x1e51;0x0e70;
  0xff9f;0xefbe;0xdfdd;0xcffc;0xbf1b;0xaf3a;0x9f59;0x8f78;
  0x9188;0x81a9;0xb1ca;0xa1eb;0xd10c;0xc12d;0xf14e;0xe16f;
  0x1080;0x00a1;0x30c2;0x20e3;0x5004;0x4025;0x7046;0x6067;
  0x83b9;0x9398;0xa3fb;0xb3da;0xc33d;0xd31c;0xe37f;0xf35e;
  0x02b1;0x1290;0x22f3;0x32d2;0x4235;0x5214;0x6277;0x7256;
  0xb5ea;0xa5cb;0x95a8;0x8589;0xf56e;0xe54f;0xd52c;0xc50d;
  0x34e2;0x24c3;0x14a0;0x0481;0x7466;0x6447;0x5424;0x4405;
  0xa7db;0xb7fa;0x8799;0x97b8;0xe75f;0xf77e;0xc71d;0xd73c;
  0x26d3;0x36f2;0x0691;0x16b0;0x6657;0x7676;0x4615;0x5634;
  0xd94c;0xc96d;0xf90e;0xe92f;0x99c8;0x89e9;0xb98a;0xa9ab;
  0x5844;0x4865;0x7806;0x6827;0x18c0;0x08e1;0x3882;0x28a3;
  0xcb7d;0xdb5c;0xeb3f;0xfb1e;0x8bf9;0x9bd8;0xabbb;0xbb9a;
  0x4a75;0x5a54;0x6a37;0x7a16;0x0af1;0x1ad0;0x2ab3;0x3a92;
  0xfd2e;0xed0f;0xdd6c;0xcd4d;0xbdaa;0xad8b;0x9de8;0x8dc9;
  0x7c26;0x6c07;0x5c64;0x4c45;0x3ca2;0x2c83;0x1ce0;0x0cc1;
  0xef1f;0xff3e;0xcf5d;0xdf7c;0xaf9b;0xbfba;0x8fd9;0x9ff8;
  0x6e17;0x7e36;0x4e55;0x5e74;0x2e93;0x3eb2;0x0ed1;0x1ef0;
|]
[@@@ocamlformat "enable"]

let compute_hash_crc16 (width, value) : Bigint.t =
  let rec partition_bytes width value =
    if Bigint.(width = zero) then []
    else
      let msb = Bigint.(width - one) in
      let lbytes = Bigint.(width - of_int 8) in
      let hd = Runtime.Ops.slice_bitstring value msb lbytes in
      hd
      :: partition_bytes lbytes
           (Runtime.Ops.slice_bitstring value Bigint.(lbytes - one) Bigint.zero)
  in
  let bytes = partition_bytes width value in
  let f crc bytes =
    let crc = Runtime.Ops.shift_bitstring_left crc (Bigint.of_int 8) in
    let crc =
      Bigint.bit_and crc Bigint.(Runtime.Ops.power_of_two (of_int 16) - one)
    in
    let idx =
      Bigint.bit_xor
        (Runtime.Ops.shift_bitstring_right crc (Bigint.of_int 8) false
           Bigint.zero)
        bytes
    in
    let idx = Bigint.to_int_exn idx in
    Bigint.bit_xor crc (crc16_table.(idx) |> Bigint.of_int)
  in
  List.fold_left f Bigint.zero bytes

let compute_hash_csum16 (width, value) : Bigint.t =
  if Bigint.(width % of_int 16 <> zero) then (
    Format.eprintf "Input to csum16 is not 16-bit aligned\n";
    assert false);
  let rec hash value_hash width value =
    if Bigint.(width = zero) then value_hash
    else
      let msb = Bigint.(width - one) in
      let lsb = Bigint.(width - of_int 16) in
      let value_hash =
        Runtime.Ops.add_one_complement value_hash
          (Runtime.Ops.slice_bitstring value msb lsb)
      in
      hash value_hash lsb
        Bigint.(Runtime.Ops.slice_bitstring value (msb - one) zero)
  in
  let value_hash = hash Bigint.zero width value in
  Runtime.Ops.bitwise_neg value_hash (Bigint.of_int 16)

let compute_hash (algo : string) =
  match algo with
  | "csum16" -> compute_hash_csum16
  | "crc16" -> compute_hash_crc16
  | _ -> Format.asprintf "Hash algorithm %s not supported\n" algo |> failwith

let package (data : R.Value.t list) : Bigint.t * Bigint.t =
  data
  |> List.map (fun value -> (R.Value.get_width value, R.Value.get_num value))
  |> List.map (fun (width, value) ->
         (width, Runtime.Ops.of_two_complement value width))
  |> List.fold_left
       (fun (width_packed, value_packed) (width, value) ->
         Bigint.
           ( width_packed + width,
             Runtime.Ops.shift_bitstring_left value_packed width + value ))
       Bigint.(zero, zero)

let compute_checksum (algo : string) (data : R.Value.t list) =
  package data |> compute_hash algo

(* Calculate a hash function of the value specified by the data
   parameter.  The value written to the out parameter named result
   will always be in the range [base, base+max-1] inclusive, if max >=
   1.  If max=0, the value written to result will always be base.

   Note that the types of all of the parameters may be the same as, or
   different from, each other, and thus their bit widths are allowed
   to be different.

   @param O          Must be a type bit<W>
   @param D          Must be a tuple type where all the fields are bit-fields (type bit<W> or int<W>) or varbits.
   @param T          Must be a type bit<W>
   @param M          Must be a type bit<W>
   extern void hash<O, T, D, M>
    (out O result, in HashAlgorithm algo, in T base, in D data, in M max); *)
let hash (ctx : Ctx.t) =
  let eid, algo = Ctx.find_var "algo" ctx |> R.Value.get_enum in
  assert (eid = "HashAlgorithm");
  let base = Ctx.find_var "base" ctx |> R.Value.get_num in
  let data = Ctx.find_var "data" ctx |> R.Value.get_tuple in
  let max_ = Ctx.find_var "max" ctx |> R.Value.get_num in
  let data = compute_checksum algo data in
  let data =
    if Bigint.(max_ = zero) then base
    else if Bigint.(max_ >= one) then Bigint.((data % (max_ - base)) + base)
    else base (* TODO: behavior in this case is unspecified *)
  in
  let typ = Ctx.find_td "O" ctx in
  (* (TODO) is this the right way to cast? *)
  let result = IntV data |> Runtime.Ops.eval_cast typ in
  let ctx = Ctx.update_var "result" result ctx in
  ctx

(* Verifies the checksum of the supplied data.
   If this method detects that a checksum of the data is not correct it
   sets the standard_metadata checksum_error bit.
   @param T          Must be a tuple type where all the fields are bit-fields or varbits.
                     The total dynamic length of the fields is a multiple of the output size.
   @param O          Checksum type; must be bit<X> type.
   @param condition  If 'false' the verification always succeeds.
   @param data       Data whose checksum is verified.
   @param checksum   Expected checksum of the data; note that is must be a left-value.
   @param algo       Algorithm to use for checksum (not all algorithms may be supported).
                     Must be a compile-time constant.
   extern void verify_checksum<T, O>
    (in bool condition, in T data, in O checksum, HashAlgorithm algo); *)
let verify_checksum (ctx : Ctx.t) =
  let cond =
    let cond = Ctx.find_var "condition" ctx in
    match cond with BoolV b -> b | _ -> assert false
  in
  if not cond then ctx
  else
    let data =
      let data = Ctx.find_var "data" ctx in
      match data with TupleV data -> data | _ -> assert false
    in
    let checksum = Ctx.find_var "checksum" ctx in
    let algo =
      let algo = Ctx.find_var "algo" ctx in
      match algo with
      | EnumFieldV ("HashAlgorithm", algo) -> algo
      | _ -> assert false
    in
    let checksum_verified = compute_checksum algo data in
    Format.eprintf
      "(TODO: verify_checksum) checksum given %a ; computed %a from %a\n"
      R.Value.pp checksum Bigint.pp checksum_verified R.Value.pp (TupleV data);
    ctx

(* Computes the checksum of the supplied data.
   @param T          Must be a tuple type where all the fields are bit-fields or varbits.
                     The total dynamic length of the fields is a multiple of the output size.
   @param O          Output type; must be bit<X> type.
   @param condition  If 'false' the checksum is not changed
   @param data       Data whose checksum is computed.
   @param checksum   Checksum of the data.
   @param algo       Algorithm to use for checksum (not all algorithms may be supported).
                     Must be a compile-time constant.
   extern void update_checksum<T, O>
    (in bool condition, in T data, inout O checksum, HashAlgorithm algo); *)
let update_checksum (ctx : Ctx.t) =
  let cond =
    let cond = Ctx.find_var "condition" ctx in
    match cond with BoolV b -> b | _ -> assert false
  in
  if not cond then ctx
  else
    let data =
      let data = Ctx.find_var "data" ctx in
      match data with TupleV data -> data | _ -> assert false
    in
    let algo =
      let algo = Ctx.find_var "algo" ctx in
      match algo with
      | EnumFieldV ("HashAlgorithm", algo) -> algo
      | _ -> assert false
    in
    let typ = Ctx.find_td "O" ctx in
    (* (TODO) is this the right way to cast? *)
    let result =
      IntV (compute_checksum algo data) |> Runtime.Ops.eval_cast typ
    in
    let ctx = Ctx.update_var "checksum" result ctx in
    ctx
