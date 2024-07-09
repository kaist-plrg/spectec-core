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

open Runtime.Base
open Runtime.Context

let hash_csum16 (width, value) : Bigint.t =
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

let hash (algo : string) =
  match algo with
  | "csum16" -> hash_csum16
  | _ ->
      Format.eprintf "Hash algorithm %s not supported\n" algo;
      assert false

let package (data : Value.t list) : Bigint.t * Bigint.t =
  data
  |> List.map (fun value -> (Value.get_width value, Value.get_num value))
  |> List.map (fun (width, value) ->
         (width, Runtime.Ops.of_two_complement value width))
  |> List.fold_left
       (fun (width_packed, value_packed) (width, value) ->
         Bigint.
           ( width_packed + width,
             Runtime.Ops.shift_bitstring_left value_packed width + value ))
       Bigint.(zero, zero)

let compute_checksum (algo : string) (data : Value.t list) =
  package data |> hash algo

(* Corresponds to extern void verify_checksum<T, O>
   (in bool condition, in T data, inout O checksum, HashAlgorithm algo); *)
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
    let cond = Ctx.find_var "condition" ctx |> snd in
    match cond with BoolV b -> b | _ -> assert false
  in
  if not cond then ctx
  else
    let data =
      let data = Ctx.find_var "data" ctx |> snd in
      match data with TupleV data -> data | _ -> assert false
    in
    let checksum = Ctx.find_var "checksum" ctx |> snd in
    let algo =
      let algo = Ctx.find_var "algo" ctx |> snd in
      match algo with
      | EnumFieldV ("HashAlgorithm", algo) -> algo
      | _ -> assert false
    in
    let checksum_verified = compute_checksum algo data in
    Format.eprintf
      "(TODO: verify_checksum) checksum given %a ; computed %a from %a\n"
      Value.pp checksum Bigint.pp checksum_verified Value.pp (TupleV data);
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
    let cond = Ctx.find_var "condition" ctx |> snd in
    match cond with BoolV b -> b | _ -> assert false
  in
  if not cond then ctx
  else
    let data =
      let data = Ctx.find_var "data" ctx |> snd in
      match data with TupleV data -> data | _ -> assert false
    in
    let algo =
      let algo = Ctx.find_var "algo" ctx |> snd in
      match algo with
      | EnumFieldV ("HashAlgorithm", algo) -> algo
      | _ -> assert false
    in
    let typ = Ctx.find_td "O" ctx in
    (* (TODO) is this the right way to cast? *)
    let value =
      AIntV (compute_checksum algo data) |> Runtime.Ops.eval_cast typ
    in
    let ctx = Ctx.update_var "checksum" typ value ctx in
    ctx
