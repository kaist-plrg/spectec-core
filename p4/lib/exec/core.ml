module Value = Runtime_static.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type
module Numerics = Runtime_static.Numerics
open Util.Error

let error_no_info = error_interp_no_info
let check = check_interp

(* Bit manipulation *)

type bits = bool Array.t

let string_to_bits str =
  let char_to_bits c =
    let n =
      match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
      | _ -> assert false
    in
    [ n land 8 <> 0; n land 4 <> 0; n land 2 <> 0; n land 1 <> 0 ]
  in
  str |> String.to_seq |> List.of_seq |> List.map char_to_bits |> List.flatten
  |> Array.of_list

let bits_to_string bits =
  let bits_to_int bits =
    List.fold_left (fun i bit -> (i lsl 1) + if bit then 1 else 0) 0 bits
  in
  let int_to_char i =
    if i < 10 then Char.chr (i + Char.code '0')
    else Char.chr (i - 10 + Char.code 'A')
  in
  let len = Array.length bits in
  let rec loop idx str =
    if idx >= len then str
    else
      let bits = Array.sub bits idx (min 4 (len - idx)) |> Array.to_list in
      let bits =
        if List.length bits < 4 then
          bits @ List.init (4 - List.length bits) (fun _ -> false)
        else bits
      in
      let c = bits |> bits_to_int |> int_to_char in
      loop (idx + 4) (str ^ String.make 1 c)
  in
  loop 0 ""

let bits_to_int_unsigned bits =
  Array.fold_left (fun i bit -> (i lsl 1) + if bit then 1 else 0) 0 bits
  |> Bigint.of_int

let bits_to_int_signed bits =
  let sign = bits.(0) in
  let int_unsigned = bits_to_int_unsigned bits in
  if sign then
    let int_max =
      let len = Array.length bits - 1 in
      Bigint.(one lsl len)
    in
    Bigint.(int_unsigned - (int_max * (one + one)))
  else int_unsigned

let int_to_bits_unsigned value size =
  Array.init size (fun i -> Bigint.(value land (one lsl i) > zero))
  |> Array.to_list |> List.rev |> Array.of_list

let int_to_bits_signed value size =
  let mask = Bigint.((one lsl size) - one) in
  let value = Bigint.(value land mask) in
  int_to_bits_unsigned value size

(* Core extern objects *)

(* Input packet *)

module PacketIn = struct
  type t = { bits : bits; idx : int; len : int }

  let pp fmt pkt = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let pp_remaining fmt pkt =
    let bits = Array.sub pkt.bits pkt.idx (pkt.len - pkt.idx) in
    Format.fprintf fmt "%s" (bits_to_string bits)

  let init (pkt : string) =
    let bits = string_to_bits pkt in
    { bits; idx = 0; len = Array.length bits }

  let get_remaining (pkt : t) =
    let bits = Array.sub pkt.bits pkt.idx (pkt.len - pkt.idx) in
    bits

  let rec sizeof ?(varsize = 0) (ctx : Ctx.t) (typ : Type.t) : int =
    let typ = Ctx.resolve_typ Ctx.Local typ ctx |> Type.canon in
    match typ with
    | DefT _ | SpecT _ -> assert false
    | BoolT -> 1
    | FIntT width | FBitT width -> width |> Bigint.to_int_exn
    | VBitT _ -> varsize
    | StructT (_, fields) | HeaderT (_, fields) ->
        fields
        |> List.map (fun (_, typ) -> sizeof ~varsize ctx typ)
        |> List.fold_left ( + ) 0
    | _ ->
        Format.asprintf "(TODO: sizeof) %a" (Type.pp ~level:0) typ
        |> error_no_info

  let rec sizeof_max (ctx : Ctx.t) (typ : Type.t) : int =
    let typ = Ctx.resolve_typ Ctx.Local typ ctx |> Type.canon in
    match typ with
    | DefT _ | SpecT _ -> assert false
    | BoolT -> 1
    | FIntT width | FBitT width | VBitT width -> width |> Bigint.to_int_exn
    | StructT (_, fields) | HeaderT (_, fields) ->
        fields
        |> List.map (fun (_, typ) -> sizeof_max ctx typ)
        |> List.fold_left ( + ) 0
    | _ ->
        Format.asprintf "(TODO: sizeof_max) %a" (Type.pp ~level:0) typ
        |> error_no_info

  let parse pkt (size : int) =
    let bits = Array.sub pkt.bits pkt.idx size in
    let pkt = { pkt with idx = pkt.idx + size } in
    (pkt, bits)

  let rec write ?(varsize = 0) (bits_in : bits) (value : Value.t) =
    match value with
    | BoolV _ ->
        let bits = Array.sub bits_in 0 1 in
        let bits_in = Array.sub bits_in 1 (Array.length bits_in - 1) in
        let value = Value.BoolV bits.(0) in
        (bits_in, value)
    | FIntV (width, _) ->
        let size = width |> Bigint.to_int_exn in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value = Value.FIntV (width, bits_to_int_signed bits) in
        (bits_in, value)
    | FBitV (width, _) ->
        let size = width |> Bigint.to_int_exn in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value = Value.FBitV (width, bits_to_int_unsigned bits) in
        (bits_in, value)
    | VBitV (width_max, _, _) ->
        let size = varsize in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value =
          Value.VBitV
            (width_max, Bigint.of_int varsize, bits_to_int_unsigned bits)
        in
        (bits_in, value)
    | StructV (id, fields) ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (member, value) ->
              let bits_in, value = write ~varsize bits_in value in
              (bits_in, fields @ [ (member, value) ]))
            (bits_in, []) fields
        in
        let value = Value.StructV (id, fields) in
        (bits_in, value)
    | HeaderV (id, _, fields) ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (member, value) ->
              let bits_in, value = write ~varsize bits_in value in
              (bits_in, fields @ [ (member, value) ]))
            (bits_in, []) fields
        in
        let value = Value.HeaderV (id, true, fields) in
        (bits_in, value)
    | _ ->
        Format.asprintf "(TODO: write) %a" (Value.pp ~level:0) value |> failwith

  (* Read a header from the packet into a fixed-sized header @hdr and advance the cursor.
     May trigger error PacketTooShort or StackOutOfBounds.
     @T must be a fixed-size header type

     void extract<T>(out T hdr); *)
  let extract (ctx : Ctx.t) pkt : Ctx.t * Sig.t * t =
    let typ = Ctx.find_typ Ctx.Local "T" ctx in
    let hdr = Ctx.find_value Ctx.Local "hdr" ctx in
    let size = sizeof ctx typ in
    if pkt.idx + size > pkt.len then
      let sign = Sig.Trans (`Reject (Value.ErrV "PacketTooShort")) in
      (ctx, sign, pkt)
    else
      let pkt, bits = parse pkt size in
      let hdr = write bits hdr |> snd in
      let ctx = Ctx.update_value Ctx.Local "hdr" hdr ctx in
      let sign = Sig.Ret None in
      (ctx, sign, pkt)

  (* Read bits from the packet into a variable-sized header @variableSizeHeader
     and advance the cursor.
     @T must be a header containing exactly 1 varbit field.
     May trigger errors PacketTooShort, StackOutOfBounds, or HeaderTooShort.

     void extract<T>(out T variableSizeHeader,
                      in bit<32> variableFieldSizeInBits); *)
  let extract_varsize (ctx : Ctx.t) pkt : Ctx.t * Sig.t * t =
    let typ = Ctx.find_typ Ctx.Local "T" ctx in
    let hdr = Ctx.find_value Ctx.Local "variableSizeHeader" ctx in
    let varsize =
      Ctx.find_value Ctx.Local "variableFieldSizeInBits" ctx |> Value.get_num
    in
    let alignment =
      Numerics.eval_bitstring_access' varsize (Bigint.of_int 2)
        (Bigint.of_int 0)
      |> Value.get_num |> Bigint.to_int_exn
    in
    if alignment <> 0 then
      let sign = Sig.Trans (`Reject (Value.ErrV "ParserInvalidArgument")) in
      (ctx, sign, pkt)
    else
      let varsize = varsize |> Bigint.to_int_exn in
      let size = sizeof ~varsize ctx typ in
      let size_max = sizeof_max ctx typ in
      if pkt.idx + size > pkt.len then
        let sign = Sig.Trans (`Reject (Value.ErrV "PacketTooShort")) in
        (ctx, sign, pkt)
      else if size > size_max then
        let sign = Sig.Trans (`Reject (Value.ErrV "HeaderTooShort")) in
        (ctx, sign, pkt)
      else
        let pkt, bits = sizeof ~varsize ctx typ |> parse pkt in
        let hdr = write ~varsize bits hdr |> snd in
        let ctx = Ctx.update_value Ctx.Local "variableSizeHeader" hdr ctx in
        let sign = Sig.Ret None in
        (ctx, sign, pkt)

  (* Read bits from the packet without advancing the cursor.
     @returns: the bits read from the packet.
     T may be an arbitrary fixed-size type.

     T lookahead<T>(); *)
  let lookahead (ctx : Ctx.t) pkt : Sig.t =
    let typ = Ctx.find_typ Ctx.Local "T" ctx in
    let hdr = Numerics.eval_default typ in
    let size = sizeof ctx typ in
    if pkt.idx + size > pkt.len then
      Sig.Trans (`Reject (Value.ErrV "PacketTooShort"))
    else
      let _pkt, bits = parse pkt size in
      let hdr = write bits hdr |> snd in
      Sig.Ret (Some hdr)

  (* Advance the packet cursor by the specified number of bits.

     void advance(in bit<32> sizeInBits); *)
  let advance (ctx : Ctx.t) pkt =
    let size =
      Ctx.find_value Ctx.Local "sizeInBits" ctx
      |> Value.get_num |> Bigint.to_int_exn
    in
    let pkt = { pkt with idx = pkt.idx + size } in
    pkt

  (* @return packet length in bytes.  This method may be unavailable on
     some target architectures.

     bit<32> length(); *)
  let length pkt =
    let len = if pkt.len mod 8 = 0 then pkt.len / 8 else (pkt.len / 8) + 1 in
    Value.FIntV (Bigint.of_int 32, Bigint.of_int len)
end

(* Output packet *)

module PacketOut = struct
  type t = { bits : bits }

  let pp fmt pkt = Format.fprintf fmt "%s" (bits_to_string pkt.bits)
  let init () = { bits = Array.make 0 false }

  let rec deparse pkt (value : Value.t) =
    match value with
    | BoolV b -> { bits = Array.append pkt.bits (Array.make 1 b) }
    | FIntV (width, value) ->
        let width = width |> Bigint.to_int_exn in
        let bits = int_to_bits_signed value width in
        { bits = Array.append pkt.bits bits }
    | FBitV (width, value) | VBitV (_, width, value) ->
        let width = width |> Bigint.to_int_exn in
        let bits = int_to_bits_unsigned value width in
        { bits = Array.append pkt.bits bits }
    | StackV (values, _, _) ->
        List.fold_left (fun pkt value -> deparse pkt value) pkt values
    | StructV (_, fields) ->
        List.fold_left (fun pkt (_, value) -> deparse pkt value) pkt fields
    | HeaderV (_, valid, fields) ->
        if valid then
          List.fold_left (fun pkt (_, value) -> deparse pkt value) pkt fields
        else pkt
    | UnionV (_, fields) ->
        List.fold_left (fun pkt (_, value) -> deparse pkt value) pkt fields
    | _ ->
        Format.asprintf "(TODO: deparse) %a" (Value.pp ~level:0) value
        |> error_no_info

  (* Write @hdr into the output packet, advancing cursor.
     @T can be a header type, a header stack, a header_union, or a struct
     containing fields with such types.

     void emit<T>(in T hdr); *)
  let emit (ctx : Ctx.t) pkt =
    let hdr = Ctx.find_value Ctx.Local "hdr" ctx in
    let pkt = deparse pkt hdr in
    (ctx, pkt)
end

(* Core extern functions *)

(* Check a predicate @check in the parser; if the predicate is true do nothing,
   otherwise set the parser error to @toSignal, and transition to the `reject` state.

   extern void verify(in bool check, in error toSignal); *)
let verify (ctx : Ctx.t) : Ctx.t * Sig.t =
  let check = Ctx.find_value Ctx.Local "check" ctx |> Value.get_bool in
  if check then (ctx, Sig.Ret None)
  else
    let value_error = Ctx.find_value Ctx.Local "toSignal" ctx in
    let sign = Sig.Trans (`Reject value_error) in
    (ctx, sign)
