open Runtime.Base
open Runtime.Context

let string_to_bits str =
  let char_to_bits c =
    match c with
    | '0' -> [ false; false; false; false ]
    | '1' -> [ false; false; false; true ]
    | '2' -> [ false; false; true; false ]
    | '3' -> [ false; false; true; true ]
    | '4' -> [ false; true; false; false ]
    | '5' -> [ false; true; false; true ]
    | '6' -> [ false; true; true; false ]
    | '7' -> [ false; true; true; true ]
    | '8' -> [ true; false; false; false ]
    | '9' -> [ true; false; false; true ]
    | 'a' | 'A' -> [ true; false; true; false ]
    | 'b' | 'B' -> [ true; false; true; true ]
    | 'c' | 'C' -> [ true; true; false; false ]
    | 'd' | 'D' -> [ true; true; false; true ]
    | 'e' | 'E' -> [ true; true; true; false ]
    | 'f' | 'F' -> [ true; true; true; true ]
    | _ -> assert false
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
      let c = int_to_char (bits_to_int bits) in
      loop (idx + 4) (str ^ String.make 1 c)
  in
  loop 0 ""

let bits_to_int bits =
  let bits = Array.to_list bits |> List.rev |> Array.of_list in
  let n = Array.length bits in
  let rec aux i acc =
    if i = n then acc
    else
      let acc = if bits.(i) then acc lor (1 lsl i) else acc in
      aux (i + 1) acc
  in
  let n = aux 0 0 |> Bigint.of_int in
  n

let int_to_bits value size =
  Array.init size (fun i -> Bigint.(value land (one lsl i) > zero))
  |> Array.to_list |> List.rev |> Array.of_list

(* The declaration of a header type is given by the following syntax: ...
   nested arbitrary, as long as all of the “leaf” types are bit<W>, int<W>,
   a serializable enum, or a bool (7.2.2) *)
let rec sizeof ?(size_var = 0) (ctx : Ctx.t) (typ : Type.t) =
  match typ with
  | BoolT -> 1
  | IntT width | BitT width -> Bigint.to_int width |> Option.get
  | VBitT _ -> size_var
  | HeaderT fields ->
      List.fold_left
        (fun acc (_, typ) -> acc + sizeof ~size_var ctx typ)
        0 fields
  | NameT _ | NewT _ -> Ctx.simplify_td typ ctx |> sizeof ~size_var ctx
  | SEnumT (_, typ, _) -> sizeof ~size_var ctx typ
  | _ -> assert false

module PacketIn = struct
  type t = { bits : bool Array.t; idx : int; len : int }

  let init pkt =
    let bits = string_to_bits pkt in
    { bits; idx = 0; len = Array.length bits }

  let pp fmt (pkt : t) = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let parse (pkt : t) (size : int) =
    let bits = Array.sub pkt.bits pkt.idx size in
    ({ pkt with idx = pkt.idx + size }, bits)

  (* (TODO) enforce that the variable-sized field is used only once *)
  let rec write ?(size_var = 0) (bits_in : bool Array.t) (value : Value.t) =
    match value with
    | BoolV _ ->
        let bit = Array.get bits_in 0 in
        let bits_in = Array.sub bits_in 1 (Array.length bits_in - 1) in
        let value = Value.BoolV bit in
        (bits_in, value)
    | IntV (width, _) ->
        let size = Bigint.to_int width |> Option.get in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value = Value.IntV (width, bits_to_int bits) in
        (bits_in, value)
    | BitV (width, _) ->
        let size = Bigint.to_int width |> Option.get in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value = Value.BitV (width, bits_to_int bits) in
        (bits_in, value)
    | VBitV (max_width, _, _) ->
        let bits = Array.sub bits_in 0 size_var in
        let bits_in =
          Array.sub bits_in size_var (Array.length bits_in - size_var)
        in
        let value =
          Value.VBitV (max_width, Bigint.of_int size_var, bits_to_int bits)
        in
        (bits_in, value)
    | StructV fields ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (key, value) ->
              let bits_in, value = write ~size_var bits_in value in
              (bits_in, fields @ [ (key, value) ]))
            (bits_in, []) fields
        in
        (bits_in, Value.StructV fields)
    | HeaderV (_, fields) ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (key, value) ->
              let bits_in, value = write ~size_var bits_in value in
              (bits_in, fields @ [ (key, value) ]))
            (bits_in, []) fields
        in
        (bits_in, Value.HeaderV (true, fields))
    | SEnumFieldV (id, member, value) ->
        let bits_in, value = write ~size_var bits_in value in
        (bits_in, Value.SEnumFieldV (id, member, value))
    | _ ->
        Format.asprintf "Cannot write value %a to packet" Value.pp value
        |> failwith

  (* Read a header from the packet into a fixed-sized header @hdr and advance the cursor.
     May trigger error PacketTooShort or StackOutOfBounds.
     @T must be a fixed-size header type
     void extract<T>(out T hdr); *)
  let extract (ctx : Ctx.t) (pkt : t) =
    let typ = Ctx.find_td "T" ctx |> Option.get in
    let _, header = Ctx.find_var "hdr" ctx |> Option.get in
    let pkt, bits = sizeof ctx typ |> parse pkt in
    let _, header = write bits header in
    let ctx = Ctx.update_var "hdr" typ header ctx in
    (ctx, pkt)

  (* Read bits from the packet into a variable-sized header @variableSizeHeader
     and advance the cursor.
     @T must be a header containing exactly 1 varbit field.
     May trigger errors PacketTooShort, StackOutOfBounds, or HeaderTooShort.
     void extract<T>(out T variableSizeHeader,
                    in bit<32> variableFieldSizeInBits); *)
  let extract_var (ctx : Ctx.t) (pkt : t) =
    let typ = Ctx.find_td "T" ctx |> Option.get in
    let _, header = Ctx.find_var "variableSizeHeader" ctx |> Option.get in
    let size_var =
      Ctx.find_var "variableFieldSizeInBits" ctx
      |> Option.get |> snd |> Runtime.Ops.extract_bigint |> Bigint.to_int
      |> Option.get
    in
    let pkt, bits = sizeof ~size_var ctx typ |> parse pkt in
    let _, header = write ~size_var bits header in
    let ctx = Ctx.update_var "variableSizeHeader" typ header ctx in
    (ctx, pkt)

  (* Read bits from the packet without advancing the cursor.
     @returns: the bits read from the packet.
     T may be an arbitrary fixed-size type.
     T lookahead<T>(); *)
  let lookahead (ctx : Ctx.t) (pkt : t) =
    let typ = Ctx.find_td "T" ctx |> Option.get in
    let _pkt, bits = sizeof ctx typ |> parse pkt in
    let value = Runtime.Ops.eval_default_value typ in
    let _, value = write bits value in
    (ctx, value)

  (* Advance the packet cursor by the specified number of bits.
     void advance(in bit<32> sizeInBits); *)
  let advance (ctx : Ctx.t) (pkt : t) =
    let size =
      Ctx.find_var "sizeInBits" ctx
      |> Option.get |> snd |> Runtime.Ops.extract_bigint |> Bigint.to_int
      |> Option.get
    in
    let pkt = { pkt with idx = pkt.idx + size } in
    (ctx, pkt)

  (* @return packet length in bytes. This method may be unavailable on
     some target architectures.
     bit<32> length(); *)
  let length (ctx : Ctx.t) (pkt : t) =
    let len =
      (if pkt.len mod 8 = 0 then pkt.len / 8 else (pkt.len / 8) + 1)
      |> Bigint.of_int
    in
    (ctx, Value.BitV (Bigint.of_int 32, len))
end

module PacketOut = struct
  type t = { bits : bool Array.t }

  let init = { bits = [||] }
  let pp fmt (pkt : t) = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let rec deparse (pkt : t) (value : Value.t) =
    match value with
    | BoolV b -> { bits = Array.append pkt.bits (Array.make 1 b) }
    | IntV (width, value) | BitV (width, value) | VBitV (_, width, value) ->
        let size = Bigint.to_int width |> Option.get in
        let bits = int_to_bits value size in
        { bits = Array.append pkt.bits bits }
    | StackV (values, _, _) -> List.fold_left deparse pkt values
    | StructV fields ->
        List.fold_left (fun pkt (_, value) -> deparse pkt value) pkt fields
    | HeaderV (valid, fields) ->
        if valid then
          List.fold_left (fun pkt (_, value) -> deparse pkt value) pkt fields
        else pkt
    | SEnumFieldV (_, _, value) -> deparse pkt value
    | _ ->
        Format.asprintf "Cannot deparse value %a to packet" Value.pp value
        |> failwith

  (* Write @hdr into the output packet, advancing cursor.
     @T can be a header type, a header stack, a header_union, or a struct
     containing fields with such types.
     void emit<T>(in T hdr); *)
  let emit (ctx : Ctx.t) (pkt : t) =
    let _, header = Ctx.find_var "hdr" ctx |> Option.get in
    let pkt = deparse pkt header in
    (ctx, pkt)
end
