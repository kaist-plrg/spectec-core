module Value = Runtime_static.Value
module Types = Runtime_static.Tdomain.Types
module Type = Types.Type

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

let bits_to_int bits =
  Array.fold_left (fun i bit -> (i lsl 1) + if bit then 1 else 0) 0 bits
  |> Bigint.of_int

let int_to_bits value size =
  Array.init size (fun i -> Bigint.(value land (one lsl i) > zero))
  |> Array.to_list |> List.rev |> Array.of_list

let rec sizeof (ctx : Ctx.t) (typ : Type.t) : int =
  let typ = Ctx.resolve_typ Ctx.Local typ ctx |> Type.canon in
  match typ with
  | DefT _ | SpecT _ -> assert false
  | BoolT -> 1
  | FIntT width | FBitT width -> width |> Bigint.to_int |> Option.get
  | HeaderT (_, fields) ->
      fields
      |> List.map (fun (_, typ) -> sizeof ctx typ)
      |> List.fold_left ( + ) 0
  | _ -> Format.asprintf "(TODO: sizeof) %a" (Type.pp ~level:0) typ |> failwith

module PacketIn = struct
  type t = { bits : bits; idx : int; len : int }

  let pp fmt pkt = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let init (pkt : string) =
    let bits = string_to_bits pkt in
    { bits; idx = 0; len = Array.length bits }

  let parse pkt (size : int) =
    let bits = Array.sub pkt.bits pkt.idx size in
    let pkt = { pkt with idx = pkt.idx + size } in
    (pkt, bits)

  let rec write (bits_in : bits) (value : Value.t) =
    match value with
    | FIntV (width, _) ->
        let size = width |> Bigint.to_int |> Option.get in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value = Value.FIntV (width, bits_to_int bits) in
        (bits_in, value)
    | FBitV (width, _) ->
        let size = width |> Bigint.to_int |> Option.get in
        let bits = Array.sub bits_in 0 size in
        let bits_in = Array.sub bits_in size (Array.length bits_in - size) in
        let value = Value.FBitV (width, bits_to_int bits) in
        (bits_in, value)
    | HeaderV (_, fields) ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (member, value) ->
              let bits_in, value = write bits_in value in
              (bits_in, fields @ [ (member, value) ]))
            (bits_in, []) fields
        in
        let value = Value.HeaderV (true, fields) in
        (bits_in, value)
    | _ ->
        Format.asprintf "(TODO: write) %a" (Value.pp ~level:0) value |> failwith

  (* Read a header from the packet into a fixed-sized header @hdr and advance the cursor.
     May trigger error PacketTooShort or StackOutOfBounds.
     @T must be a fixed-size header type
     void extract<T>(out T hdr); *)
  let extract (ctx : Ctx.t) pkt : Ctx.t * t =
    let typ = Ctx.find_typ Ctx.Local "T" ctx in
    let hdr = Ctx.find_value Ctx.Local "hdr" ctx in
    let pkt, bits = sizeof ctx typ |> parse pkt in
    let hdr = write bits hdr |> snd in
    let ctx = Ctx.update_value Ctx.Local "hdr" hdr ctx in
    (ctx, pkt)

  let extract_varsize (_ctx : Ctx.t) _pkt = failwith "TODO"
  let lookahead (_ctx : Ctx.t) _pkt = failwith "TODO"
  let advance (_ctx : Ctx.t) _pkt = failwith "TODO"
  let length (_ctx : Ctx.t) _pkt = failwith "TODO"
end

module PacketOut = struct
  type t = { bits : bits }

  let pp fmt pkt = Format.fprintf fmt "%s" (bits_to_string pkt.bits)
  let init () = { bits = Array.make 0 false }

  let rec deparse pkt (value : Value.t) =
    match value with
    | BoolV b -> { bits = Array.append pkt.bits (Array.make 1 b) }
    | FIntV (width, value) | FBitV (width, value) ->
        let width = width |> Bigint.to_int |> Option.get in
        let bits = int_to_bits value width in
        { bits = Array.append pkt.bits bits }
    | HeaderV (_, fields) ->
        List.fold_left (fun pkt (_, value) -> deparse pkt value) pkt fields
    | _ ->
        Format.asprintf "(TODO: deparse) %a" (Value.pp ~level:0) value
        |> failwith

  (* Write @hdr into the output packet, advancing cursor.
     @T can be a header type, a header stack, a header_union, or a struct
     containing fields with such types.
     void emit<T>(in T hdr); *)
  let emit (ctx : Ctx.t) pkt =
    let hdr = Ctx.find_value Ctx.Local "hdr" ctx in
    let pkt = deparse pkt hdr in
    (ctx, pkt)
end
