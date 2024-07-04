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

let rec sizeof (ctx : Ctx.t) (typ : Type.t) =
  match typ with
  | BoolT -> 1
  | IntT width | BitT width -> Bigint.to_int width |> Option.get
  | HeaderT fields ->
      List.fold_left (fun acc (_, typ) -> acc + sizeof ctx typ) 0 fields
  | NameT _ | NewT _ -> Eval.eval_simplify_type ctx typ |> sizeof ctx
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

  let rec write (bits_in : bool Array.t) (value : Value.t) =
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
    | StructV fields ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (key, value) ->
              let bits_in, value = write bits_in value in
              (bits_in, fields @ [ (key, value) ]))
            (bits_in, []) fields
        in
        (bits_in, Value.StructV fields)
    | HeaderV (_, fields) ->
        let bits_in, fields =
          List.fold_left
            (fun (bits_in, fields) (key, value) ->
              let bits_in, value = write bits_in value in
              (bits_in, fields @ [ (key, value) ]))
            (bits_in, []) fields
        in
        (bits_in, Value.HeaderV (true, fields))
    | _ ->
        Format.asprintf "Cannot write value %a to packet" Value.pp value
        |> failwith

  (* Corresponds to void extract<T>(out T hdr); *)
  let extract (ctx : Ctx.t) (pkt : t) =
    let typ, header = Ctx.find_var "hdr" ctx |> Option.get in
    let pkt, bits = sizeof ctx typ |> parse pkt in
    let _, header = write bits header in
    let ctx = Ctx.update_var "hdr" typ header ctx in
    (ctx, pkt)
end

module PacketOut = struct
  type t = { bits : bool Array.t }

  let init = { bits = [||] }
  let pp fmt (pkt : t) = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let rec deparse (pkt : t) (value : Value.t) =
    match value with
    | BoolV b -> { bits = Array.append pkt.bits (Array.make 1 b) }
    | IntV (width, value) ->
        let size = Bigint.to_int width |> Option.get in
        let bits = int_to_bits value size in
        { bits = Array.append pkt.bits bits }
    | BitV (width, value) ->
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
    | _ ->
        Format.asprintf "Cannot deparse value %a to packet" Value.pp value
        |> failwith

  (* Corresponds to void emit<T>(in T hdr); *)
  let emit (ctx : Ctx.t) (pkt : t) =
    let _, header = Ctx.find_var "hdr" ctx |> Option.get in
    let pkt = deparse pkt header in
    (ctx, pkt)
end
