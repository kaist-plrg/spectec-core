open Runtime
open Runtime.Scope

(* Corresponds to extern packet_in { ... } *)
module Packet = struct
  let data = ref (Array.init 0 (fun _ -> false))
  let idx = ref 0
  let len = ref 0

  let init bits =
    data := bits;
    idx := 0;
    len := Array.length bits

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

  let parse (size : int) =
    let bits = Array.sub !data !idx size in
    idx := !idx + size;
    bits

  let rec sizeof (bscope : bscope) (typ : Type.t) =
    match typ with
    | TBool -> 1
    | TInt { width; _ }
    | TBit { width; _ } -> Bigint.to_int width |> Option.get
    | THeader { entries; _ } ->
        List.fold_left
          (fun acc (_, typ) -> acc + sizeof bscope typ)
          0 entries
    | TName _
    | TNewType _ ->
        sizeof bscope (Eval.eval_simplify_typ bscope typ)
    | _ -> assert false

  let rec write (parsed_data : bool Array.t) (value : Value.t) =
    match value with
    | VBool _ ->
        let bit = Array.get parsed_data 0 in
        let parsed_data = Array.sub parsed_data 1 (Array.length parsed_data - 1) in
        let value = Value.VBool bit in
        (parsed_data, value)
    | VInt { width; _ } ->
        let size = Bigint.to_int width |> Option.get in
        let bits = Array.sub parsed_data 0 size in
        let parsed_data = Array.sub parsed_data size (Array.length parsed_data - size) in
        let value = Value.VInt { value = bits_to_int bits; width } in
        (parsed_data, value)
    | VBit { width; _ } ->
        let size = Bigint.to_int width |> Option.get in
        let bits = Array.sub parsed_data 0 size in
        let parsed_data = Array.sub parsed_data size (Array.length parsed_data - size) in
        let value = Value.VBit { value = bits_to_int bits; width } in
        (parsed_data, value)
    | VStruct { entries; _ } ->
        let parsed_data, entries =
          List.fold_left
            (fun (parsed_data, entries) (key, value) ->
              let parsed_data, value = write parsed_data value in
              (parsed_data, (key, value) :: entries))
            (parsed_data, [])
            entries
        in
        (parsed_data, Value.VStruct { entries })
    | VHeader { entries; _ } ->
        let parsed_data, entries =
          List.fold_left
            (fun (parsed_data, entries) (key, value) ->
              let parsed_data, value = write parsed_data value in
              (parsed_data, (key, value) :: entries))
            (parsed_data, [])
            entries
        in
        (parsed_data, Value.VHeader { valid = true; entries })
    | _ ->
      Format.asprintf "Cannot write value %a to packet" Value.pp value
      |> failwith

  (* Corresponds to void extract<T>(out T hdr); *)
  let extract (bscope : bscope) =
    let theader, vheader = find_var "hdr" bscope in
    let size = sizeof bscope theader in
    let parsed_data = parse size in
    let _, vheader = write parsed_data vheader in
    let bscope = update_value "hdr" vheader bscope in
    bscope
end

(* Entry point for builtin functions *)

let eval_builtin (bscope : bscope) (mthd : string) =
  match mthd with
  | "extract" -> Packet.extract bscope
  | _ -> "Unknown builtin method " ^ mthd |> failwith
