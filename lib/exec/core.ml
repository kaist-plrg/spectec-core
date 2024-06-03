open Runtime.Base
open Runtime.Context
open Runtime.Signal

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

  let rec sizeof (ctx : Ctx.t) (typ : Type.t) =
    match typ with
    | BoolT -> 1
    | IntT width | BitT width -> Bigint.to_int width |> Option.get
    | HeaderT fields ->
        List.fold_left (fun acc (_, typ) -> acc + sizeof ctx typ) 0 fields
    | NameT _ | NewT _ -> Eval.eval_simplify_type ctx typ |> sizeof ctx
    | _ -> assert false

  let rec write (parsed_data : bool Array.t) (value : Value.t) =
    match value with
    | BoolV _ ->
        let bit = Array.get parsed_data 0 in
        let parsed_data =
          Array.sub parsed_data 1 (Array.length parsed_data - 1)
        in
        let value = Value.BoolV bit in
        (parsed_data, value)
    | IntV (width, _) ->
        let size = Bigint.to_int width |> Option.get in
        let bits = Array.sub parsed_data 0 size in
        let parsed_data =
          Array.sub parsed_data size (Array.length parsed_data - size)
        in
        let value = Value.IntV (width, bits_to_int bits) in
        (parsed_data, value)
    | BitV (width, _) ->
        let size = Bigint.to_int width |> Option.get in
        let bits = Array.sub parsed_data 0 size in
        let parsed_data =
          Array.sub parsed_data size (Array.length parsed_data - size)
        in
        let value = Value.BitV (width, bits_to_int bits) in
        (parsed_data, value)
    | StructV fields ->
        let parsed_data, fields =
          List.fold_left
            (fun (parsed_data, entries) (key, value) ->
              let parsed_data, value = write parsed_data value in
              (parsed_data, (key, value) :: entries))
            (parsed_data, []) fields
        in
        (parsed_data, Value.StructV fields)
    | HeaderV (_, fields) ->
        let parsed_data, fields =
          List.fold_left
            (fun (parsed_data, entries) (key, value) ->
              let parsed_data, value = write parsed_data value in
              (parsed_data, (key, value) :: entries))
            (parsed_data, []) fields
        in
        (parsed_data, Value.HeaderV (true, fields))
    | _ ->
        Format.asprintf "Cannot write value %a to packet" Value.pp value
        |> failwith

  (* Corresponds to void extract<T>(out T hdr); *)
  let extract (ctx : Ctx.t) =
    let typ, header = Ctx.find_var "hdr" ctx |> Option.get in
    let size = sizeof ctx typ in
    let parsed_data = parse size in
    let _, header = write parsed_data header in
    Ctx.update_var "hdr" typ header ctx
end

(* Entry point for builtin functions *)

let interp_builtin (sign : Sig.t) (ctx : Ctx.t) (mthd : string) =
  match sign with
  | Ret _ | Exit -> (sign, ctx)
  | Cont -> (
      match mthd with
      | "extract" ->
          let ctx = Packet.extract ctx in
          (sign, ctx)
      | _ ->
          Format.eprintf "Unknown builtin method %s." mthd;
          assert false)
