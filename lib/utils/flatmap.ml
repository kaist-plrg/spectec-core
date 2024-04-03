(* Flat map *)

module FlatMap (K : sig
  type t

  val print : t -> string
  val compare : t -> t -> int
end) (V : sig
  type t

  val print : t -> string
end) =
struct
  type key = K.t

  module KMap = Map.Make (K)

  type value = V.t
  type t = value KMap.t

  let empty = KMap.empty

  let find key env =
    match KMap.find_opt key env with
    | Some value -> value
    | None -> Printf.sprintf "Key %s not found" (K.print key) |> failwith

  let add key value env = KMap.add key value env

  let update key value env =
    match KMap.find_opt key env with
    | Some _ -> KMap.add key value env
    | None -> Printf.sprintf "Key %s not found" (K.print key) |> failwith

  let print (env : t) =
    let print_binding key value acc =
      acc @ [ Printf.sprintf "%s: %s" (K.print key) (V.print value) ]
    in
    let bindings = KMap.fold print_binding env [] in
    String.concat "\n" bindings
end
