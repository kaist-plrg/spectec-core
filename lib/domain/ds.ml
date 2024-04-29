module Var = struct
  type t = string

  let pp fmt var = Format.fprintf fmt "%s" var
  let compare = String.compare
end

module Path = struct
  type t = string list

  let concat path = String.concat "." path
  let pp fmt path = Format.fprintf fmt "%s" (concat path)
  let compare = compare
end

module Addr = struct
  type t = int

  let pp fmt addr = Format.fprintf fmt "%d" addr
  let compare = compare
end

module Env (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module E = Map.Make (Var)

  type t = V.t E.t

  let empty = E.empty
  let find = E.find
  let add = E.add

  let pp fmt env =
    let bindings =
      E.bindings env
      |> List.fold_left
           (fun acc (var, value) ->
             acc @ [ Format.asprintf "%a : %a" Var.pp var V.pp value ])
           []
    in
    Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
end

module Heap (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module H = Map.Make (Addr)

  type t = V.t H.t

  let empty = H.empty
  let find = H.find
  let add = H.add

  let fresh heap =
    let addrs = H.bindings heap |> List.map fst in
    List.fold_left max 0 addrs |> ( + ) 1

  let pp fmt heap =
    let bindings =
      H.bindings heap
      |> List.sort (fun (addr, _) (addr', _) -> Addr.compare addr addr')
      |> List.fold_left
           (fun acc (addr, value) ->
             acc @ [ Format.asprintf "%a : %a" Addr.pp addr V.pp value ])
           []
    in
    Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
end
