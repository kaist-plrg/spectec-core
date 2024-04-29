module Var = struct
  type t = string

  let print (t : t) = t
  let compare = String.compare
end

module Path = struct
  type t = string list

  let print (t : t) = String.concat "." t
  let concat = print
  let compare = compare
end

module Addr = struct
  type t = int

  let print (t : t) = string_of_int t
  let compare = compare
  let inc = ( + ) 1
  let max (ts : t list) = List.fold_left max 0 ts
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
             acc @ [ Format.asprintf "%s : %a" (Var.print var) V.pp value ])
           []
    in
    Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
end

module AEnv = struct
  module AE = Map.Make (Var)
  type t = Addr.t AE.t

  let empty = AE.empty

  let find = AE.find

  let add = AE.add

  let pp fmt aenv =
    let bindings =
      AE.bindings aenv
      |> List.sort (fun (_, addr) (_, addr') -> Addr.compare addr addr')
      |> List.fold_left
           (fun acc (var, addr) ->
             acc @ [ Format.asprintf "%s : %s" (Var.print var) (Addr.print addr) ])
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
    Addr.max addrs |> Addr.inc

  let pp fmt heap =
    let bindings =
      H.bindings heap 
      |> List.sort (fun (addr, _) (addr', _) -> Addr.compare addr addr')
      |> List.fold_left
           (fun acc (addr, value) ->
             acc @ [ Format.asprintf "%s : %a" (Addr.print addr) V.pp value ])
           []
    in
    Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
end
