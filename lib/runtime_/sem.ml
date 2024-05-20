module Var = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = String.compare
end

module Path = struct
  type t = string list

  let concat path = String.concat "." path
  let pp fmt path = Format.fprintf fmt "%s" (concat path)
  let compare = compare
end

module MakeScope = struct
  module S = Set.Make (Var)

  type t = S.t

  let empty = S.empty
  let find = S.find_opt
  let add = S.add

  let pp fmt sp =
    let elements = S.elements sp in
    Format.fprintf fmt "{@[<hv>%a@]}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         Var.pp)
      elements
end

module MakeEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module E = Map.Make (Var)

  type t = V.t E.t

  let empty = E.empty
  let find = E.find_opt
  let add = E.add

  let pp fmt env =
    let bindings = E.bindings env in
    Format.fprintf fmt "{@[<hv>%a@]}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt (var, value) ->
           Format.fprintf fmt "%a = %a" Var.pp var V.pp value))
      bindings
end
