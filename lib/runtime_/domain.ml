module Var = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = String.compare
end

module VM = Map.Make (Var)
module VS = Set.Make (Var)

module Path = struct
  type t = string list

  let concat path = String.concat "." path
  let pp fmt path = Format.fprintf fmt "%s" (concat path)
  let compare = compare
end

module PM = Map.Make (Path)

module MakeVis (K : sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end) =
struct
  module KS = Set.Make (K)

  type t = KS.t

  let empty = KS.empty
  let find = KS.find_opt
  let add = KS.add
  let mem = KS.mem

  let pp fmt sp =
    let elements = KS.elements sp in
    Format.fprintf fmt "{@[<hv>%a@]}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         K.pp)
      elements
end

module MakeEnv (K : sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end) (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  module KM = Map.Make (K)

  type t = V.t KM.t

  let empty = KM.empty
  let find = KM.find_opt
  let add = KM.add
  let fold = KM.fold
  let filter = KM.filter

  let pp fmt env =
    let bindings = KM.bindings env in
    Format.fprintf fmt "{ @[<hv>%a@] }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt (var, value) ->
           Format.fprintf fmt "%a:%a" K.pp var V.pp value))
      bindings
end
