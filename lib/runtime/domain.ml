open Syntax.Ast
open Util.Source

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module TId = Id

module FId = struct
  type t = string * string list

  let pp fmt (name, params) =
    Format.fprintf fmt "%s(%s)" name (String.concat ", " params)

  let compare = compare

  let to_fid (id : id) (params : param list) =
    let params = List.map (fun { it = id, _, _, _; _ } -> id.it) params in
    (id.it, params)
end

module Path = struct
  type t = string list

  let concat path = String.concat "." path
  let pp fmt path = Format.fprintf fmt "%s" (concat path)
  let compare = compare
end

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
  let union = KS.union

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
  let bindings = KM.bindings
  let add = KM.add
  let map = KM.map
  let fold = KM.fold
  let filter = KM.filter
  let find_opt = KM.find_opt

  let find k m =
    match find_opt k m with
    | Some v -> v
    | None -> Format.asprintf "Key not found: %a@." K.pp k |> failwith

  let pp fmt env =
    let bindings = KM.bindings env in
    Format.fprintf fmt "{ @[<hv>%a@] }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt (var, value) ->
           Format.fprintf fmt "%a:%a" K.pp var V.pp value))
      bindings
end
