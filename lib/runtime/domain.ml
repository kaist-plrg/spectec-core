open Syntax.Ast
open Util.Source

(* Domain of variables, type variables, and objects *)

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

  let to_fid (id : id') (params : param' list) =
    let params = List.map (fun (id, _, _, _) -> id.it) params in
    (id, params)
end

module Path = struct
  type t = string list

  let concat path = String.concat "." path
  let pp fmt path = Format.fprintf fmt "%s" (concat path)
  let compare = compare
end

(* (Monomorphic) Functors of sets and maps *)

module type KEY = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type VALUE = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type VIS = sig
  type t_key
  type t

  val empty : t
  val find : t_key -> t -> t_key option
  val add : t_key -> t -> t -> t
  val mem : t_key -> t -> bool
  val union : t -> t -> t
  val of_list : t_key list -> t
  val pp : Format.formatter -> t -> unit
end

module MakeVis (K : KEY) = struct
  module KS = Set.Make (K)

  type t_key = K.t
  type t = KS.t

  let empty = KS.empty
  let find = KS.find_opt
  let add = KS.add
  let mem = KS.mem
  let union = KS.union
  let of_list = KS.of_list

  let pp fmt sp =
    let elements = KS.elements sp in
    Format.fprintf fmt "{@[<hv>%a@]}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         K.pp)
      elements
end

module type ENV = sig
  type t_key
  type t_value
  type t

  val empty : t
  val bindings : t -> (t_key * t_value) list
  val iter : (t_key -> t_value -> unit) -> t -> unit
  val add : t_key -> t_value -> t -> t
  val map : (t_value -> t_value) -> t -> t
  val fold : (t_key -> t_value -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (t_key -> t_value -> bool) -> t -> t
  val mem : t_key -> t -> bool
  val find_opt : t_key -> t -> t_value option
  val find : t_key -> t -> t_value
  val of_list : (t_key * t_value) list -> t
  val pp : Format.formatter -> t -> unit
end

module type FENV = sig
  include ENV with type t_key = FId.t

  val find_overloaded_opt : id' * arg' list -> t -> t_value option
  val find_overloaded : id' * arg' list -> t -> t_value
end

module MakeEnv (K : KEY) (V : VALUE) = struct
  module KM = Map.Make (K)

  type t_key = K.t
  type t_value = V.t
  type t = V.t KM.t

  let empty = KM.empty
  let bindings = KM.bindings
  let iter = KM.iter
  let add = KM.add
  let map = KM.map
  let fold = KM.fold
  let filter = KM.filter
  let mem = KM.mem
  let find_opt = KM.find_opt

  let find k m =
    match find_opt k m with
    | Some v -> v
    | None -> Format.asprintf "Key not found: %a@." K.pp k |> failwith

  let of_list (l : (t_key * t_value) list) =
    List.fold_left (fun env (k, v) -> add k v env) empty l

  let pp fmt env =
    let bindings = KM.bindings env in
    Format.fprintf fmt "{ @[<hv>%a@] }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt (var, value) ->
           Format.fprintf fmt "%a:%a" K.pp var V.pp value))
      bindings
end
