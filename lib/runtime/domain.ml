(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module IdMap = struct
  include Map.Make (Id)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a -> %a" Id.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings
end

(* Type identifiers *)

module TId = Id
module TIdMap = IdMap

(* Function identifiers *)

module FId = struct
  type t = string * string list

  let pp fmt (name, params) =
    Format.fprintf fmt "%s(%s)" name (String.concat ", " params)

  let compare = compare
end

module FIdMap = struct
  include Map.Make (FId)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a -> %a" FId.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings
end

(* Constructor identifiers *)

module CId = FId
module CIdMap = FIdMap

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" Id.pp id |> failwith

  let pp fmt env =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a -> %a" Id.pp k V.pp v in
    let bindings = bindings env in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" FId.pp id |> failwith

  let pp fmt env =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a -> %a" FId.pp k V.pp v in
    let bindings = bindings env in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings
end

module MakeCIdEnv = MakeFIdEnv
