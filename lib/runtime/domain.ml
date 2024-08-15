(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module IdMap = Map.Make (Id)

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

module FIdMap = Map.Make (FId)

(* Constructor identifiers *)

module CId = FId
module CIdMap = FIdMap

(* Environment functor *)

module MakeIdEnv (V : sig
  type t
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" Id.pp id |> failwith
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" FId.pp id |> failwith
end

module MakeCIdEnv = MakeFIdEnv
