open Util.Source

(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module IdSet = struct
  include Set.Make (Id)

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" Id.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap

(* Function identifiers *)

module FId = struct
  type t = string * string list

  let pp fmt (name, params) =
    Format.fprintf fmt "%s(%s)" name (String.concat ", " params)

  let compare = compare

  let to_fid (id : El.Ast.id') (params : El.Ast.param' list) =
    let params = List.map (fun (id, _, _, _, _) -> id.it) params in
    (id, params)
end

module FIdSet = struct
  include Set.Make (FId)

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module FIdMap = struct
  include Map.Make (FId)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" FId.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Constructor identifiers *)

module CId = FId
module CIdSet = FIdSet
module CIdMap = FIdMap

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let pp fmt env = IdMap.pp V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" Id.pp id |> failwith
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let pp fmt env = FIdMap.pp V.pp fmt env

  (* (TODO) resolve overloaded functions with argument names *)
  let find_opt (fid, args) fenv =
    let arity = List.length args in
    let funcs =
      List.filter
        (fun ((fid', params), _) -> fid = fid' && arity = List.length params)
        (bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)

  let find (fid, args) env =
    match find_opt (fid, args) env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %s\n" fid |> failwith
end

module MakeCIdEnv = MakeFIdEnv
