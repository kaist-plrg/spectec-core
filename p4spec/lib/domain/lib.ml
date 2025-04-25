open Util.Source

(* Value identifiers *)

module VId = struct
  type t = int

  let to_string t = string_of_int t
  let compare t_a t_b = compare t_a t_b
end

module VIdSet = struct
  include Set.Make (VId)

  let to_string ?(with_braces = true) s =
    let sset = String.concat ", " (List.map VId.to_string (elements s)) in
    if with_braces then "{ " ^ sset ^ " }" else sset

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module VIdMap = struct
  include Map.Make (VId)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> VIdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = VId.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Variable identifiers *)

module Id = struct
  type t = string phrase

  let to_string t = t.it
  let compare t_a t_b = compare t_a.it t_b.it
end

module IdSet = struct
  include Set.Make (Id)

  let to_string ?(with_braces = true) s =
    let sset = String.concat ", " (List.map Id.to_string (elements s)) in
    if with_braces then "{ " ^ sset ^ " }" else sset

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let dom m = m |> keys |> IdSet.of_list
  let values m = List.map snd (bindings m)

  let to_string ?(with_braces = true) ?(bind = " : ")
      (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = Id.to_string k ^ bind ^ to_string_v v in
    let bindings = bindings m in
    let smap = String.concat ", " (List.map to_string_binding bindings) in
    if with_braces then "{ " ^ smap ^ " }" else smap

  let extend env_a env_b =
    List.fold_left (fun env (k, v) -> add k v env) env_a (bindings env_b)

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap

(* Relation identifiers *)

module RId = Id
module RIdSet = IdSet
module RIdMap = IdMap

(* Function identifiers *)

module FId = Id
module FIdSet = IdSet
module FIdMap = IdMap

(* Phantom identifiers *)

module PId = VId
module PIdSet = VIdSet
module PIdMap = VIdMap

(* Environment functor *)

module MakeVIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include VIdMap

  type t = V.t VIdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    VIdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakePIdEnv = MakeVIdEnv

module MakeIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let to_string ?(with_braces = true) ?(bind = " : ") env =
    IdMap.to_string ~with_braces ~bind V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false
end

module MakeTIdEnv = MakeIdEnv
module MakeRIdEnv = MakeIdEnv
module MakeFIdEnv = MakeIdEnv
