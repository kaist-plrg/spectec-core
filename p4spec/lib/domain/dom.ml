(* Variable identifiers *)

module Id = struct
  type t = string

  let to_string t = t
  let compare = compare
end

module IdSet = struct
  include Set.Make (Id)

  let to_string s =
    "{ " ^ String.concat ", " (List.map Id.to_string (elements s)) ^ " }"

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  type 'v to_string_v = 'v -> string

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let to_string (to_string_v : 'v to_string_v) m =
    let to_string_binding (k, v) = Id.to_string k ^ " : " ^ to_string_v v in
    let bindings = bindings m in
    "{ " ^ String.concat ", " (List.map to_string_binding bindings) ^ " }"

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

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let to_string env = IdMap.to_string V.to_string env

  let find id env =
    match find_opt id env with Some value -> value | None -> assert false

  let add_nodup id value env =
    if mem id env then assert false else add id value env

  let extend_nodup env_a env_b =
    List.fold_left (fun env (k, v) -> add_nodup k v env) env_a (bindings env_b)
end

module MakeTIdEnv = MakeIdEnv
module MakeRIdEnv = MakeIdEnv
