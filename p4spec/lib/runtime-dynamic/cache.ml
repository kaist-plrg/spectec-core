(* Cache for relation and function invocations *)

module Entry = struct
  type t = string * Value.t list

  let equal (id_a, values_a) (id_b, values_b) =
    id_a = id_b
    && List.compare (fun v_a v_b -> Value.compare v_a v_b) values_a values_b = 0

  let hash = Hashtbl.hash
end

module Cache = Hashtbl.Make (Entry)

(* Cache targets *)

let is_cached_func = function
  | "subst_typ" | "subst_typdef_poly" | "specialize_typdef" | "canon_typ"
  | "free_typ" | "is_nominal" | "find_map" | "update_map" | "dom_map"
  | "bound_tids" | "in_set" | "merge_cstr'" | "merge_cstr"
  | "find_matching_funcs" | "nestable_structt" | "nestable_structt_in_headert"
    ->
      true
  | _ -> false

let is_cached_rule = function
  | "Sub_expl" | "Sub_expl_canon" | "Sub_expl_canon_neq" | "Sub_impl"
  | "Sub_impl_canon" | "Sub_impl_canon_neq" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false
