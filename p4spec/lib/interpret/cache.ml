open Il.Ast

module Entry = struct
  type t = string * value list

  let equal (id_a, values_a) (id_b, values_b) =
    id_a = id_b
    && List.compare
         (fun v_a v_b -> Runtime_dynamic.Value.compare v_a v_b)
         values_a values_b
       = 0

  let hash = Hashtbl.hash
end

module Cache = Hashtbl.Make (Entry)
