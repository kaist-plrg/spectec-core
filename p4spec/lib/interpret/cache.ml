open Il.Ast

module HashedFunc = struct
    type t = string * value list
    
    let equal (id_a, values_a) (id_b, values_b) = 
      id_a = id_b 
      && List.compare_lengths values_a values_b = 0
      && List.equal (fun v_a v_b -> Runtime_dynamic.Value.compare v_a v_b = 0) values_a values_b

    let hash = Hashtbl.hash
  end

module Cache = Hashtbl.Make (HashedFunc)


