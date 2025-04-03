open Il.Ast
open Xl

module VlistHash =
  struct
    type t = value list
    let equal values_a values_b = 
      List.compare_lengths values_a values_b = 0 && List.equal (=) values_a values_b
    let hash values = 
      let hash' value = 
        match value with
        | BoolV _ -> 1
        | NumV _ -> 2
        | TextV _ -> 3
        | StructV _ -> 4
        | CaseV _ -> 5
        | TupleV _ -> 6
        | OptV _ -> 7
        | ListV _ -> 8
        | FuncV _ -> 9
      in 
      List.fold_left (fun sum a -> sum * 10 + hash' a) 0 values
  end

module VlistOrdered = 
  struct
    type t = value list
    let compare values_a values_b =
      let cmp value_a value_b = 
        match value_a, value_b with
        | (BoolV a), (BoolV b) ->
          if a && b then 0 else
          if a then -1 else 1
        | (NumV a), (NumV b) -> Bigint.to_int_exn (Num.to_int b) - Bigint.to_int_exn (Num.to_int a)
        | (TextV a), (TextV b) -> String.compare a b
        | _ -> 0
      in List.compare cmp values_a values_b
  end

module ArgMap = 
  struct
    include Hashtbl.Make (VlistHash)
  end

module Cache =
  struct
    include Map.Make (String)

    let cache_enabled = function
      | "specialize_typdef" 
      | "free_typ"
      | "nestable_structt"
      | "subst_typdef_poly"
      | "canon_typ"
      | "subst_typ"
      | "is_nominal"
      | "nestable_structt_in_headert" -> true
      | "unions_set" -> false
      | _ -> false

    let add_arg id values_in value_out cache =
      let arg_map_opt = find_opt id cache in
      (match arg_map_opt with
       | Some arg_map -> 
         ArgMap.add arg_map values_in value_out;
         cache
       | None ->
         let arg_map = ArgMap.create 30 in
         ArgMap.add arg_map values_in value_out;
         cache |> add id arg_map)

    let find_arg_opt id values_in cache =
      let arg_map_opt = find_opt id cache in
      let val_opt = Option.bind arg_map_opt (fun arg_map -> ArgMap.find_opt arg_map values_in) in
      val_opt
  end
type cache = (value ArgMap.t) Cache.t 
