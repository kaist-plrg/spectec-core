open Il.Ast

module HashedFunc = struct
    type t = string * value list
    let equal (id_a, values_a) (id_b, values_b) = 
      id_a = id_b 
      && List.compare_lengths values_a values_b = 0
      && List.equal (=) values_a values_b
    (*let hash (id, values) = 
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
      (Hashtbl.hash id) * 33 + List.fold_left (fun sum a -> sum * 10 + hash' a) 0 values*)
    let hash = Hashtbl.hash
  end

(*
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
*)

module Cache = Hashtbl.Make (HashedFunc)


