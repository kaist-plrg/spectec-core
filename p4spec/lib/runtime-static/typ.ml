open Il.Ast
open Il.Print

(* Type with dimension *)

type t = typ * iter list

let to_string (typ, iters) =
  string_of_typ typ ^ String.concat "" (List.map string_of_iter iters)

let compare (_typ_a, iters_a) (_typ_b, iters_b) = compare iters_a iters_b

(* (TODO) Also check that types are equivalent *)
let equiv (_typ_a, iters_a) (_typ_b, iters_b) =
  List.length iters_a = List.length iters_b
  && List.for_all2 ( = ) iters_a iters_b

(* (TODO) Also check that types are equivalent *)
let sub (_typ_a, iters_a) (_typ_b, iters_b) =
  List.length iters_a <= List.length iters_b
  && List.for_all2 ( = ) iters_a
       (List.filteri (fun idx _ -> idx < List.length iters_a) iters_b)

let add_iter (iter : iter) (typ, iters) = (typ, iters @ [ iter ])
