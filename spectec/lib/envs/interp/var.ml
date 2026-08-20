open Il
open Common.Source

type t = id * iter list

let to_string (id, iters) =
  Print.string_of_varid id
  ^ String.concat "" (List.map Print.string_of_iter iters)

let compare_iter (iter_a : iter) (iter_b : iter) =
  match (iter_a, iter_b) with
  | Opt, Opt | List, List -> 0
  | Opt, List -> -1
  | List, Opt -> 1

let compare (id_a, iters_a) (id_b, iters_b) =
  let cmp_id = String.compare id_a.it id_b.it in
  if cmp_id <> 0 then cmp_id else List.compare compare_iter iters_a iters_b
