open Sl.Ast
open Sl.Print
open Util.Source

type t = id * iter list

let to_string (id, iters) =
  string_of_varid id ^ String.concat "" (List.map string_of_iter iters)

let compare (id_a, iters_a) (id_b, iters_b) =
  let cmp_id = compare id_a.it id_b.it in
  if cmp_id = 0 then compare iters_a iters_b else cmp_id
