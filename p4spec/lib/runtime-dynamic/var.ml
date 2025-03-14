open Domain.Lib
module Dim = Runtime_static.Dim

type t = Id.t * Dim.t

let to_string (id, dim) = Id.to_string id ^ Dim.to_string dim

let compare (id_a, dim_a) (id_b, dim_b) =
  let cmp_id = Id.compare id_a id_b in
  if cmp_id = 0 then Dim.compare dim_a dim_b else cmp_id
