module Var = struct
  type t = string

  let print (t : t) = t
  let compare = String.compare
end

module Path = struct
  type t = string list

  let print (t : t) = String.concat "." t
  let concat = print
  let compare = compare
end

module Env = Map.Make (Var)

module Loc = struct
  type t = int

  let print (t : t) = string_of_int t
  let compare = compare
  let inc = ( + ) 1
  let max (ts : t list) = List.fold_left max 0 ts
end

module Heap = struct
  include Map.Make (Loc)

  let fresh heap =
    let locs = bindings heap |> List.map fst in
    Loc.max locs |> Loc.inc
end
