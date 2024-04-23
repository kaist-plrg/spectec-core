module Env = Map.Make (Var)

module Heap = struct
  include Map.Make (Loc)

  let fresh heap =
    let locs = bindings heap |> List.map fst in
    Loc.max locs |> Loc.inc
end

let find_single var env heap =
  match Env.find_opt var env with
  | Some loc -> Heap.find_opt loc heap
  | None -> None

let find_double var env heap heap' =
  match Env.find_opt var env with
  | Some loc -> (
      match (Heap.find_opt loc heap, Heap.find_opt loc heap') with
      | Some v, Some v' -> Some (v, v')
      | _ -> None)
  | None -> None

let add_single var v env heap =
  let loc = Heap.fresh heap in
  let env = Env.add var loc env in
  let heap = Heap.add loc v heap in
  (env, heap)

let add_double var v v' env heap heap' =
  let loc = Heap.fresh heap in
  let env = Env.add var loc env in
  let heap = Heap.add loc v heap in
  let heap' = Heap.add loc v' heap' in
  (env, heap, heap')

let update var v env heap =
  let loc = Env.find var env in
  Heap.add loc v heap
