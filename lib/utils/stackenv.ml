(* Stacked Environment *)
(* Invariant:
    1. the stack is non-empty
    2. the first element of the list is the most recent scope *)

module StackEnv
  (K: sig type t val print : t -> string val compare : t -> t -> int end)
  (V: sig type t val print : t -> string end) = struct
  type key = K.t
  module KMap = Map.Make(K)
  type value = V.t
  type t = value KMap.t list

  let empty = [ KMap.empty ]

  let current env =
    assert (List.length env > 0);
    (List.hd env, List.tl env)

  let enter env = KMap.empty :: env

  let exit env =
    assert (List.length env > 0);
    List.tl env

  let find var env =
    let rec find' env =
      match env with
      | [] -> None
      | now :: old -> (
          match KMap.find_opt var now with
          | Some value -> Some value
          | None -> find' old)
    in
    match find' env with
    | Some value -> value
    | None -> Printf.sprintf "K %s not found" (K.print var) |> failwith

  let find_toplevel var env =
    let top = List.rev env |> List.hd in
    match KMap.find_opt var top with
    | Some value -> value
    | None ->
        Printf.sprintf "K %s not found in top scope" (K.print var)
        |> failwith

  let insert var value env =
    let now, old = current env in
    let now = KMap.add var value now in
    now :: old

  let update var value env =
    let rec update' env =
      match env with
      | [] -> Printf.sprintf "K %s not found" (K.print var) |> failwith
      | now :: old -> (
          match KMap.find_opt var now with
          | Some _ -> KMap.add var value now :: old
          | None -> now :: update' old)
    in
    update' env

  let update_toplevel var value env =
    let renv = List.rev env in
    let top, tail = (List.hd renv, List.tl renv) in
    match KMap.find_opt var top with
    | Some _ -> KMap.add var value top :: tail |> List.rev
    | None ->
        Printf.sprintf "K %s not found in top scope" (K.print var)
        |> failwith

  let print ?(indent = 0) (env : t) =
    let print_binding key value acc =
      acc @ [ Printf.sprintf "%s: %s" (K.print key) (V.print value) ]
    in
    let print' acc env =
      let bindings = KMap.fold print_binding env [] in
      let bindings = String.concat ", " bindings in
      acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
    in
    let senv = List.fold_left print' [] env in
    String.concat "\n" senv
end
