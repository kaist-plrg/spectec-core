(* Stacked set *)
(* Invariant:
    1. the stack is non-empty
    2. the first element of the list is the most recent scope *)

module StackSet (K : sig
  type t

  val print : t -> string
  val compare : t -> t -> int
end) =
struct
  type key = K.t

  module KSet = Set.Make (K)

  type t = KSet.t list

  let empty = [ KSet.empty ]

  let current env =
    assert (List.length env > 0);
    (List.hd env, List.tl env)

  let enter env = KSet.empty :: env

  let exit env =
    assert (List.length env > 0);
    List.tl env

  let find key env =
    let rec find' env =
      match env with
      | [] -> None
      | now :: old -> (
          match KSet.find_opt key now with
          | Some key -> Some key 
          | None -> find' old)
    in
    match find' env with
    | Some key -> key 
    | None -> Printf.sprintf "Key %s not found" (K.print key) |> failwith

  let find_toplevel key env =
    let top = List.rev env |> List.hd in
    match KSet.find_opt key top with
    | Some key -> key
    | None ->
        Printf.sprintf "Key %s not found in top scope" (K.print key) |> failwith

  let add key env =
    let now, old = current env in
    let now = KSet.add key now in
    now :: old

  let print ?(indent = 0) (env : t) =
    let print_binding key acc =
      acc @ [ K.print key ]
    in
    let print' acc env =
      let bindings = KSet.fold print_binding env [] in
      let bindings = String.concat ", " bindings in
      acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
    in
    let senv = List.fold_left print' [] env in
    String.concat "\n" senv
end
