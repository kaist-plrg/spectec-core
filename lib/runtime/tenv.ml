open Utils

(* Type environment *)
(* Invariant:
    1. the map is non-empty
    2. the first element of the list is the most recent scope *)

type t = Typ.t Var.VMap.t list

let empty = [ Var.VMap.empty ]

let current (tenv : t) =
  assert (List.length tenv > 0);
  (List.hd tenv, List.tl tenv)

let enter (tenv : t) = Var.VMap.empty :: tenv

let exit (tenv : t) =
  assert (List.length tenv > 0);
  List.tl tenv

let insert (var : Var.t) (typ : Typ.t) (tenv : t) =
  let now, old = current tenv in
  let now = Var.VMap.add var typ now in
  now :: old

let find (var : Var.t) (tenv : t) =
  let rec find' tenv =
    match tenv with
    | [] -> None
    | now :: old -> (
        match Var.VMap.find_opt var now with
        | Some typ -> Some typ
        | None -> find' old)
  in
  match find' tenv with
  | Some typ -> typ
  | None ->
      Printf.sprintf "Type variable %s not found" (Var.print var) |> failwith

let find_toplevel (var : Var.t) (tenv : t) =
  let top = List.rev tenv |> List.hd in
  match Var.VMap.find_opt var top with
  | Some typ -> typ
  | None ->
      Printf.sprintf "Type variable %s not found in top scope" (Var.print var)
      |> failwith

let print ?(indent = 0) (tenv : t) =
  let print_binding var typ acc =
    acc @ [ Printf.sprintf "%s: %s" (Var.print var) (Typ.print typ) ]
  in
  let print' acc tenv =
    let bindings = Var.VMap.fold print_binding tenv [] in
    let bindings = String.concat ", " bindings in
    acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
  in
  let stenv = List.fold_left print' [] tenv in
  String.concat "\n" stenv
