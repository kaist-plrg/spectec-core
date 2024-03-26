open Utils

(* Type-alias environment *)
(* Invariant:
    1. the map is non-empty
    2. the first element of the list is the most recent scope *)

type t = Typ.t Var.VMap.t list

(* Printer *)

let print ?(indent = 0) (tdenv : t) =
  let print_binding var typ acc =
    acc @ [ Printf.sprintf "%s: %s" (Var.print var) (Typ.print typ) ]
  in
  let print' acc tdenv =
    let bindings = Var.VMap.fold print_binding tdenv [] in
    let bindings = String.concat ", " bindings in
    acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
  in
  let stdenv = List.fold_left print' [] tdenv in
  String.concat "\n" stdenv

(* Utils *)

let empty = [ Var.VMap.empty ]

let current (tdenv : t) =
  assert (List.length tdenv > 0);
  (List.hd tdenv, List.tl tdenv)

let enter (tdenv : t) = Var.VMap.empty :: tdenv

let exit (tdenv : t) =
  assert (List.length tdenv > 0);
  List.tl tdenv

let insert (var : Var.t) (typ : Typ.t) (tdenv : t) =
  let now, old = current tdenv in
  let now = Var.VMap.add var typ now in
  now :: old

let find (var : Var.t) (tdenv : t) =
  let rec find' tdenv =
    match tdenv with
    | [] -> None
    | now :: old -> (
        match Var.VMap.find_opt var now with
        | Some typ -> Some typ
        | None -> find' old)
  in
  match find' tdenv with
  | Some typ -> typ
  | None ->
      print tdenv |> print_endline;
      Printf.sprintf "Type name %s not found" (Var.print var) |> failwith

let find_toplevel (var : Var.t) (tdenv : t) =
  let top = List.rev tdenv |> List.hd in
  match Var.VMap.find_opt var top with
  | Some typ -> typ
  | None ->
      Printf.sprintf "Type name %s not found in top scope" (Var.print var)
      |> failwith
