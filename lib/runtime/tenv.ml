open Syntax
open Ast
open Utils

(* Type environment *)
(* Invariant:
    1. the map is non-empty
    2. the first element of the list is the most recent scope *)

type t = Type.t Var.VMap.t list

let empty = [ Var.VMap.empty ]

let current (env : t) =
  assert (List.length env > 0);
  (List.hd env, List.tl env)

let enter (env : t) = Var.VMap.empty :: env

let exit (env : t) =
  assert (List.length env > 0);
  List.tl env

let insert (var : Var.t) (typ : Type.t) (env : t) =
  let now, old = current env in
  let now = Var.VMap.add var typ now in
  now :: old

let find (var : Var.t) (env : t) =
  let rec find' env =
    match env with
    | [] -> None
    | now :: old -> (
        match Var.VMap.find_opt var now with
        | Some typ -> Some typ
        | None -> find' old)
  in
  match find' env with
  | Some typ -> typ 
  | None -> Printf.sprintf "Type variable %s not found" (Var.print var) |> failwith

let print ?(indent = 0) (env : t) =
  let print_binding var typ acc =
    acc @ [ Printf.sprintf "%s: %s" (Var.print var) (Pretty.print_type typ) ]
  in
  let print' acc env =
    let bindings = Var.VMap.fold print_binding env [] in
    let bindings = String.concat ", " bindings in
    acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
  in
  let senv = List.fold_left print' [] env in
  String.concat "\n" senv
