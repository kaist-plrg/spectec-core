open Syntax
open Ast
open Utils

(* Constructor closure environment *)

type t = Cclosure.t Var.VMap.t list

let empty = [ Var.VMap.empty ]

let current (ccenv : t) =
  assert (List.length ccenv > 0);
  (List.hd ccenv, List.tl ccenv)

let enter (ccenv : t) = Var.VMap.empty :: ccenv

let exit (ccenv : t) =
  assert (List.length ccenv > 0);
  List.tl ccenv

let insert (var : Var.t) (cclos : Cclosure.t) (ccenv : t) =
  let now, old = current ccenv in
  let now = Var.VMap.add var cclos now in
  now :: old

let find (var : Var.t) (ccenv : t) =
  let rec find' ccenv =
    match ccenv with
    | [] -> None
    | now :: old -> (
        match Var.VMap.find_opt var now with
        | Some value -> Some value
        | None -> find' old)
  in
  match find' ccenv with
  | Some cclos -> cclos
  | None ->
      Printf.sprintf "Constructor closure %s not found" (Var.print var)
      |> failwith

let find_toplevel (var : Var.t) (ccenv : t) =
  let top = List.rev ccenv |> List.hd in
  match Var.VMap.find_opt var top with
  | Some value -> value
  | None ->
      Printf.sprintf "Constructor closure %s not found in top scope"
        (Var.print var)
      |> failwith

let rec find_from_type (typ : Type.t) (ccenv : t) =
  match typ with
  | Type.TypeName { name = Name.BareName text; _ } -> (find text.str ccenv, [])
  | Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      (find_toplevel text.str ccenv, [])
  | Type.SpecializedType { base; args; _ } -> 
      let cclos, _ = find_from_type base ccenv in
      (cclos, args)
  | _ ->
      Printf.sprintf "Constructor closure %s not found" (Pretty.print_type typ)
      |> failwith

(* Printer *)

let print ?(indent = 0) (ccenv : t) =
  let print_binding var cclos acc =
    acc @ [ Printf.sprintf "%s: %s" (Var.print var) (Cclosure.print cclos) ]
  in
  let print' acc ccenv =
    let bindings = Var.VMap.fold print_binding ccenv [] in
    let bindings = String.concat ", " bindings in
    acc @ [ Printf.sprintf "%s[ %s ]" (Print.print_indent indent) bindings ]
  in
  let sccenv = List.fold_left print' [] ccenv in
  String.concat "\n" sccenv
