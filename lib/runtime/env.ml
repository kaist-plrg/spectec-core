open Utils

(* Environment *)
(* Invariant:
    1. the map is non-empty
    2. the first element of the list is the most recent scope *)

type t = (Value.t Var.VMap.t) list

let empty = [ Var.VMap.empty ]

let current (env: t) =
  (assert ((List.length env) > 0));
  (List.hd env, List.tl env)

let enter (env: t) =
  Var.VMap.empty :: env

let exit (env: t) =
  (assert ((List.length env) > 0));
  List.tl env

let insert
  (var: Var.t) (value: Value.t) (env: t) =
  let (now, old) = current env in
  let now = Var.VMap.add var value now in
  now :: old

let find
  (var: Var.t) (env: t) =
  let rec find' env =
    match env with
    | [] -> None
    | now :: old ->
      (match Var.VMap.find_opt var now with
      | Some value -> Some value
      | None -> find' old)
  in
  match find' env with
  | Some value -> value
  | None ->
      Printf.sprintf
        "Variable %s not found" (Var.print var)
      |> failwith

let print (env: t) =
  let print_binding var value acc =
    Printf.sprintf "%s, %s -> %s"
      acc (Var.print var) (Value.print value)
  in
  let rec print' env =
    match env with
    | [] -> ""
    | now :: old ->
        "[ " ^ (Var.VMap.fold print_binding now "") ^ " ] / "
        ^ (print' old)
  in
  print' env
