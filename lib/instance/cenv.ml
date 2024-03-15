open Utils

(* Constructor closure environment *)

type t = (Cclosure.t Var.VMap.t) list

let empty = [ Var.VMap.empty ]

let current (cenv: t) =
  (assert ((List.length cenv) > 0));
  (List.hd cenv, List.tl cenv)

let enter (cenv: t) =
  Var.VMap.empty :: cenv

let exit (cenv: t) =
  (assert ((List.length cenv) > 0));
  List.tl cenv

let insert
  (var: Var.t) (cclos: Cclosure.t) (cenv: t) =
  let (now, old) = current cenv in
  let now = Var.VMap.add var cclos now in
  now :: old

let find
  (var: Var.t) (cenv: t) =
  let rec find' cenv =
    match cenv with
    | [] -> None
    | now :: old ->
      (match Var.VMap.find_opt var now with
      | Some value -> Some value
      | None -> find' old)
  in
  match find' cenv with
  | Some cclos -> cclos 
  | None ->
      Printf.sprintf
        "Variable %s not found" (Var.print var)
      |> failwith

let print (cenv: t) =
  let print_binding var cclos acc =
    Printf.sprintf "%s, %s -> %s"
      acc (Var.print var) (Cclosure.print cclos)
  in
  let rec print' cenv =
    match cenv with
    | [] -> ""
    | now :: old ->
        "[ " ^ (Var.VMap.fold print_binding now "") ^ " ] / "
        ^ (print' old)
  in
  print' cenv
