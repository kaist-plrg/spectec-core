type base =
  | Bool of bool
  | Integer of int
  | Bit of (int * int)
  | Int of (int * int)
  | String of string

type t =
  | Base of base
  | Ref of Path.t


(* Environment *)

and env = t Path.PMap.t

let empty_env = Path.PMap.empty

let insert_env
  (path: Path.t) (value: t) (env: env) =
  Path.PMap.add path value env

let find_env
  (path: Path.t) (env: env) =
  Path.PMap.find path env


(* Utils *)

let print_base_type (base: base) =
  match base with
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Integer i -> Printf.sprintf "Integer(%d)" i
  | Bit (w, v) -> Printf.sprintf "Bit(%d, %d)" w v
  | Int (w, v) -> Printf.sprintf "Int(%d, %d)" w v
  | String s -> Printf.sprintf "String(%s)" s

let print_type (t: t) =
  match t with
  | Base b -> print_base_type b
  | Ref p -> Printf.sprintf "Ref(%s)" (String.concat "." p)

let print_env (env: env) =
  "{ " ^
  (Path.PMap.bindings env
  |> List.map (fun (p, t) -> Printf.sprintf "%s -> %s" (String.concat "." p) (print_type t))
  |> String.concat ", ")
  ^ " }"
