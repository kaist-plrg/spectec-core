open Utils

(* Environment *)
(* Q. Should it be a list of lists, instead of a flat map,
   to account for scoping/shadowing? *)

type t = Value.t Path.PMap.t

let empty = Path.PMap.empty

let insert
  (path: Path.t) (value: Value.t) (env: t) =
  Path.PMap.add path value env

let find
  (path: Path.t) (env: t) =
  Path.PMap.find path env

let print (env: t) =
  "{ " ^
  (Path.PMap.bindings env
  |> List.map
      (fun (p, v) ->
        Printf.sprintf "%s -> %s"
          (String.concat "." p) (Value.print_value v))
  |> String.concat ", ")
  ^ " }"
