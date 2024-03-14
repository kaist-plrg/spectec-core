open Utils

(* Constructor closure environment *)

type t = Cclosure.t Path.PMap.t

let empty = Path.PMap.empty

let insert
  (path: Path.t) (cclos: Cclosure.t) (cenv: t) =
  Path.PMap.add path cclos cenv

let find (path: Path.t) (cenv: t) =
  Path.PMap.find path cenv

let print (cenv: t) =
  Printf.sprintf "{ %s }"
    (String.concat "; "
      (List.map
        (fun (path, cclos) ->
          Printf.sprintf "%s -> %s" 
            (String.concat "." path) (Cclosure.print_cclos cclos))
        (Path.PMap.bindings cenv)))
