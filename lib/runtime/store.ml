open Utils

(* Global store of objects *)
(* Instances should be distinguishable by their paths,
   or fully-qualified names, so a store can be a flat map *)

type t = Object.t Path.PMap.t

let empty = Path.PMap.empty

let insert
  (path: Path.t) (obj: Object.t) (store: t) =
  Path.PMap.add path obj store

let find
  (path: Path.t) (store: t) =
  Path.PMap.find path store

let print (store: t) =
  let print_binding path obj acc =
    Printf.sprintf "%s\n  %s -> %s"
      acc (String.concat "." path) (Object.print_object obj)
  in
  "[" ^ (Path.PMap.fold print_binding store "") ^ "\n]"
