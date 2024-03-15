open Utils

(* Global store of objects *)
(* Instances should be distinguishable by their paths,
   or fully-qualified names, so a store can be a flat map *)

type t = Object.t Path.PMap.t

let empty = Path.PMap.empty

let insert (path : Path.t) (obj : Object.t) (store : t) =
  Path.PMap.add path obj store

let find (path : Path.t) (store : t) = Path.PMap.find path store

let print ?(indent = 0) (store : t) =
  let print_binding path obj acc =
    acc
    @ [
        Printf.sprintf "%s%s:\n%s%s"
          (Print.print_indent (indent + 1))
          (Path.print path)
          (Print.print_indent (indent + 1))
          (Object.print obj ~indent:(indent + 1));
      ]
  in
  let sstore = Path.PMap.fold print_binding store [] in
  "[\n" ^ String.concat "\n" sstore ^ " ]"
