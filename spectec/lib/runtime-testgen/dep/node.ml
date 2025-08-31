open Xl
open Sl.Ast
open Util.Source

(* Mirror of the runtime values *)

type mirror = (mirror', typ') note

and mirror' =
  | BoolN of bool
  | NumN of Num.t
  | TextN of string
  | StructN of (atom * vid) list
  | CaseN of mixop * vid list
  | TupleN of vid list
  | OptN of vid option
  | ListN of vid list
  | FuncN of id

(* Taint represents whether they are from the source P4 program or not *)

type taint = Red | White

(* Node *)

type t = mirror * taint

(* Helpers *)

let mirror (mirror, _) = mirror
let taint (_, taint) = taint

(* Tainting *)

let is_source (taint : taint) : bool = taint = Red

(* Dot output *)

let dot_of_mirror (mirror : mirror) : string =
  match mirror.it with
  | BoolN b -> string_of_bool b
  | NumN n -> Num.string_of_num n
  | TextN s -> "\\\"" ^ s ^ "\\\""
  | StructN [] -> "{}"
  | StructN nodefields ->
      Format.asprintf "{ %s }"
        (String.concat "; "
           (List.map
              (fun (atom, vid) ->
                Format.asprintf "%s %s"
                  (Sl.Print.string_of_atom atom)
                  (Sl.Print.string_of_vid vid))
              nodefields))
  | CaseN (mixop, vids) ->
      let atoms_h, mixop_t = (List.hd mixop, List.tl mixop) in
      Format.asprintf "(%s%s)"
        (Il.Print.string_of_atoms atoms_h)
        (String.concat ""
           (List.map2
              (fun vid atoms ->
                Format.asprintf "%s%s"
                  (Sl.Print.string_of_vid vid)
                  (Sl.Print.string_of_atoms atoms))
              vids mixop_t))
  | TupleN vids ->
      Format.asprintf "(%s)"
        (String.concat ", " (vids |> List.map Sl.Print.string_of_vid))
  | OptN (Some vid) -> Format.asprintf "Some(%s)" (Sl.Print.string_of_vid vid)
  | OptN None -> "None"
  | ListN [] -> "[]"
  | ListN vids ->
      Format.asprintf "[ %s ]"
        (String.concat ", " (List.map Sl.Print.string_of_vid vids))
  | FuncN id -> Sl.Print.string_of_defid id

let dot_of_taint (taint : taint) : string =
  Format.asprintf "style=filled, fillcolor=%s"
    (match taint with Red -> "red" | White -> "white")

let dot_of_node (vid : vid) ((mirror, taint) : t) : string =
  Format.asprintf "  %d [label=\"%s (%s)\", %s];" vid
    (Sl.Print.string_of_vid vid)
    (dot_of_mirror mirror) (dot_of_taint taint)
