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

type taint = Red | Pink | White

(* Node *)

type t = mirror * taint

(* Helpers *)

let mirror (mirror, _) = mirror
let taint (_, taint) = taint

(* Tainting *)

let is_source (taint : taint) : bool = taint = Red
let is_interesting (taint : taint) : bool = taint = Red || taint = Pink

let init_taint ~(init : bool) (taints_pred : taint list) : taint =
  if init then Red
  else if List.length taints_pred = 0 then White
  else if List.for_all is_source taints_pred then Red
  else if List.exists is_interesting taints_pred then Pink
  else White

let update_taint (taint : taint) (taint_pred : taint) : taint =
  match (taint, taint_pred) with
  | Red, Red -> Red
  | Red, (Pink | White) -> Pink
  | Pink, (Red | Pink | White) -> Pink
  | White, (Red | Pink) -> Pink
  | White, White -> White

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
    (match taint with Red -> "red" | Pink -> "pink" | White -> "white")

let dot_of_node (vid : vid) ((mirror, taint) : t) : string =
  Format.asprintf "  %d [label=\"%s (%s)\", %s];" vid
    (Sl.Print.string_of_vid vid)
    (dot_of_mirror mirror) (dot_of_taint taint)
