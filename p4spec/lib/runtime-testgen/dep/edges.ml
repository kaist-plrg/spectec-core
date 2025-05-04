open Sl.Ast

(* Label for categorizing dependency *)

type rel = id * int
type func = id * int

type op =
  | UnOp of unop
  | BinOp of binop
  | CmpOp of cmpop
  | CastOp of typ
  | DownCastOp
  | SubOp of typ
  | MatchOp of pattern
  | CatOp
  | MemOp
  | LenOp
  | UpdOp

type label =
  | Narrow
  | Expand
  | Control
  | Assign
  | Iter
  | Rel of rel
  | Func of func
  | Op of op

(* Set of edges *)

module E = Hashtbl.Make (struct
  type t = label * vid

  let equal (label_a, vid_a) (label_b, vid_b) =
    label_a = label_b && vid_a = vid_b

  let hash (label, vid) = Hashtbl.hash (label, vid)
end)

type t = unit E.t

(* Dot output *)

let dot_of_func ((id_func, idx_arg) : func) : string =
  Sl.Print.string_of_defid id_func ^ "/" ^ string_of_int idx_arg
  |> String.escaped

let dot_of_rel ((id_rel, idx_arg) : rel) : string =
  Sl.Print.string_of_relid id_rel ^ "/" ^ string_of_int idx_arg
  |> String.escaped

let dot_of_op (op : op) : string =
  (match op with
  | UnOp unop -> Sl.Print.string_of_unop unop
  | BinOp binop -> Sl.Print.string_of_binop binop
  | CmpOp cmpop -> Sl.Print.string_of_cmpop cmpop
  | CastOp typ -> "cast" ^ Sl.Print.string_of_typ typ
  | DownCastOp -> "downcast"
  | SubOp typ -> "as " ^ Sl.Print.string_of_typ typ
  | MatchOp pattern -> "match " ^ Sl.Print.string_of_pattern pattern
  | CatOp -> "cat"
  | MemOp -> "mem"
  | LenOp -> "len"
  | UpdOp -> "upd")
  |> String.escaped

let dot_of_label (label : label) : string =
  match label with
  | Narrow -> "narrow"
  | Expand -> "expand"
  | Control -> "control"
  | Assign -> "assign"
  | Iter -> "iteration"
  | Rel rel -> dot_of_rel rel
  | Func func -> dot_of_func func
  | Op op -> dot_of_op op

let dot_of_edge (vid_from : vid) (label : label) (vid_to : vid) : string =
  Format.asprintf "  %d -> %d [label=\"%s\"];" vid_from vid_to
    (dot_of_label label)
