module F = Format

(* Separator *)

type sep = Nl | Comma | CommaNl | Semicolon | SemicolonNl

let is_nl = function Nl | CommaNl | SemicolonNl -> true | _ -> false

let pp_sep fmt = function
  | Nl -> F.fprintf fmt "\n"
  | Comma -> F.fprintf fmt ", "
  | CommaNl -> F.fprintf fmt ",\n"
  | Semicolon -> F.fprintf fmt "; "
  | SemicolonNl -> F.fprintf fmt ";\n"

(* Relations *)

type rel = Space | Colon | Eq

let pp_rel fmt = function
  | Space -> F.fprintf fmt " "
  | Colon -> F.fprintf fmt ": "
  | Eq -> F.fprintf fmt " = "

(* Printers *)

let indent level = String.make (2 * level) ' '
let pp_option pp_elem fmt = function Some x -> pp_elem fmt x | None -> ()

let pp_list ?(level = 0) pp_elem ~(sep : sep) fmt l =
  List.iteri
    (fun i elem ->
      let startline = if is_nl sep then indent level else "" in
      F.fprintf fmt "%s%a" startline pp_elem elem;
      if i < List.length l - 1 then pp_sep fmt sep)
    l

let pp_pairs ?(trailing = false) ?(level = 0) pp_k pp_v ~(rel : rel)
    ~(sep : sep) fmt pairs =
  List.iteri
    (fun i (k, v) ->
      let startline = if is_nl sep then indent level else "" in
      F.fprintf fmt "%s%a%a%a" startline pp_k k pp_rel rel pp_v v;
      if i < List.length pairs - 1 then pp_sep fmt sep)
    pairs;
  if trailing && pairs <> [] then pp_sep fmt sep
