open Common.Source
module Atom = Xl.Atom

(* Elaboration-time mixop: a tree representation used while building up
   mixfix operators from the EL surface syntax. Converted to the flat IL
   [Il.Mixfix.mixop] form via [to_il] at the end of elaboration. *)

type atom = Atom.t phrase

type t =
  | Arg
  | Atom of atom
  | Brack of atom * t * atom
  | Infix of t * atom * t
  | Seq of t list

let rec to_string (mixop : t) : string =
  match mixop with
  | Arg -> "%"
  | Atom a -> Atom.unparse a.it
  | Brack (l, inner, r) ->
      Atom.unparse l.it ^ to_string inner ^ Atom.unparse r.it
  | Infix (l, a, r) ->
      "(" ^ to_string l ^ " " ^ Atom.unparse a.it ^ " " ^ to_string r ^ ")"
  | Seq ts -> "[" ^ String.concat " " (List.map to_string ts) ^ "]"

(* Inverse of [to_il] for mixops produced by the EL parser (balanced brackets,
   fixed infix precedence, no direct infix-in-Seq): [of_il (to_il t) = t].

   Recursive descent with precedence climbing over the grammar:

     expr        = primary_seq (INFIX expr)*
     primary_seq = primary+
     primary     = ATOM | ARG | BRACKET_L expr BRACKET_R

   Each [parse_*] returns the parsed node and the unconsumed mixemes. *)

let of_il (mixemes : Lang.Il.Mixfix.mixop) : t =
  let module M = Lang.Il.Mixfix in
  let bracket_matches l r =
    match Atom.closer_of l with Some p -> p = r | None -> false
  in
  let rec parse_expr min_prec mixemes =
    let lhs, mixemes = parse_primary_seq mixemes in
    (* Fold infix operators of precedence >= min_prec into lhs, left to right. *)
    let rec climb lhs mixemes =
      match mixemes with
      | M.Atom atom :: tail -> (
          match Atom.kind atom.it with
          | Atom.Infix { assoc; level = prec } when prec >= min_prec ->
              let inner_min_prec =
                match assoc with Left | Non -> prec + 1 | Right -> prec
              in
              let rhs, mixemes = parse_expr inner_min_prec tail in
              climb (Infix (lhs, atom, rhs)) mixemes
          | _ -> (lhs, mixemes))
      | _ -> (lhs, mixemes)
    in
    climb lhs mixemes
  and parse_primary_seq mixemes =
    let rec collect primaries_rev mixemes =
      match parse_primary_opt mixemes with
      | Some (primary, mixemes) -> collect (primary :: primaries_rev) mixemes
      | None -> (List.rev primaries_rev, mixemes)
    in
    let primaries, mixemes = collect [] mixemes in
    let tree =
      match primaries with [] -> Seq [] | [ single ] -> single | ps -> Seq ps
    in
    (tree, mixemes)
  and parse_primary_opt mixemes =
    match mixemes with
    | M.Arg () :: tail -> Some (Arg, tail)
    | M.Atom atom :: tail -> (
        match Atom.kind atom.it with
        | Atom.Plain -> Some (Atom atom, tail)
        | Atom.BracketL -> (
            let inner, mixemes = parse_expr 0 tail in
            match mixemes with
            | M.Atom atom_r :: tail when bracket_matches atom.it atom_r.it ->
                Some (Brack (atom, inner, atom_r), tail)
            | _ ->
                (* unreachable: EL's BrackT lifts bracket atoms only in matched pairs. *)
                assert false)
        | Atom.BracketR | Atom.Infix _ -> None)
    | [] -> None
  in
  let tree, mixemes_remaining = parse_expr 0 mixemes in
  match mixemes_remaining with
  | [] -> tree
  | _ ->
      (* unreachable: parse_expr / parse_primary_seq consume every EL-emitted atom. *)
      assert false

(* Conversion to the flat IL representation *)

let to_il (mixop : t) : Lang.Il.Mixfix.mixop =
  let rec flatten (mixop : t) (mixemes_rev : Lang.Il.Mixfix.mixop) :
      Lang.Il.Mixfix.mixop =
    match mixop with
    | Arg -> Lang.Il.Mixfix.Arg () :: mixemes_rev
    | Atom atom -> Lang.Il.Mixfix.Atom atom :: mixemes_rev
    | Brack (atom_left, inner, atom_right) ->
        let mixemes_rev = Lang.Il.Mixfix.Atom atom_left :: mixemes_rev in
        let mixemes_rev = flatten inner mixemes_rev in
        Lang.Il.Mixfix.Atom atom_right :: mixemes_rev
    | Infix (mixop_left, atom, mixop_right) ->
        let mixemes_rev = flatten mixop_left mixemes_rev in
        let mixemes_rev = Lang.Il.Mixfix.Atom atom :: mixemes_rev in
        flatten mixop_right mixemes_rev
    | Seq parts ->
        List.fold_left
          (fun mixemes_rev p -> flatten p mixemes_rev)
          mixemes_rev parts
  in
  List.rev (flatten mixop [])
