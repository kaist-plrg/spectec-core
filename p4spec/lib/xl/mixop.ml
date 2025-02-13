open Util.Source

(* Mixop is a generalized case constructor *)

type t = Atom.t phrase list list

let compare mixop_a mixop_b =
  let mixop_a = List.map (List.map it) mixop_a in
  let mixop_b = List.map (List.map it) mixop_b in
  List.compare (List.compare Atom.compare) mixop_a mixop_b

let eq mixop_a mixop_b = compare mixop_a mixop_b = 0

(* Combinator *)

let merge mixop_a mixop_b =
  match (mixop_a, mixop_b) with
  | _, [] -> mixop_a
  | [], _ -> mixop_b
  | mixop_a, atoms_b :: mixop_b ->
      let mixop_a, atoms_a =
        List.rev mixop_a |> fun mixop_a ->
        (mixop_a |> List.tl |> List.rev, mixop_a |> List.hd)
      in
      mixop_a @ [ atoms_a @ atoms_b ] @ mixop_b

(* Stringifier *)

let string_of_mixop mixop =
  let mixop = List.map (List.map it) mixop in
  let smixop =
    String.concat "%"
      (List.map
         (fun atoms -> String.concat "" (List.map Atom.string_of_atom atoms))
         mixop)
  in
  "`" ^ smixop ^ "`"
