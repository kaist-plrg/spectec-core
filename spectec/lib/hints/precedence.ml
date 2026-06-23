let parse (hintexp : El.exp) : Xl.Atom.t option =
  match hintexp.it with
  | El.AtomE { it = (Xl.Atom.Operator _ | Xl.Atom.Keyword _) as atom; _ } ->
      Some atom
  | _ -> None

let find (hints : Il.hint list) : Xl.Atom.t option =
  List.find_map
    (fun (hint : Il.hint) ->
      match Registry.lookup hint.hintid.it with
      | Some { kind = Registry.Precedence; _ } -> parse hint.hintexp
      | _ -> None)
    hints
