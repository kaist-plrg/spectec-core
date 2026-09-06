(** Match prose steps to existing AST source locations. *)

open Common.Source
module Sl = Lang.Sl
module Pl = Lang.Pl

let first_line text =
  match String.index_opt text '\n' with
  | None -> text
  | Some stop -> String.sub text 0 stop

let numbered text =
  let length = String.length text in
  let rec spaces i =
    if i < length && text.[i] = ' ' then spaces (i + 1) else i
  in
  let rec digits i =
    if i < length && '0' <= text.[i] && text.[i] <= '9' then digits (i + 1)
    else i
  in
  let indent = spaces 0 in
  let stop = digits indent in
  if
    stop > indent
    && stop + 1 < length
    && text.[stop] = '.'
    && text.[stop + 1] = ' '
  then Some (indent, String.sub text (stop + 2) (length - stop - 2))
  else None

let heading text =
  match numbered (first_line text) with
  | Some (_, body) -> body
  | None -> first_line text

(* Verify every heading before exposing precise source mappings. *)
let align text steps =
  let lines =
    String.split_on_char '\n' text
    |> List.mapi (fun line text ->
           Option.map
             (fun (indent, body) -> (line, indent, body))
             (numbered text))
    |> List.filter_map Fun.id
  in
  let rec pair acc lines steps =
    match (lines, steps) with
    | [], [] -> List.rev acc
    | (line, indent, body) :: lines, (level, expected, region) :: steps
      when indent = level * 2 && String.equal body expected ->
        let acc = if region = no_region then acc else (line, region) :: acc in
        pair acc lines steps
    | _ -> []
  in
  pair [] lines steps

let sl ~text (def : Sl.def) =
  let steps = ref [] in
  let add level body region = steps := (level, body, region) :: !steps in
  let first_region = function [] -> no_region | instr :: _ -> instr.at in
  let phantom level region = function
    | None -> ()
    | Some phantom ->
        add level ("Else " ^ Sl.Print.string_of_phantom phantom) region
  in
  let rec block level instrs = List.iter (instr level) instrs
  and instr level (node : Sl.instr) =
    (* Render shallow nodes to avoid repeatedly printing subtrees. *)
    let empty = Sl.ResultI [] $ no_region in
    let shallow =
      match node.it with
      | Sl.RelI call -> Sl.RelI { call with block = [] }
      | Sl.RelAssertI call ->
          Sl.RelAssertI { call with block = []; phantom = None }
      | Sl.IfI (exp, iters, _, _) -> Sl.IfI (exp, iters, [], None)
      | Sl.CaseI (exp, _, _) -> Sl.CaseI (exp, [], None)
      | Sl.OtherwiseI _ -> Sl.OtherwiseI empty
      | Sl.LetI (left, right, iters, _) -> Sl.LetI (left, right, iters, [])
      | Sl.DebugI (exp, _) -> Sl.DebugI (exp, empty)
      | Sl.ResultI _ | Sl.ReturnI _ -> node.it
    in
    add level
      (heading (Sl.Print.string_of_instr { node with it = shallow }))
      node.at;
    match node.it with
    | Sl.RelI { block = children; _ } | Sl.LetI (_, _, _, children) ->
        block level children
    | Sl.RelAssertI { block = children; phantom = tail; _ }
    | Sl.IfI (_, _, children, tail) ->
        block (level + 1) children;
        phantom level node.at tail
    | Sl.CaseI (_, cases, tail) ->
        List.iter
          (fun (guard, children) ->
            add (level + 1)
              (heading (Sl.Print.string_of_case (guard, [])))
              (first_region children);
            block (level + 2) children)
          cases;
        phantom level node.at tail
    | Sl.OtherwiseI child -> instr (level + 1) child
    | Sl.DebugI (_, child) -> instr level child
    | Sl.ResultI _ | Sl.ReturnI _ -> ()
  in
  (match def.it with
  | Sl.RelD (_, _, children, tail) | Sl.DecD (_, _, _, children, tail) ->
      block 0 children;
      Option.iter
        (fun children ->
          add 0
            (heading (Sl.Print.string_of_elseblock []))
            (first_region children);
          block 1 children)
        tail
  | Sl.TypD _ | Sl.BuiltinDecD _ -> ());
  align text (List.rev !steps)

let pl ~text (def : Pl.def) =
  let steps = ref [] in
  let add level body region = steps := (level, body, region) :: !steps in
  let first_region = function
    | [] -> no_region
    | (instr : Pl.instr) :: _ -> instr.node.at
  in
  let phantom level region = function
    | None -> ()
    | Some phantom ->
        add level ("Else " ^ Pl.Print.string_of_phantom phantom) region
  in
  let rec block level instrs = List.iter (instr level) instrs
  and instr level (node : Pl.instr) =
    add level
      (first_line (Pl.Print.string_of_instr ~short:true node))
      node.node.at;
    match node.node.it with
    | Pl.RelAssertI { block = children; phantom = tail; _ }
    | Pl.IfI (_, _, children, tail) ->
        block (level + 1) children;
        phantom level node.node.at tail
    | Pl.CaseI (_, cases, tail) ->
        List.iter
          (fun (guard, children) ->
            add (level + 1)
              (heading (Pl.Print.string_of_case (guard, [])))
              (first_region children);
            block (level + 2) children)
          cases;
        phantom level node.node.at tail
    | Pl.OtherwiseI child -> instr (level + 1) child
    | Pl.TryI arms -> List.iter (block (level + 1)) arms
    | Pl.CheckLetI (_, _, children) | Pl.OptionGetI (_, _, children) ->
        block (level + 1) children
    | Pl.RelI _ | Pl.LetI _ | Pl.ResultI _ | Pl.ReturnI _ | Pl.DebugI _
    | Pl.DestructI _ ->
        ()
  in
  (match def.node.it with
  | Pl.RelD (_, _, _, children, tail) | Pl.DecD (_, _, _, children, tail) ->
      block 0 children;
      Option.iter
        (fun children ->
          add 0
            (heading (Pl.Print.string_of_elseblock []))
            (first_region children);
          block 1 children)
        tail
  | Pl.TypD _ | Pl.BuiltinDecD _ -> ());
  align text (List.rev !steps)
