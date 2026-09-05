(** Annotate unexplained diagnostics with likely undeclared metavariables. *)

module Lsp = Linol_eio

(* Elaboration knows these without a declaration. *)
let builtin =
  [ "bool"; "nat"; "int"; "rat"; "real"; "text"; "eps"; "true"; "false" ]

let is_ident_start c = ('a' <= c && c <= 'z') || c = '_'

let is_ident_char c =
  ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || ('0' <= c && c <= '9')
  || c = '_' || c = '\''

(* Scan lowercase identifiers, excluding atoms and functions. *)
let identifiers text =
  let n = String.length text in
  let rec scan i acc =
    if i >= n then List.rev acc
    else if
      is_ident_start text.[i] && (i = 0 || not (is_ident_char text.[i - 1]))
    then (
      let j = ref i in
      while !j < n && is_ident_char text.[!j] do
        incr j
      done;
      let word = String.sub text i (!j - i) in
      let preceded_by_dollar = i > 0 && text.[i - 1] = '$' in
      scan !j (if preceded_by_dollar then acc else word :: acc))
    else scan (i + 1) acc
  in
  scan 0 []

(* Extract diagnostic text using zero-based LSP positions. *)
let text_of_range (text : string) (range : Lsp.Range.t) =
  let lines = String.split_on_char '\n' text in
  let line_at i = try Some (List.nth lines i) with _ -> None in
  if range.start.line = range.end_.line then
    match line_at range.start.line with
    | None -> ""
    | Some line ->
        let len = String.length line in
        let s = min range.start.character len in
        let e = min range.end_.character len in
        if e > s then String.sub line s (e - s) else ""
  else
    let rec gather i acc =
      if i > range.end_.line then String.concat "\n" (List.rev acc)
      else
        match line_at i with
        | None -> String.concat "\n" (List.rev acc)
        | Some line -> gather (i + 1) (line :: acc)
    in
    gather range.start.line []

let undeclared ~(index : Index.t) snippet =
  identifiers snippet
  |> List.filter (fun name ->
         (not (List.mem name builtin))
         && (not (List.mem (Index.base_name name) builtin))
         && not (Index.declares index name))
  |> List.sort_uniq String.compare

(* Suggest base declarations even for malformed subscripts. *)
let suggested_base name =
  let name = String.concat "" (String.split_on_char '\'' name) in
  let n = ref (String.length name) in
  while !n > 0 && '0' <= name.[!n - 1] && name.[!n - 1] <= '9' do
    decr n
  done;
  if !n > 0 && name.[!n - 1] = '_' then decr n;
  if !n = 0 then name else String.sub name 0 !n

(* Detect numeric suffixes missing the subscript underscore. *)
let mistyped_subscript name =
  let n = String.length name in
  n >= 2
  && '0' <= name.[n - 1]
  && name.[n - 1] <= '9'
  && not (String.contains name '_')

let note_for names =
  let quoted = List.map (fun n -> "`" ^ n ^ "`") names in
  let subject, verb =
    match quoted with
    | [ one ] -> (one, "is not a declared metavariable")
    | _ -> (String.concat ", " quoted, "are not declared metavariables")
  in
  let suggestion =
    names |> List.map suggested_base
    |> List.sort_uniq String.compare
    |> List.map (fun base -> Printf.sprintf "`var %s : <type>`" base)
    |> String.concat ", "
  in
  let renaming name =
    let base = suggested_base name in
    let digits =
      String.sub name (String.length base)
        (String.length name - String.length base)
    in
    Printf.sprintf "`%s` should be `%s_%s`" name base digits
  in
  let subscript_note =
    match List.filter mistyped_subscript names with
    | [] -> ""
    | mistyped ->
        Printf.sprintf " A subscript is written with `_`, so %s."
          (String.concat ", " (List.map renaming mistyped))
  in
  Printf.sprintf
    "\n\nnote: %s %s, so this expression cannot be typed. Declare %s.%s" subject
    verb suggestion subscript_note

(* Annotate only errors lacking their own culprit. *)
let wants_reason (d : Lsp.Diagnostic.t) =
  match d.severity with
  | Some Lsp.DiagnosticSeverity.Error -> (
      match d.message with
      | `String message ->
          let has substring =
            let n = String.length substring in
            let rec go i =
              i + n <= String.length message
              && (String.equal (String.sub message i n) substring || go (i + 1))
            in
            n > 0 && go 0
          in
          has "elaboration of expression"
          || has "does not match any case"
          || has "does not match notation"
      | _ -> false)
  | _ -> false

let enrich ~(index : Index.t) ~(text : string) (d : Lsp.Diagnostic.t) :
    Lsp.Diagnostic.t =
  if not (wants_reason d) then d
  else
    match undeclared ~index (text_of_range text d.range) with
    | [] -> d
    | names -> (
        match d.message with
        | `String message ->
            { d with message = `String (message ^ note_for names) }
        | _ -> d)
