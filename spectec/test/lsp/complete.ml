(** Asks for completion at a cursor and prints what would be offered, with the
    text each candidate inserts, diffed against [complete.expected]. *)

module Lsp = Linol_eio

(* The spec is indexed once, as it is on the server: the index comes from the
   last save, and the line being typed is only text. Half-written lines are the
   normal case for completion, and most of them do not parse. *)
let spec =
  "syntax expr =\n\
  \  | ENum int\n\
  \  | EContains expr expr\n\
  \  | ENop\n\
   relation Eval: state |- expr ==> value\n\
   dec $lookup(state, expr) : value\n"

let index = (Spectec_lsp.Check.analyze ~path:"complete.spectec" spec).index

let describe (item : Lsp.CompletionItem.t) =
  (* Candidates carry an explicit replacement range, so what gets typed is the
     edit's text rather than [insertText]. *)
  let inserted =
    match item.textEdit with
    | Some (`TextEdit edit) -> edit.newText
    | _ -> (
        match item.insertText with None -> item.label | Some text -> text)
  in
  let format =
    match item.insertTextFormat with
    | Some Lsp.InsertTextFormat.Snippet -> "snippet"
    | _ -> "text"
  in
  Printf.printf "  %-16s %-8s %s\n" item.label format inserted

(* [line] is written with [@] where the cursor sits. A turnstile is the one
   thing these lines are sure to contain, which rules out the obvious marker. *)
let case name line =
  let character = Option.get (String.index_opt line '@') in
  let line =
    String.sub line 0 character
    ^ String.sub line (character + 1) (String.length line - character - 1)
  in
  Printf.printf "## %s\n" name;
  let list = Spectec_lsp.Complete.candidates ~index ~line ~character in
  (match list.items with
  | [] -> print_endline "  (nothing)"
  | items -> List.iter describe items);
  print_newline ()

let () =
  (* A case, mid-conclusion, arrives as its name alone: what it takes is shown
     as the item's detail, not typed into the document. [expr] rides along
     because the prefix match ignores case, which is what makes a lowercase name
     findable from a capital. *)
  case "case in a conclusion" "  state |- E@";
  (* A case that takes nothing, which now reads the same as one that does. *)
  case "case with no arguments" "  state |- ENo@";
  (* The [$] already on the line is not repeated: the [dec] is offered under
     its full name and inserts the rest. *)
  case "function after a dollar" "  $l@";
  (* A relation is invoked after a [--], and the separating space has to come
     with the name, or the line reads [--Eval]. *)
  case "relation after a dash" "  --@"
