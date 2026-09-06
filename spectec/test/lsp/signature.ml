(** Asks for a parameter hint at a cursor and prints the form it describes, with
    the argument marked, diffed against [signature.expected]. *)

module Lsp = Linol_eio

(* Indexed once, as on the server: the hint is wanted while the application is
   still missing arguments, which is text that does not parse. *)
let spec =
  "syntax expr =\n\
  \  | ENum int\n\
  \  | EContains expr expr\n\
  \  | ENop\n\
   relation Eval: state |- expr ==> value\n\
   dec $lookup(state, expr) : value\n"

let index = (Spectec_lsp.Check.analyze ~path:"signature.spectec" spec).index

(* The active argument is reported as an offset into the label, so print the
   label with that span underlined rather than the raw numbers. *)
let underline label (from, until) =
  String.init (String.length label) (fun i ->
      if i >= from && i < until then '~' else ' ')

let describe (help : Lsp.SignatureHelp.t) =
  match help.signatures with
  | [] -> print_endline "  (no hint)"
  | signature :: _ -> (
      let active = Option.join signature.activeParameter in
      let spans =
        Option.value signature.parameters ~default:[]
        |> List.filter_map (fun (p : Lsp.ParameterInformation.t) ->
               match p.label with
               | `Offset (from, until) -> Some (from, until)
               | `String _ -> None)
      in
      Printf.printf "  %s\n" signature.label;
      match Option.bind active (List.nth_opt spans) with
      | Some span -> Printf.printf "  %s\n" (underline signature.label span)
      | None -> ())

(* [line] is written with [@] where the cursor sits. A turnstile is the one
   thing these lines are sure to contain, which rules out the obvious marker. *)
let case name line =
  let character = Option.get (String.index_opt line '@') in
  let line =
    String.sub line 0 character
    ^ String.sub line (character + 1) (String.length line - character - 1)
  in
  Printf.printf "## %s\n" name;
  describe (Spectec_lsp.Signature.at ~index ~line ~character);
  print_newline ()

let () =
  (* The moment a name is accepted from the completion list: the hint names the
     argument to type first, before any separator is there to go by. *)
  case "on the name itself" "  state |- EContains@";
  case "first argument" "  state |- EContains @";
  case "second argument" "  state |- EContains e_1 @";
  case "inside the second" "  state |- EContains e_1 e@";
  (* A nested application is the innermost one, and its own arguments are
     counted from its own head. *)
  case "nested call" "  state |- EContains (ENum @";
  (* The space inside the finished nested call is not a separator of ours. *)
  case "after a nested call" "  state |- EContains (ENum 3) @";
  (* A [dec] separates its arguments with a comma rather than a space. *)
  case "function argument" "  $lookup(st@";
  case "second function argument" "  $lookup(state, @";
  (* A comma belonging to a nested call is likewise not ours to count. *)
  case "comma inside a nested call" "  $lookup($lookup(a, b), @";
  (* A relation's arguments are separated by its own notation. *)
  case "relation, first" "  -- Eval: @";
  case "relation, past the turnstile" "  -- Eval: state |- @";
  (* Nothing is being applied here. *)
  case "not in an application" "  state |- @";
  case "a name that takes nothing" "  state |- ENop @"
