(** Infer completions from text, tolerating incomplete syntax. *)

module Lsp = Linol_eio

(* Cap results to keep large specs responsive. *)
let limit = 200

type context =
  | Function  (** After a [$]: only [dec]s can follow. *)
  | Premise  (** After [--]: relations or premise keywords. *)
  | Declaration  (** At the left margin: a declaration keyword. *)
  | Rule_relation  (** Before the rule header's [/]: the relation. *)
  | Rule_name  (** After the rule header's [/]: the rule name. *)
  | Anywhere  (** Inside an expression: types, metavariables, cases. *)

let is_ident_char c =
  ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || ('0' <= c && c <= '9')
  || c = '_' || c = '\''

(* Split cursor text into preceding text and identifier. *)
let split_line (line : string) (character : int) =
  let at = min character (String.length line) in
  let start = ref at in
  while !start > 0 && is_ident_char line.[!start - 1] do
    decr start
  done;
  (String.sub line 0 !start, String.sub line !start (at - !start))

let leading_word (before : string) =
  let trimmed = String.trim before in
  match String.index_opt trimmed ' ' with
  | Some i -> String.sub trimmed 0 i
  | None -> trimmed

(* Rule headers accept relation/name until the colon. *)
let in_rule_header (before : string) =
  String.equal (leading_word before) "rule" && not (String.contains before ':')

let context_of ~(before : string) =
  let trimmed = String.trim before in
  if String.length before > 0 && before.[String.length before - 1] = '$' then
    Function
  else if trimmed = "--" then Premise
    (* Indented blank lines allow expressions, never declarations. *)
  else if before = "" then Declaration
  else if in_rule_header before then
    if String.contains before '/' then Rule_name else Rule_relation
  else Anywhere

let declaration_keywords = [ "syntax"; "var"; "relation"; "rule"; "dec"; "def" ]
let premise_keywords = [ "if"; "otherwise" ]

let item_kind : Index.kind -> Lsp.CompletionItemKind.t = function
  | Index.Syntax -> Lsp.CompletionItemKind.Class
  | Index.Var -> Lsp.CompletionItemKind.Variable
  | Index.Relation -> Lsp.CompletionItemKind.Interface
  | Index.Dec -> Lsp.CompletionItemKind.Function
  | Index.Rule -> Lsp.CompletionItemKind.Method
  | Index.Case -> Lsp.CompletionItemKind.EnumMember
  | Index.Field -> Lsp.CompletionItemKind.Field

(* Strip the function sigil already being typed. *)
let bare_name ~(context : context) name =
  match context with
  | Function when String.length name > 0 && name.[0] = '$' ->
      String.sub name 1 (String.length name - 1)
  | _ -> name

(* Include [$] explicitly so client filtering agrees. *)
let replacing ~(line : int) ~(character : int) ~(prefix : string)
    ~(context : context) =
  let dollar = match context with Function -> 1 | _ -> 0 in
  let start = max 0 (character - String.length prefix - dollar) in
  Lsp.Range.create
    ~start:(Lsp.Position.create ~line ~character:start)
    ~end_:(Lsp.Position.create ~line ~character)

(* Insert premise spacing; filter by name alone. *)
let edit ~(range : Lsp.Range.t) ~(needs_space : bool) name =
  let newText = if needs_space then " " ^ name else name in
  `TextEdit (Lsp.TextEdit.create ~range ~newText)

let keyword_item ~range ~needs_space label =
  Lsp.CompletionItem.create ~label ~kind:Lsp.CompletionItemKind.Keyword
    ~detail:"keyword"
    ~textEdit:(edit ~range ~needs_space label)
    ~filterText:label ()

(* Rank by expected type without excluding candidates. *)

let find_from text pos sub =
  let n = String.length sub and m = String.length text in
  if n = 0 then None
  else
    let rec at i =
      if i + n > m then None
      else if String.equal (String.sub text i n) sub then Some i
      else at (i + 1)
    in
    at (max 0 pos)

(* Find the enclosing rule from unindented lines. *)
let enclosing_relation (preceding : string list) =
  let rec go = function
    | [] -> None
    | line :: above ->
        if String.trim line = "" || line.[0] = ' ' || line.[0] = '\t' then
          go above
        else if String.equal (leading_word line) "rule" then
          let rest = String.trim (String.sub line 4 (String.length line - 4)) in
          Option.map (String.sub rest 0) (String.index_opt rest '/')
        else None
  in
  go (List.rev preceding)

(* Count notation separators to locate the hole. *)
let hole_at ~(before : string) (notation : Index.part list) =
  let text = String.trim before in
  let slot = ref (-1) in
  let rec separator = function
    | [] -> None
    | Index.Literal sep :: rest ->
        let sep = String.trim sep in
        if sep = "" then separator rest else Some sep
    | Index.Hole _ :: _ -> None
  in
  let rec go pos = function
    | [] -> None
    | Index.Literal _ :: rest -> go pos rest
    | Index.Hole name :: rest -> (
        incr slot;
        match separator rest with
        | None -> Some (!slot, name)
        | Some sep -> (
            match find_from text pos sep with
            | Some i -> go (i + String.length sep) rest
            | None -> Some (!slot, name)))
  in
  go 0 notation

(* Use IL types, falling back to EL names. *)
let expected_type ~(index : Index.t) ~(typing : Typing.t)
    ~(preceding : string list) ~(before : string) =
  let trimmed = String.trim before in
  (* Premises invoke their own relation, requiring separate inference. *)
  if String.length trimmed >= 2 && String.sub trimmed 0 2 = "--" then None
  else
    match enclosing_relation preceding with
    | None -> None
    | Some relation -> (
        let hole =
          Index.find index relation
          |> List.find_opt (fun (e : Index.entry) -> e.kind = Index.Relation)
          |> Fun.flip Option.bind (fun (e : Index.entry) ->
                 hole_at ~before e.notation)
        in
        match hole with
        | None -> None
        | Some (slot, name) ->
            let typ =
              match Typing.hole_type typing ~relation ~index:slot with
              | Some typ -> typ
              | None -> name
            in
            Some (typ, Typing.canonical typing typ))

(* Rank declared types first, aliases second, others last. *)
let sort_text ~(typing : Typing.t) ~(expected : (string * string) option)
    (entry : Index.entry) =
  match (expected, entry.fills) with
  | None, _ -> None
  | Some _, None -> Some ("2" ^ entry.name)
  | Some (declared, canonical), Some fills ->
      let band =
        if String.equal fills declared then "0"
        else if String.equal (Typing.canonical typing fills) canonical then "1"
        else "2"
      in
      Some (band ^ entry.name)

(* Insert names only; show arguments in details. *)
let entry_item ~(range : Lsp.Range.t) ~(needs_space : bool) ~(typing : Typing.t)
    ~(expected : (string * string) option) (entry : Index.entry) =
  Lsp.CompletionItem.create ~label:entry.name ~kind:(item_kind entry.kind)
    ~detail:entry.signature
    ~textEdit:(edit ~range ~needs_space entry.name)
    ~filterText:entry.name
    ?documentation:
      (Option.map
         (fun text -> `String text)
         (match (entry.doc, entry.detail) with
         | Some doc, Some detail -> Some (doc ^ "\n\n" ^ detail)
         | Some doc, None -> Some doc
         | None, detail -> detail))
    ?sortText:(sort_text ~typing ~expected entry)
    ()

let wanted context (entry : Index.entry) =
  match (context, entry.kind) with
  | Function, Index.Dec -> true
  | Function, _ -> false
  | Premise, Index.Relation -> true
  | Premise, _ -> false
  | Declaration, _ -> false
  | Rule_relation, Index.Relation -> true
  | Rule_relation, _ -> false
  (* Suggest case names as conventional rule names. *)
  | Rule_name, Index.Case -> true
  | Rule_name, _ -> false
  (* Expressions exclude rules, functions, and relation names. *)
  | Anywhere, (Index.Rule | Index.Dec | Index.Relation) -> false
  | Anywhere, _ -> true

let starts_with ~prefix text =
  let n = String.length prefix in
  String.length text >= n
  && String.equal
       (String.lowercase_ascii (String.sub text 0 n))
       (String.lowercase_ascii prefix)

let keywords_for = function
  | Declaration -> declaration_keywords
  | Premise -> premise_keywords
  | Function | Rule_relation | Rule_name | Anywhere -> []

let in_context ~(index : Index.t) ~(typing : Typing.t)
    ~(preceding : string list) ~(line : string) ~(character : int) =
  let before, prefix = split_line line character in
  let context = context_of ~before in
  let expected =
    match context with
    | Anywhere -> expected_type ~index ~typing ~preceding ~before
    | _ -> None
  in
  let needs_space =
    String.length before > 0 && before.[String.length before - 1] = '-'
  in
  let range =
    replacing ~line:(List.length preceding)
      ~character:(min character (String.length line))
      ~prefix ~context
  in
  let keywords =
    keywords_for context
    |> List.filter (fun keyword -> starts_with ~prefix keyword)
    |> List.map (keyword_item ~range ~needs_space)
  in
  let entries =
    Index.to_list index
    |> List.filter (fun (entry : Index.entry) ->
           wanted context entry
           &&
           match context with
           (* Match function prefixes without the leading sigil. *)
           | Function -> starts_with ~prefix (bare_name ~context entry.name)
           | _ -> starts_with ~prefix entry.name)
  in
  (* Offer each name once despite repeated declarations. *)
  let seen = Hashtbl.create 64 in
  let entries =
    List.filter
      (fun (entry : Index.entry) ->
        if Hashtbl.mem seen entry.name then false
        else (
          Hashtbl.add seen entry.name ();
          true))
      entries
  in
  let items =
    keywords
    @ List.map (entry_item ~range ~needs_space ~typing ~expected) entries
  in
  let truncated = List.length items > limit in
  let items =
    if truncated then List.filteri (fun i _ -> i < limit) items else items
  in
  (* Mark filtered lists incomplete so deletions refresh. *)
  let incomplete = truncated || prefix <> "" in
  Lsp.CompletionList.create ~isIncomplete:incomplete ~items ()

let candidates ~(index : Index.t) ~(line : string) ~(character : int) =
  in_context ~index ~typing:Typing.empty ~preceding:[] ~line ~character
