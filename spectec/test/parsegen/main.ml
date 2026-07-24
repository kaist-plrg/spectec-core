(* Differential test for the grammar-driven parser and printer, both built from
   the extracted grammar alone. Parse: Parsegen.Parse must reproduce impty's
   hand-written parser. Round-trip: Parsegen.Print must render each value so it
   re-parses unchanged. Run over hand-written expressions and the base .imp
   corpus. *)

open Spectec
module Parse = Parsegen.Parse
module Print = Parsegen.Print
module Lexer = Targets_impty.Lexer
module Reference = Targets_impty.Parse

let filename = "<test>"

let load_grammar spec : Grammar.t =
  match parse_spec_files [ spec ] with
  | Ok el -> (
      match elaborate el with
      | Ok il -> Grammar.extract ~start:"prog" il
      | Error _ -> failwith "could not elaborate the impty spec")
  | Error _ -> failwith "could not parse the impty spec"

(* Tokens that appear only in the closure spec (fun, rec, braces, arrow, ternary)
   have no base-grammar terminal, so they map to [None]. *)
let classify : Lexer.token -> Grammar.Terminal.t option =
  let keyword k = Some (Grammar.Terminal.Atom (Lang.Xl.Atom.keyword k)) in
  let operator s = Some (Grammar.Terminal.Atom (Lang.Xl.Atom.operator s)) in
  let primitive kind value = Some (Grammar.Terminal.Primitive (kind, value)) in
  function
  | Lexer.Plus -> operator "+"
  | Lexer.Leq -> operator "<="
  | Lexer.And -> operator "&&"
  | Lexer.Bang -> operator "!"
  | Lexer.Eq -> operator "="
  | Lexer.Semi -> operator ";"
  | Lexer.Lparen -> Some (Grammar.Terminal.Atom Lang.Xl.Atom.LParen)
  | Lexer.Rparen -> Some (Grammar.Terminal.Atom Lang.Xl.Atom.RParen)
  | Lexer.KwInt -> keyword "INT"
  | Lexer.KwBool -> keyword "BOOL"
  | Lexer.KwSkip -> keyword "SKIP"
  | Lexer.KwIf -> keyword "IF"
  | Lexer.KwThen -> keyword "THEN"
  | Lexer.KwElse -> keyword "ELSE"
  | Lexer.KwEnd -> keyword "END"
  | Lexer.KwWhile -> keyword "WHILE"
  | Lexer.KwDo -> keyword "DO"
  | Lexer.Num n ->
      primitive (Grammar.Num `IntT) (Lang.Il.Value.int (Bigint.of_int n))
  | Lexer.KwTrue -> primitive Grammar.Bool (Lang.Il.Value.bool true)
  | Lexer.KwFalse -> primitive Grammar.Bool (Lang.Il.Value.bool false)
  | Lexer.Ident s -> primitive Grammar.Text (Lang.Il.Value.text s)
  | Lexer.KwFun | Lexer.KwRec | Lexer.Lbrace | Lexer.Rbrace | Lexer.Arrow
  | Lexer.Question | Lexer.Colon | Lexer.Eof ->
      None

(* The inverse of [classify]: render each grammar terminal back to surface text. *)
let print_terminal : Grammar.Terminal.t -> string = function
  | Grammar.Terminal.Atom a ->
      Lang.Xl.Atom.to_string a |> String.lowercase_ascii
  | Grammar.Terminal.Primitive (Grammar.Num _, v) ->
      Bigint.to_string (Lang.Xl.Num.to_int (Lang.Il.Value.get_num v))
  | Grammar.Terminal.Primitive (Grammar.Bool, v) ->
      if Lang.Il.Value.get_bool v then "true" else "false"
  | Grammar.Terminal.Primitive (Grammar.Text, v) -> Lang.Il.Value.get_text v

let lex (source : string) : Lexer.token list =
  Lexer.tokenize ~filename source
  |> List.filter_map (fun (token, _region) ->
         match token with Lexer.Eof -> None | token -> Some token)

let show_value = Lang.Il.Print_debug.string_of_value

let check (grammar : Grammar.t) (start : string)
    (parse_reference : filename:string -> string -> Lang.Il.value)
    (label : string) (source : string) : unit =
  let generated = Parse.run classify grammar start (lex source) in
  let reference = parse_reference ~filename source in
  let verdict =
    if Lang.Il.Eq.eq_value generated reference then "OK" else "MISMATCH"
  in
  Printf.printf "%-32s %s\n  generated: %s\n  reference: %s\n" label verdict
    (show_value generated) (show_value reference)

let roundtrip (grammar : Grammar.t) (start : string)
    (parse_reference : filename:string -> string -> Lang.Il.value)
    (label : string) (source : string) : unit =
  let value = parse_reference ~filename source in
  let printed = Print.run print_terminal grammar start value in
  let reparsed = Parse.run classify grammar start (lex printed) in
  let verdict =
    if Lang.Il.Eq.eq_value value reparsed then "OK" else "MISMATCH"
  in
  Printf.printf "%-32s %s\n  printed: %s\n" label verdict printed

let expressions =
  [
    "1";
    "x";
    "true";
    "1 + 2 + 3";
    "1 + 2 <= 3";
    "a <= b + c";
    "! a && b";
    "! (a + b) <= c && d";
    "1 + (2 + 3)";
  ]

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let corpus corpus_dir : string list =
  Sys.readdir corpus_dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".imp")
  |> List.sort String.compare

let run spec corpus_dir =
  let grammar = load_grammar spec in
  let expr_cases = List.map (fun e -> (e, e)) expressions in
  let prog_cases =
    corpus corpus_dir
    |> List.map (fun file ->
           (file, read_file (Filename.concat corpus_dir file)))
  in
  let section title run_case cases =
    print_endline ("=== " ^ title ^ " ===");
    List.iter (fun (label, source) -> run_case label source) cases
  in
  section "parse: expressions"
    (check grammar "expr" Reference.parse_expr_exn)
    expr_cases;
  print_newline ();
  section "parse: programs (base .imp corpus)"
    (check grammar "prog" Reference.parse_string_exn)
    prog_cases;
  print_newline ();
  section "round-trip: expressions"
    (roundtrip grammar "expr" Reference.parse_expr_exn)
    expr_cases;
  print_newline ();
  section "round-trip: programs (base .imp corpus)"
    (roundtrip grammar "prog" Reference.parse_string_exn)
    prog_cases

let command =
  Core.Command.basic
    ~summary:"differential test of the grammar-driven parser and printer"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map spec = flag "-s" (required string) ~doc:"FILE impty base spec file"
  and corpus_dir =
    flag "-d" (required string) ~doc:"DIR base .imp program corpus"
  in
  fun () -> run spec corpus_dir

let () = Command_unix.run command
