(* Differential test: the grammar-driven parser must build the same IL value as
   impty's hand-written one, over hand-written expressions and the base .imp
   program corpus. *)

open Spectec
module Parse = Parsegen.Parse
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

let check_expr grammar expr =
  check grammar "expr" Reference.parse_expr_exn expr expr

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

let check_prog grammar corpus_dir file =
  check grammar "prog" Reference.parse_string_exn file
    (read_file (Filename.concat corpus_dir file))

let corpus corpus_dir : string list =
  Sys.readdir corpus_dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".imp")
  |> List.sort String.compare

let run spec corpus_dir =
  let grammar = load_grammar spec in
  print_endline "=== expressions ===";
  List.iter (check_expr grammar) expressions;
  print_endline "\n=== programs (base .imp corpus) ===";
  List.iter (check_prog grammar corpus_dir) (corpus corpus_dir)

let command =
  Core.Command.basic
    ~summary:"differential test of the grammar-driven parser against impty"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map spec = flag "-s" (required string) ~doc:"FILE impty base spec file"
  and corpus_dir =
    flag "-d" (required string) ~doc:"DIR base .imp program corpus"
  in
  fun () -> run spec corpus_dir

let () = Command_unix.run command
