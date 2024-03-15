open Syntax

let preprocess (includes : string) (filename : string) =
  try Some (Preprocessor.preprocess includes filename)
  with _ ->
    Printf.sprintf "preprocessor error" |> print_endline;
    None

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Some (Lexing.from_string file)
  with Lexer.Error s ->
    Printf.sprintf "lexer error: %s" s |> print_endline;
    None

let parse (lexbuf : Lexing.lexbuf) =
  try Some (Parser.p4program Lexer.lexer lexbuf)
  with Parser.Error ->
    let info = Lexer.info lexbuf in
    Printf.sprintf "parser error: %s" (Info.to_string info) |> print_endline;
    None

let ( let* ) = Option.bind

let parse_file (includes : string) (filename : string) =
  let* file = preprocess includes filename in
  let* tokens = lex filename file in
  parse tokens

let parse_string (filename : string) (str : string) =
  (* assume str is preprocessed *)
  let* tokens = lex filename str in
  parse tokens
