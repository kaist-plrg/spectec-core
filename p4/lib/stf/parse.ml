open Core

let lex (filename : string) =
  try
    let file = In_channel.read_all filename in
    Some (Lexing.from_string file)
  with Lexer.Error s ->
    Format.eprintf "lexer error: %s\n" s;
    None

let parse (lexbuf : Lexing.lexbuf) =
  try Some (Parser.stmts Lexer.token lexbuf)
  with Parser.Error ->
    Format.eprintf "parser error\n";
    None

let parse_file (filename : string) =
  match lex filename with Some tokens -> parse tokens | None -> None
