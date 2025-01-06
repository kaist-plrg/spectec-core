open Core
open Util.Error

let error = error_stf

let lex (filename : string) =
  try
    let file = In_channel.read_all filename in
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.stmts Lexer.token lexbuf
  with Parser.Error -> Format.asprintf "parser error" |> error

let parse_file (filename : string) =
  let tokens = lex filename in
  parse tokens
