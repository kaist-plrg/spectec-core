open Syntax
open Frontend

type error =
  | LexerError of string
  | ParserError of Info.t

let error_to_string (e : error) : string =
  match e with
  | LexerError s ->
    Printf.sprintf "lexer error: %s" s
  | ParserError info ->
    Printf.sprintf "parser error: %s" (Info.to_string info)

let handle_error (res: ('a, error) Result.t): 'a =
  match res with
  | Ok a -> a
  | Error e -> failwith (error_to_string e)

let lex (filename: string) (input: string) =
  try 
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Ok (Lexing.from_string input)
  with Lexer.Error s -> Error (LexerError s)

let parse (lexbuf: Lexing.lexbuf) =
  try
    Ok (Parser.p4program Lexer.lexer lexbuf)
  with Parser.Error -> Error (ParserError (Lexer.info lexbuf))

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in

  let chan = open_in filename in
  let input = In_channel.input_all chan in
  close_in chan;

  let _prog =
    let input = handle_error (lex filename input) in 
    handle_error (parse input)
  in

  print_endline "Parsing success. (TODO: print AST)"
