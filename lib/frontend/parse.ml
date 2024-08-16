open Util

let preprocess (includes : string list) (filename : string) =
  try Some (Preprocessor.preprocess includes filename)
  with _ ->
    Format.eprintf "preprocessor error\n";
    None

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Some (Lexing.from_string file)
  with Lexer.Error s ->
    Format.eprintf "lexer error: %s\n" s;
    None

let parse (lexbuf : Lexing.lexbuf) =
  try
    let program = Parser.p4program Lexer.lexer lexbuf in
    let program = Transform.transform_program program in
    Some program
  with Parser.Error ->
    let info = Lexer.info lexbuf in
    Format.eprintf "parser error: %a\n" Source.pp info;
    None

let ( let* ) = Option.bind

let parse_file (includes : string list) (filename : string) =
  let* file = preprocess includes filename in
  let* tokens = lex filename file in
  parse tokens

let parse_string (filename : string) (str : string) =
  (* assume str is preprocessed *)
  let* tokens = lex filename str in
  parse tokens
