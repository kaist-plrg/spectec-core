open Error
module Source = Util.Source

let with_lexbuf name lexbuf start =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try start Lexer.token lexbuf
  with Parser.Error ->
    error (Lexer.region lexbuf) "syntax error: unexpected token"

let parse_file file =
  let ic = open_in file in
  try
    Fun.protect
      (fun () -> with_lexbuf file (Lexing.from_channel ic) Parser.spec)
      ~finally:(fun () -> close_in ic)
  with Sys_error msg ->
    error (Source.region_of_file file) ("i/o error: " ^ msg)
