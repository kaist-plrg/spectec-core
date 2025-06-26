open Util.Error

let error = error_parse
let error_no_region = error_parse_no_region

let preprocess (includes : string list) (filename : string) =
  try Preprocessor.preprocess includes filename
  with _ -> "preprocessor error" |> error_no_region

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexer.enable_lexer_debug ();
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_region

let parse (lexbuf : Lexing.lexbuf) =
  let debug_level = Some "quiet" in
  try 
    match debug_level with
    | Some level ->
        let debug_level = match level with
          | "quiet" -> Parser_debug.Quiet
          | "basic" -> Parser_debug.Basic
          | "verbose" -> Parser_debug.Verbose
          | "full" -> Parser_debug.Full
          | _ -> Parser_debug.Basic
        in
        Parser_debug.set_debug_level debug_level;
        Parser_debug.debug_parse Lexer.lexer lexbuf
    | None ->
        Parser.p4program Lexer.lexer lexbuf
  with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      let pos = Lexing.lexeme_start_p lexbuf in
      let msg = Format.asprintf "syntax error at line %d, column %d" 
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) in
      error (Source.to_region info) msg
  | e -> raise e

let parse_string (filename : string) (str : string) : Il.Ast.value =
  (* assume str is preprocessed *)
  let tokens = lex filename str in
  parse tokens

let parse_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = preprocess includes filename in
  parse_string filename program

let roundtrip_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = parse_file includes filename in
  let program_str = Format.asprintf "%a\n" Pp.pp_value program in
  let program' = parse_string filename program_str in
  (* if not (Il.Eq.eq_value program program') then *)
  (*   "roundtrip error" |> error_no_region; *)
  program'

