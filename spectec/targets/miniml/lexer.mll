{
  open Lexing
  open Parse
  open Common.Source

  let pos p =
    { file = p.pos_fname; line = p.pos_lnum; column = p.pos_cnum }
  let region lb =
    { left = pos (lexeme_start_p lb); right = pos (lexeme_end_p lb) }
}

let digit = ['0'-'9']
let char = ['a'-'z' '_' 'A'-'Z']

rule token = parse
| [' ' '\t' '\r']+
  { token lexbuf }
| '\n'
  { new_line lexbuf; token lexbuf }
| "//" [^ '\n']* ('\n' | eof)
  { new_line lexbuf; token lexbuf }
| digit+ as s
  { CST (Bigint.of_string s) }
| "let"
  { LET }
| "in"
  { IN }
| "fun"
  { FUN }
| "ifz"
  { IFZ }
| "fst"
  { FST }
| "snd"
  { SND }
| char+ as s
  { ID s }
| '+'
  { ADD }
| '-'
  { SUB }
| '('
  { LPAR }
| ')'
  { RPAR }
| ','
  { COMMA }
| '='
  { EQUAL }
| "->"
  { ARROW }
| _ as c
  { Error.error (region lexbuf) (Printf.sprintf "lexical error: %c" c) }
| eof
  { EOF }

{

  let parse ~filename lb =
    set_filename lb filename;
    try Ok (Parse.prog token lb) with
    | Parsing.Parse_error ->
      Error (Spectec.Error.TaskParseError (region lb, "syntax error"))
    | Error.MinimlParseError (at, msg) ->
      Error (Spectec.Error.TaskParseError (at, msg))

  let parse_file filename =
    let c = open_in filename in
    let lb = from_channel c in
    let r = parse ~filename lb in close_in c; r

  let parse_string ~spec:_ ~filename content =
    let lb = from_string content in
    Result.bind (parse ~filename lb) (fun e -> Ok [e])

}
