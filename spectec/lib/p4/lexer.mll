(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
*)

{
open Lexing
open Context
open Il.Ast
open Util.Source
open Parser
open Concrete.Ast_utils
module F = Format

exception Error of string

let debug_channel = ref stderr
let set_debug_channel ch = debug_channel := ch
let lexer_debug_enabled () = Debug_config.lexer_debug_enabled Debug_config.Basic

let debug_print fmt =
  if Debug_config.lexer_debug_enabled Debug_config.Basic then
    Printf.fprintf !debug_channel fmt
  else
    Printf.ifprintf !debug_channel fmt

let debug_token lexeme =
  debug_print "%s" lexeme

let current_line  = ref 1 
let current_fname = ref ""
let line_start    = ref 1

type lexer_state =
  (* Nothing to recall from the previous tokens *)
  | SRegular
  | SRangle of Source.info
  | SPragma
  (* We have seen a template *)
  | STemplate
  (* We have seen an identifier:
   * we have just emitted a [NAME] token.
   * The next token will be either [IDENTIFIER] or [TYPENAME],
   * depending on what kind of identifier this is *)
  | SIdent of string * lexer_state
    let lexer_state = ref SRegular
    
let reset () =
  Context.reset ();
  lexer_state := SRegular;
  current_line := 1;
  current_fname := "";
  line_start := 1

let line_number () = !current_line
let filename () = !current_fname
let start_of_line () = !line_start

let set_line n =
  current_line := n

let set_start_of_line c =
  line_start := c

let set_filename s =
  current_fname := s

let set_lexer_debug_channel ch = set_debug_channel ch
let newline lexbuf =
  current_line := line_number() + 1 ;
  set_start_of_line (lexeme_end lexbuf)

let info lexbuf : Source.info =
  let f = filename () in
  let c1 = lexeme_start lexbuf in
  let c2 = lexeme_end lexbuf in
  let c = start_of_line () in
  let l = line_number() in
  Source.I 
    { filename = f;
      line_start = l;
      line_end = None;
      col_start = c1 - c;
      col_end = c2 - c }

let sanitize s =
  String.concat "" (String.split_on_char '_' s)

let strip_prefix s =
  let length = String.length s in
  assert (length > 2);
  String.sub s 2 (length - 2)

let parse_int n _info =
  let i = Bigint.of_string (sanitize n) in
  NumV (`Int i) |> with_typ (NumT `IntT)

let parse_width_int s n _info =
  let l_s = String.length s in
  let width = String.sub s 0 (l_s - 1) in
  let sign = String.sub s (l_s - 1) 1 in
  let i = Bigint.of_string (sanitize n) in
  let w = Bigint.of_string width in
  match sign with
    | "s" ->
      if (int_of_string width < 2)
      then raise (Error "signed integers must have width at least 2")
      else 
        let value_width =
          NumV (`Nat w) |> with_typ (NumT `NatT)
        in
        let value_int =
          NumV (`Int i) |> with_typ (NumT `IntT)
        in
        [ NT value_width; Term "S"; NT value_int ]
        |> wrap_case_v |> with_typ (wrap_var_t "number")
    | "w" ->
      let value_width =
        NumV (`Nat w) |> with_typ (NumT `NatT)
      in
      let value_int =
        NumV (`Int i) |> with_typ (NumT `IntT)
      in
      [ NT value_width; Term "W"; NT value_int ]
      |> wrap_case_v |> with_typ (wrap_var_t "number")
    | _ ->
      raise (Error "Illegal integer constant")
}

let name = [ 'A'-'Z' 'a'-'z' '_' ] [ 'A'-'Z' 'a'-'z' '0'-'9' '_' ]*
let hex_number = '0' [ 'x' 'X' ] [ '0'-'9' 'a'-'f' 'A'-'F' '_' ]+
let dec_number = '0' [ 'd' 'D' ] [ '0'-'9' '_' ]+
let oct_number = '0' [ 'o' 'O' ] [ '0'-'7' '_' ]+
let bin_number = '0' [ 'b' 'B' ] [ '0' '1' '_' ]+
let int = [ '0'-'9' ] [ '0'-'9' '_' ]*
let sign = [ '0'-'9' ]+ [ 'w' 's' ]

let whitespace = [ ' ' '\t' '\012' '\r' ]

rule tokenize = parse
  | "/*"
      { debug_token "/*";
        match multiline_comment None lexbuf with 
       | None -> tokenize lexbuf
       | Some info -> PRAGMA_END (info) }
  | "//"
      { singleline_comment lexbuf; tokenize lexbuf }
  | '\n'
      { debug_token "⏎\n"; newline lexbuf; PRAGMA_END (info lexbuf) }
  | '"'
      { let str, end_info = (string lexbuf) in
        debug_token ("\"" ^ str ^ "\"");
        end_info |> ignore;
        let value = 
          TextV str $$$ { vid = Concrete.Value.fresh (); typ = TextT }
        in
        STRING_LITERAL value
      }
  | whitespace
      { debug_token " "; tokenize lexbuf }
  | '#'
      { debug_token ""; preprocessor lexbuf ; tokenize lexbuf }
  | "@pragma"
      { debug_token "@pragma"; PRAGMA (info lexbuf) }
  | hex_number as n
      { debug_token n; NUMBER_INT (parse_int n (info lexbuf), n) }
  | dec_number as n
      { debug_token n; NUMBER_INT (parse_int (strip_prefix n) (info lexbuf), n) }
  | oct_number as n
      { debug_token n; NUMBER_INT (parse_int n (info lexbuf), n) }
  | bin_number as n
      { debug_token n; NUMBER_INT (parse_int n (info lexbuf), n) }
  | int as n
      { debug_token n; NUMBER_INT (parse_int n (info lexbuf), n) }
  | (sign as s) (hex_number as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | (sign as s) (dec_number as n)
      { NUMBER (parse_width_int s (strip_prefix n) (info lexbuf), n) }
  | (sign as s) (oct_number as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | (sign as s) (bin_number as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | (sign as s) (int as n)
      { NUMBER (parse_width_int s n (info lexbuf), n) }
  | "abstract"
      { debug_token "abstract"; ABSTRACT (info lexbuf) }
  | "action"
      { debug_token "action"; ACTION (info lexbuf) }
  | "actions"
      { debug_token "actions"; ACTIONS (info lexbuf) }
  | "apply"
      { debug_token "apply"; APPLY (info lexbuf) }
  | "bool"
      { debug_token "bool"; BOOL (info lexbuf) }
  | "bit"
      { debug_token "bit"; BIT (info lexbuf) }
  | "break"
      { debug_token "break"; BREAK (info lexbuf) }
  | "const"
      { debug_token "const"; CONST (info lexbuf) }
  | "continue"
      { debug_token "continue"; CONTINUE (info lexbuf) }
  | "control"
      { debug_token "control"; CONTROL (info lexbuf) }
  | "default"
      { debug_token "default"; DEFAULT (info lexbuf) }
  | "else"
      { debug_token "else"; ELSE (info lexbuf) }
  | "entries"
      { debug_token "entries"; ENTRIES (info lexbuf) }
  | "enum"
      { debug_token "enum"; ENUM (info lexbuf) }
  | "error"
      { debug_token "error"; ERROR (info lexbuf) }
  | "exit"
      { debug_token "exit"; EXIT (info lexbuf) }
  | "extern"
      { debug_token "extern"; EXTERN (info lexbuf) }
  | "header"
      { debug_token "header"; HEADER (info lexbuf) }
  | "header_union"
      { debug_token "header_union"; HEADER_UNION (info lexbuf) }
  | "true"
      { debug_token "true"; TRUE (info lexbuf) }
  | "false"
      { debug_token "false"; FALSE (info lexbuf) }
  | "for"
      { debug_token "for"; FOR (info lexbuf) }
  | "if"
      { debug_token "if"; IF (info lexbuf) }
  | "in"
      { debug_token "in"; IN (info lexbuf) }
  | "inout"
      { debug_token "inout"; INOUT (info lexbuf) }
  | "int"
      { debug_token "int"; INT (info lexbuf) }
  | "key"
      { debug_token "key"; KEY (info lexbuf) }
  | "list"
      { debug_token "list"; LIST (info lexbuf) }
  | "match_kind"
      { debug_token "match_kind"; MATCH_KIND (info lexbuf) }
  | "out"
      { debug_token "out"; OUT (info lexbuf) }
  | "parser"
      { debug_token "parser"; PARSER (info lexbuf) }
  | "package"
      { debug_token "package"; PACKAGE (info lexbuf) }
  | "pragma" 
      { debug_token "pragma"; PRAGMA (info lexbuf) }
  | "priority"
      { debug_token "priority"; PRIORITY (info lexbuf) }
  | "return"
      { debug_token "return"; RETURN (info lexbuf) }
  | "select"
      { debug_token "select"; SELECT (info lexbuf) }
  | "state"
      { debug_token "state"; STATE (info lexbuf) }
  | "string"
      { debug_token "string"; STRING (info lexbuf) }
  | "struct"
      { debug_token "struct"; STRUCT (info lexbuf) }
  | "switch"
      { debug_token "switch"; SWITCH (info lexbuf) }
  | "table"
      { debug_token "table"; TABLE (info lexbuf) }
  | "this"
      { debug_token "this"; THIS (info lexbuf) }  
  | "transition"
      { debug_token "transition"; TRANSITION (info lexbuf) }
  | "tuple"
      { debug_token "tuple"; TUPLE (info lexbuf) }
  | "typedef"
      { debug_token "typedef"; TYPEDEF (info lexbuf) }
  | "type"
      { debug_token "type"; TYPE (info lexbuf) }
  | "value_set"
      { debug_token "value_set"; VALUE_SET (info lexbuf) }
  | "varbit"
      { debug_token "varbit"; VARBIT (info lexbuf) }
  | "void"
      { debug_token "void"; VOID (info lexbuf) }
  | "_"
      { debug_token "_"; DONTCARE (info lexbuf) }
  | name
      { let text = Lexing.lexeme lexbuf in
        debug_token text;
        let value =
          let vid = Concrete.Value.fresh () in
          let typ = Il.Ast.TextT in
          TextV text $$$ { vid; typ }
        in
        NAME value }
  | "<="
      { debug_token "<="; LE (info lexbuf) }
  | ">="
      { debug_token ">="; GE (info lexbuf) }
  | "<<"
      { debug_token "<<"; SHL (info lexbuf) }
  | "&&"
      { debug_token "&&"; AND (info lexbuf) }
  | "||"
      { debug_token "||"; OR (info lexbuf) }
  | "!="
      { debug_token "!="; NE (info lexbuf) }
  | "=="
      { debug_token "=="; EQ (info lexbuf) }
  | "+"
      { debug_token "+"; PLUS (info lexbuf) }
  | "-"
      { debug_token "-"; MINUS (info lexbuf) }
  | "|+|"
      { debug_token "|+|"; PLUS_SAT (info lexbuf) }
  | "|-|"
      { debug_token "|-|"; MINUS_SAT (info lexbuf) }
  | "*"
      { debug_token "*"; MUL (info lexbuf) }
  | "{#}"
      { debug_token "{#}"; INVALID (info lexbuf) }
  | "/"
      { debug_token "/"; DIV (info lexbuf) }
  | "%"
      { debug_token "%"; MOD (info lexbuf) }
  | "|"
      { debug_token "|"; BIT_OR (info lexbuf) }
  | "&"
      { debug_token "&"; BIT_AND (info lexbuf) }
  | "^"
      { debug_token "^"; BIT_XOR (info lexbuf) }
  | "~"
      { debug_token "~"; COMPLEMENT (info lexbuf) }
  | "["
      { debug_token "["; L_BRACKET (info lexbuf) }
  | "]"
      { debug_token "]"; R_BRACKET (info lexbuf) }
  | "{"
      { debug_token "{"; L_BRACE (info lexbuf) }
  | "}"
      { debug_token "}"; R_BRACE (info lexbuf) }
  | "<"
      { debug_token "<"; L_ANGLE (info lexbuf) }
  | ">"
      { debug_token ">"; R_ANGLE (info lexbuf) }
  | "("
      { debug_token "("; L_PAREN (info lexbuf) }
  | ")"
      { debug_token ")"; R_PAREN (info lexbuf) }
  | "!"
      { debug_token "!"; NOT (info lexbuf) }
  | ":"
      { debug_token ":"; COLON (info lexbuf) }
  | ","
      { debug_token ","; COMMA (info lexbuf) }
  | "?"
      { debug_token "?"; QUESTION (info lexbuf) }
  | "."
      { debug_token "."; DOT (info lexbuf) }
  | "="
      { debug_token "="; ASSIGN (info lexbuf) }
  | ";"
      { debug_token ";"; SEMICOLON (info lexbuf) }
  | "@"
      { debug_token "@"; AT (info lexbuf) }
  | "++"
      { debug_token "++"; PLUSPLUS (info lexbuf) }
  | "&&&"
      { debug_token "&&&"; MASK (info lexbuf) }
  | "..."
      { debug_token "..."; DOTS (info lexbuf) }
  | ".."
      { debug_token ".."; RANGE (info lexbuf) }
  | "+="
      { debug_token "+="; PLUS_ASSIGN (info lexbuf) }
  | "|+|="
      { debug_token "|+|="; PLUS_SAT_ASSIGN (info lexbuf) }
  | "-="
      { debug_token "-="; MINUS_ASSIGN (info lexbuf) }
  | "|-|="
      { debug_token "|-|="; MINUS_SAT_ASSIGN (info lexbuf) }
  | "*="
      { debug_token "*="; MUL_ASSIGN (info lexbuf) }
  | "/="
      { debug_token "/="; DIV_ASSIGN (info lexbuf) } 
  | "%="
      { debug_token "%="; MOD_ASSIGN (info lexbuf) }
  | "<<="
      { debug_token "<<="; SHL_ASSIGN (info lexbuf) }
  | ">>="
      { debug_token ">>="; SHR_ASSIGN (info lexbuf) }
  | "&="
      { debug_token "&="; BIT_AND_ASSIGN (info lexbuf) }
  | "^="
      { debug_token "^="; BIT_XOR_ASSIGN (info lexbuf) }
  | "|="
      { debug_token "|="; BIT_OR_ASSIGN (info lexbuf) }
  | eof
      { debug_token "EOF"; END (info lexbuf) }
  | _
      { let text = lexeme lexbuf in
        debug_token text;
        let value =
          let vid = Concrete.Value.fresh () in
          let typ = Il.Ast.TextT in
          TextV text $$$ { vid; typ }
        in
        UNEXPECTED_TOKEN value }
      
and string = parse
  | eof
      { raise (Error "File ended while reading a string literal") }
  | "\\\""
      { let rest, end_info = (string lexbuf) in
        ("\"" ^ rest, end_info) }
  | '\\' 'n'
      { let rest, end_info = (string lexbuf) in
        ("\n" ^ rest, end_info) }
  | '\\' '\\'
      { let rest, end_info = (string lexbuf) in
        ("\\" ^ rest, end_info) }
  | '\\' _ as c
      { raise (Error ("Escape sequences not yet supported: \\" ^ c)) }
  | '"'
      { ("", info lexbuf) }
  | _ as chr
      { let rest, end_info = (string lexbuf) in
        ((String.make 1 chr) ^ rest, end_info) }
    
(* Preprocessor annotations indicate line and filename *)
and preprocessor = parse
  | ' '
      { preprocessor lexbuf }
  | int
      { let line = int_of_string (lexeme lexbuf) in
        set_line line ; preprocessor lexbuf }
  | '"'
      { preprocessor_string lexbuf }
  | '\n'
      { set_start_of_line (lexeme_end lexbuf) }
  | _
      { preprocessor lexbuf }
      
and preprocessor_string = parse
  | [^ '"'] * '"'
    { let filename = lexeme lexbuf in 
      let filename = String.sub filename 0 (String.length filename - 1) in
      set_filename filename;
      preprocessor_column lexbuf }
      
(* Once a filename has been recognized, ignore the rest of the line *)
and preprocessor_column = parse
  | ' ' 
      { preprocessor_column lexbuf }
  | '\n'
      { set_start_of_line (lexeme_end lexbuf) }
  | eof
      { () }
  | _
      { preprocessor_column lexbuf }
      
(* Multi-line comment terminated by "*/" *)
and multiline_comment opt = parse
  | "*/"   { opt }
  | eof    { failwith "unterminated comment" }
  | '\n'   { newline lexbuf; multiline_comment (Some(info lexbuf)) lexbuf }
  | _      { multiline_comment opt lexbuf }
      
(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { newline lexbuf }
  | eof    { () }
  | _      { singleline_comment lexbuf }
      
{
let rec lexer (lexbuf:lexbuf): token = 
   match !lexer_state with
    | SIdent(id, next) ->
      begin match get_kind id with
      | TypeName true ->
        lexer_state := STemplate;
        TYPENAME
      | Ident true ->
        lexer_state := STemplate;
        IDENTIFIER
      | TypeName false ->
        lexer_state := next;
        TYPENAME
      | Ident false ->
        lexer_state := next;
        IDENTIFIER
      end
    | SRangle info1 -> 
      begin match tokenize lexbuf with
      | R_ANGLE info2 when Source.follows info1 info2 -> 
        lexer_state := SRegular;
        R_ANGLE_SHIFT info2
      | PRAGMA _ as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END _ -> 
        lexer_state := SRegular;
        lexer lexbuf
      | NAME value as token ->
        let text = Concrete.Value.get_text value in
        lexer_state := SIdent (text, SRegular);
        token          
      | token -> 
        lexer_state := SRegular;
        token
      end
    | SRegular ->
      begin match tokenize lexbuf with
      | NAME value as token ->
        let text = Concrete.Value.get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA _ as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END _ ->
        lexer lexbuf
      | R_ANGLE info as token -> 
        lexer_state := SRangle info;
        token
      | token ->
        lexer_state := SRegular;
        token
       end
    | STemplate ->
      begin match tokenize lexbuf with
      | L_ANGLE info -> L_ANGLE_ARGS info
      | NAME value as token ->
        let text = Concrete.Value.get_text value in
        lexer_state := SIdent (text, SRegular);
        token
      | PRAGMA _ as token ->
        lexer_state := SPragma;
        token
      | PRAGMA_END _ -> lexer lexbuf
      | R_ANGLE info as token -> 
        lexer_state := SRangle info;
        token
      | token ->
        lexer_state := SRegular;
        token
       end
    | SPragma -> 
      begin match tokenize lexbuf with
      | PRAGMA_END _info as token -> 
         lexer_state := SRegular;
         token
      | NAME value as token ->
         let text = Concrete.Value.get_text value in
         lexer_state := SIdent(text, SPragma);
         token
      | token -> token
      end
}


