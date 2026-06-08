{
module Source = Common.Source
open Lang.Xl
open Parser
open Diagnostic

(* Error handling *)

let convert_pos pos =
  Source.{
    file = pos.Lexing.pos_fname;
    line = pos.Lexing.pos_lnum;
    column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  }

let region lexbuf =
  let left = convert_pos (Lexing.lexeme_start_p lexbuf) in
  let right = convert_pos (Lexing.lexeme_end_p lexbuf) in
  Source.{ left; right }

let error ?code ?detail ?related lexbuf msg = error ?code ?detail ?related (region lexbuf) msg
let error_nest ?code ?detail ?related start lexbuf msg =
  lexbuf.Lexing.lex_start_p <- start;
  error ?code ?detail ?related lexbuf msg

(* Numbers *)

let nat _lexbuf s = Bigint.of_string s
let hex _lexbuf s = Bigint.of_string s
let int lexbuf s =
  try int_of_string s
  with Failure _ ->
    error ~code:Hole_index_overflow
      ~detail:"The hole index in `%N` must fit in a 63-bit native integer."
      lexbuf "hole index out of range"

(* Texts *)

let text _lexbuf s =
  let b = Buffer.create (String.length s) in
  let i = ref 1 in
  while !i < String.length s - 1 do
    let c = if s.[!i] <> '\\' then s.[!i] else
      match (incr i; s.[!i]) with
      | 'n' -> '\n'
      | 'r' -> '\r'
      | 't' -> '\t'
      | '\\' -> '\\'
      | '\'' -> '\''
      | '\"' -> '\"'
      | 'u' ->
          let j = !i + 2 in
          i := String.index_from s j '}';
          let n = int_of_string ("0x" ^ String.sub s j (!i - j)) in
          let bs = Utf8.encode [n] in
          Buffer.add_substring b bs 0 (String.length bs - 1);
          bs.[String.length bs - 1]
      | h ->
          incr i;
          Char.chr (int_of_string ("0x" ^ String.make 1 h ^ String.make 1 s.[!i]))
    in
    Buffer.add_char b c;
    incr i
  done;
  Buffer.contents b

(* Identifiers : a hack to access parser state *)

let is_var s =
  let s = [| UPID s; EOF |] in
  let i = ref (-1) in
  Parser.check_atom (fun _ -> incr i; s.(!i)) (Lexing.from_string "")
}

(* Numbers *)

let digit = ['0'-'9']
let nat = digit ('_'? digit)*

let sign = '+' | '-'
let int = sign nat

let hexdigit = ['0'-'9''A'-'F']
let hex = hexdigit ('_'? hexdigit)*

(* Identifiers *)

let upletter = ['A'-'Z']
let loletter = ['a'-'z']
let letter = upletter | loletter

let idchar = letter | digit | '_' | '\''
let upid = upletter idchar*
let loid = (loletter | '_') idchar*
let id = upid | loid

let atomid = upid
let typid = loid
let expid = loid
let defid = id
let metaid = id

let symbol = ['+''-''*''/''\\''^''~''=''<''>''!''?''@''#''$''%''&''|'':''`''.''\'']

(* Texts *)

let space = [' ''\t''\n''\r']
let control = ['\x00'-'\x1f''\x7f'] # space
let ascii = ['\x00'-'\x7f']
let printable = ascii # control
let ascii_no_nl = ascii # '\x0a'
let utf8cont = ['\x80'-'\xbf']
let utf8enc =
    ['\xc2'-'\xdf'] utf8cont
  | ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xed'] ['\x80'-'\x9f'] utf8cont
  | ['\xe1'-'\xec''\xee'-'\xef'] utf8cont utf8cont
  | ['\xf0'] ['\x90'-'\xbf'] utf8cont utf8cont
  | ['\xf4'] ['\x80'-'\x8f'] utf8cont utf8cont
  | ['\xf1'-'\xf3'] utf8cont utf8cont utf8cont
let utf8 = ascii | utf8enc
let utf8_no_nl = ascii_no_nl | utf8enc

let escape = ['n''r''t''\\''\'''\"']
let character =
    [^'"''\\''\x00'-'\x1f''\x7f'-'\xff']
  | utf8enc
  | '\\'escape
  | '\\'hexdigit hexdigit
  | "\\u{" hex '}'
let text = '"' character* '"'

(* Indentation *)

let indent = [' ''\t']

(* Comments *)

let line_comment = ";;"utf8_no_nl*

(* Lexer rules *)

rule after_nl = parse
  | indent* "|"[' ''\t'] { NL_BAR }
  | indent* '\n' { Lexing.new_line lexbuf; after_nl_nl lexbuf }
  | "" { token lexbuf }

and after_nl_nl = parse
  | indent* "|"[' ''\t'] { NL_BAR }
  | indent* '\n' { Lexing.new_line lexbuf; NL3 }
  | indent* line_comment '\n' { Lexing.new_line lexbuf; after_nl_nl lexbuf }
  | indent* line_comment? eof { EOF }
  | "" { NL2 }

and token = parse
  (* escaped tokens *)
  | "_"(upid as s) { TAG_UPID s }
  | "'" ([^'\'''\n']* as s) "'" { OPERATOR s }
  (* matched grouping brackets: backtick on both delimiters *)
  | "`<" { TICK_LANGLE }
  | "`>" { TICK_RANGLE }
  | "`(" { TICK_LPAREN }
  | "`)" { TICK_RPAREN }
  | "`[" { TICK_LBRACK }
  | "`]" { TICK_RBRACK }
  | "`{" { TICK_LBRACE }
  | "`}" { TICK_RBRACE }
  (* normal tokens *)
  | "|-" { TURNSTILE }
  | "-|" { TILESTURN }
  | "->" { ARROW }
  | "->_" { ARROW_SUB }
  | "=>" { DOUBLE_ARROW }
  | "=>_" { DOUBLE_ARROW_SUB }
  | "<=>" { DOUBLE_ARROW_BOTH }
  | "==>" { DOUBLE_ARROW_LONG }
  | "~>" { SQARROW }
  | "~>*" { SQARROW_STAR }
  | "/\\" { AND }
  | "\\/" { OR }
  | "." { DOT }
  | ".." { DOT2 }
  | "..." { DOT3 }
  | "," { COMMA }
  | "," indent* line_comment? '\n' { Lexing.new_line lexbuf; COMMA_NL }
  | ";" { SEMICOLON }
  | ":" { COLON }
  | "::" { COLON2 }
  | ":/" { COLON_SLASH }
  | ":=" { COLON_EQ }
  | "#" { HASH }
  | "##" { HASH2 }
  | "$" { DOLLAR }
  | "?" { QUEST }
  | "<:" { SUB }
  | "~" { TILDE }
  | "~~" { TILDE2 }
  | "<" { LANGLE }
  | "<-" { LANGLE_DASH }
  | "<=" { LANGLE_EQ }
  | ">" { RANGLE }
  | ">=" { RANGLE_EQ }
  | ">(" { RANGLE_LPAREN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "+" { PLUS }
  | "++" { PLUS2 }
  | "-" { MINUS }
  | "--" { DASH }
  | "*" { STAR }
  | "/" { SLASH }
  | "\\" { BACKSLASH }
  | "%" { HOLE }
  | "%"(nat as s) { HOLE_NUM (int lexbuf s) }
  | "%%" { HOLE_MULTI }
  | "!%" { HOLE_NIL }
  | "=" { EQ }
  | "=/=" { NEQ }
  | "^" { UP }
  | "|" { BAR }
  | line_comment? '\n' { Lexing.new_line lexbuf; after_nl lexbuf }
  | "%latex" { LATEX }
  | "bool" { BOOL }
  | "nat" { NAT }
  | "int" { INT }
  | "text" { TEXT }
  | "syntax" { SYNTAX }
  | "relation" { RELATION }
  | "rule" { RULE }
  | "var" { VAR }
  | "builtin" { BUILTIN }
  | "dec" { DEC }
  | "def" { DEF }
  | "if" { IF }
  | "otherwise" { OTHERWISE }
  | "debug" { DEBUG }
  | "hint(" { HINT_LPAREN }
  | "eps" { EPS }
  | "true" { BOOLLIT true }
  | "false" { BOOLLIT false }
  | nat as s { NATLIT (nat lexbuf s) }
  | ("0x" hex) as s { HEXLIT (hex lexbuf s) }
  | text as s { TEXTLIT (text lexbuf s) }
  | '"'character*('\n'|eof) { error ~code:Unclosed_text_literal lexbuf "unclosed text literal" }
  | '"'character*['\x00'-'\x09''\x0b'-'\x1f''\x7f']
    { error ~code:Illegal_control_in_text_literal lexbuf "illegal control character in text literal" }
  | '"'character*'\\'_
    { error_nest ~code:Illegal_escape (Lexing.lexeme_end_p lexbuf) lexbuf "illegal escape" }
  | upid as s { if is_var s then LOID s else UPID s }
  | loid as s { LOID s }
  | (upid as s) "(" { if is_var s then LOID_LPAREN s else UPID_LPAREN s }
  | (loid as s) "(" { LOID_LPAREN s }
  | (upid as s) "<" { if is_var s then LOID_LANGLE s else UPID_LANGLE s }
  | (loid as s) "<" { LOID_LANGLE s }
  | "."(id as s) { DOTID s }
  | line_comment eof { EOF }
  | line_comment '\n' { Lexing.new_line lexbuf; token lexbuf }
  | line_comment { token lexbuf (* causes error on following position *) }
  | "(;" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; token lexbuf }
  | space#'\n' { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | "\\\n" { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
  | printable { error ~code:Stray_printable lexbuf "malformed token" }
  | control { error ~code:Stray_control_char lexbuf "misplaced control character" }
  | utf8enc { error ~code:Stray_non_ascii_char lexbuf "misplaced unicode character" }
  | _ { error ~code:Invalid_utf8 lexbuf "malformed UTF-8 encoding" }

and comment start = parse
  | ";)" { () }
  | "(;" { comment (Lexing.lexeme_start_p lexbuf) lexbuf; comment start lexbuf }
  | "\n" { Lexing.new_line lexbuf; comment start lexbuf }
  | utf8_no_nl { comment start lexbuf }
  | eof { error_nest ~code:Unclosed_block_comment start lexbuf "unclosed comment" }
  | _ { error ~code:Invalid_utf8_in_comment lexbuf "malformed UTF-8 encoding" }
