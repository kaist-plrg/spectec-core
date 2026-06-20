(** Hand-written lexer for impty.

    Produces a stream of tokens over the input string. Whitespace and comments
    (// to EOL, /* ... */) are skipped. *)

open Common.Source

type token =
  | KwInt
  | KwBool
  | KwSkip
  | KwIf
  | KwThen
  | KwElse
  | KwEnd
  | KwWhile
  | KwDo
  | KwFun
  | KwTrue
  | KwFalse
  | Semi
  | Eq
  | Plus
  | Leq
  | Bang
  | And
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Arrow
  | Num of int
  | Ident of string
  | Eof

let string_of_token = function
  | KwInt -> "int"
  | KwBool -> "bool"
  | KwSkip -> "skip"
  | KwIf -> "if"
  | KwThen -> "then"
  | KwElse -> "else"
  | KwEnd -> "end"
  | KwWhile -> "while"
  | KwDo -> "do"
  | KwFun -> "fun"
  | KwTrue -> "true"
  | KwFalse -> "false"
  | Semi -> ";"
  | Eq -> "="
  | Plus -> "+"
  | Leq -> "<="
  | Bang -> "!"
  | And -> "&&"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Arrow -> "->"
  | Num n -> string_of_int n
  | Ident s -> s
  | Eof -> "<eof>"

(* Lexer state: input string, current index, and 1-based line/column. *)

type state = {
  filename : string;
  source : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
}

let make ~filename source = { filename; source; pos = 0; line = 1; col = 1 }

let peek st =
  if st.pos >= String.length st.source then None else Some st.source.[st.pos]

let peek2 st =
  if st.pos + 1 >= String.length st.source then None
  else Some st.source.[st.pos + 1]

let advance st =
  if st.pos < String.length st.source then (
    if st.source.[st.pos] = '\n' then (
      st.line <- st.line + 1;
      st.col <- 1)
    else st.col <- st.col + 1;
    st.pos <- st.pos + 1)

let current_pos st : pos =
  { file = st.filename; line = st.line; column = st.col - 1 }

let region_from start_pos end_pos = { left = start_pos; right = end_pos }

(* Skip whitespace and comments. Returns when next char is start of a token. *)
let rec skip_trivia st =
  match peek st with
  | None -> ()
  | Some (' ' | '\t' | '\r' | '\n') ->
      advance st;
      skip_trivia st
  | Some '/' -> (
      match peek2 st with
      | Some '/' ->
          while peek st <> None && peek st <> Some '\n' do
            advance st
          done;
          skip_trivia st
      | Some '*' ->
          advance st;
          advance st;
          let rec consume () =
            match (peek st, peek2 st) with
            | None, _ ->
                Error.error
                  (region_from (current_pos st) (current_pos st))
                  "unterminated block comment"
            | Some '*', Some '/' ->
                advance st;
                advance st
            | _ ->
                advance st;
                consume ()
          in
          consume ();
          skip_trivia st
      | _ -> ())
  | _ -> ()

let is_digit c = c >= '0' && c <= '9'

let is_ident_start c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'

let is_ident_cont c = is_ident_start c || is_digit c

let lex_number st =
  let start = st.pos in
  while match peek st with Some c when is_digit c -> true | _ -> false do
    advance st
  done;
  let lexeme = String.sub st.source start (st.pos - start) in
  Num (int_of_string lexeme)

let keyword_of = function
  | "int" -> Some KwInt
  | "bool" -> Some KwBool
  | "skip" -> Some KwSkip
  | "if" -> Some KwIf
  | "then" -> Some KwThen
  | "else" -> Some KwElse
  | "end" -> Some KwEnd
  | "while" -> Some KwWhile
  | "do" -> Some KwDo
  | "fun" -> Some KwFun
  | "true" -> Some KwTrue
  | "false" -> Some KwFalse
  | _ -> None

let lex_ident st =
  let start = st.pos in
  while match peek st with Some c when is_ident_cont c -> true | _ -> false do
    advance st
  done;
  let lexeme = String.sub st.source start (st.pos - start) in
  match keyword_of lexeme with Some kw -> kw | None -> Ident lexeme

(* Lex one token, paired with its source region. *)
let next st : token * region =
  skip_trivia st;
  let left = current_pos st in
  let tok =
    match peek st with
    | None -> Eof
    | Some c when is_digit c -> lex_number st
    | Some c when is_ident_start c -> lex_ident st
    | Some ';' ->
        advance st;
        Semi
    | Some '+' ->
        advance st;
        Plus
    | Some '!' ->
        advance st;
        Bang
    | Some '(' ->
        advance st;
        Lparen
    | Some ')' ->
        advance st;
        Rparen
    | Some '{' ->
        advance st;
        Lbrace
    | Some '}' ->
        advance st;
        Rbrace
    | Some '=' ->
        advance st;
        Eq
    | Some '<' -> (
        match peek2 st with
        | Some '=' ->
            advance st;
            advance st;
            Leq
        | _ ->
            let here = region_from left (current_pos st) in
            Error.error here "expected '<=' (bare '<' is not used)")
    | Some '&' -> (
        match peek2 st with
        | Some '&' ->
            advance st;
            advance st;
            And
        | _ ->
            let here = region_from left (current_pos st) in
            Error.error here "expected '&&'")
    | Some '-' -> (
        match peek2 st with
        | Some '>' ->
            advance st;
            advance st;
            Arrow
        | Some d when is_digit d -> (
            advance st;
            match lex_number st with Num n -> Num (-n) | tok -> tok)
        | _ ->
            let here = region_from left (current_pos st) in
            Error.error here "expected '->' or a negative number")
    | Some c ->
        let here = region_from left (current_pos st) in
        Error.error here (Printf.sprintf "unexpected character %C" c)
  in
  let right = current_pos st in
  (tok, region_from left right)

(* Tokenize the entire input. *)
let tokenize ~filename (source : string) : (token * region) list =
  let st = make ~filename source in
  let rec loop acc =
    let tok, at = next st in
    let acc = (tok, at) :: acc in
    if tok = Eof then List.rev acc else loop acc
  in
  loop []
