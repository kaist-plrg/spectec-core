(** Recursive-descent parser for impty.

    Consumes the token stream from {!Lexer} and emits an IL [value] tree shaped
    to match the surface syntax in [specs/impty/*/spec.spectec]. Precedence is
    encoded by the call structure of [parse_expr] and friends. *)

open Common.Source
open Lang.Il
open Lang.Il.Value

(* ----------------------------------------------------------------- *)
(* Token cursor                                                       *)
(* ----------------------------------------------------------------- *)

(* The cursor never advances past Eof; the trailing Eof token gives a stable
   region for error reporting at end of file. *)
module Cursor = struct
  type t = { mutable tokens : (Lexer.token * region) list }

  let make tokens = { tokens }
  let peek c = match c.tokens with [] -> assert false | (t, _) :: _ -> t
  let region c = match c.tokens with [] -> assert false | (_, at) :: _ -> at
  let peek2 c = match c.tokens with _ :: (t, _) :: _ -> t | _ -> Lexer.Eof

  let advance c =
    match c.tokens with
    | (Lexer.Eof, _) :: _ -> ()
    | _ :: rest -> c.tokens <- rest
    | [] -> assert false

  let expect c expected =
    let tok = peek c in
    if tok = expected then advance c
    else
      Error.error (region c)
        (Printf.sprintf "expected %s, got %s"
           (Lexer.string_of_token expected)
           (Lexer.string_of_token tok))

  let expect_ident c =
    match peek c with
    | Lexer.Ident s ->
        advance c;
        s
    | tok ->
        Error.error (region c)
          (Printf.sprintf "expected identifier, got %s"
             (Lexer.string_of_token tok))
end

(* ----------------------------------------------------------------- *)
(* Combinators                                                        *)
(* ----------------------------------------------------------------- *)

let surrounded ~left ~right p c =
  Cursor.expect c left;
  let v = p c in
  Cursor.expect c right;
  v

(* [left_assoc ~op_token ~op_atom ~var ~next c] left-folds [next] over
   repeated [op_token]s, building [next op next op ... op next] as a chain
   of binary case_v values under [var]. *)
let left_assoc ~op_token ~op_atom ~var ~next c =
  let lhs = next c in
  let rec loop acc =
    if Cursor.peek c = op_token then (
      Cursor.advance c;
      let rhs = next c in
      loop ([ arg acc; atom op_atom; arg rhs ] |> case_v ~var))
    else acc
  in
  loop lhs

(* ----------------------------------------------------------------- *)
(* IL value constructors                                              *)
(* ----------------------------------------------------------------- *)

let v_id (s : string) : value = [ atom "`ID"; arg (text s) ] |> case_v ~var:"id"

let v_lit_num (n : int) : value =
  [ atom "`NUM"; arg (nat (Bigint.of_int n)) ] |> case_v ~var:"literal"

let v_lit_bool (b : bool) : value =
  [ atom "`BOOL"; arg (bool b) ] |> case_v ~var:"literal"

(* ----------------------------------------------------------------- *)
(* Types                                                              *)
(* ----------------------------------------------------------------- *)

(* Function-type arrow is right-associative: `int -> int -> int` parses as
   `int -> (int -> int)`. *)
let rec parse_type c : value =
  let lhs = parse_type_atom c in
  if Cursor.peek c = Lexer.Arrow then (
    Cursor.advance c;
    let rhs = parse_type c in
    [ arg lhs; atom "`->"; arg rhs ] |> case_v ~var:"type")
  else lhs

and parse_type_atom c : value =
  match Cursor.peek c with
  | Lexer.KwInt ->
      Cursor.advance c;
      [ atom "INT" ] |> case_v ~var:"type"
  | Lexer.KwBool ->
      Cursor.advance c;
      [ atom "BOOL" ] |> case_v ~var:"type"
  | Lexer.Lparen ->
      surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen parse_type c
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "expected type, got %s" (Lexer.string_of_token tok))

(* ----------------------------------------------------------------- *)
(* Expressions                                                        *)
(* ----------------------------------------------------------------- *)

(* Precedence (loose to tight): ?: , && , <= , + , unary ! , postfix call,
   primary. Binary operators fold left; the ternary is looser than all of them,
   and its else-branch recurses so `a ? b : c ? d : e` reads as
   `a ? b : (c ? d : e)`. The `?`/`:` atoms are Quest and Colon `Tick, matching
   the spec's `expr `? expr `: expr`. *)

let rec parse_expr c : value = parse_ternary c

and parse_ternary c : value =
  let cond = parse_and c in
  if Cursor.peek c = Lexer.Question then (
    Cursor.advance c;
    let e_then = parse_expr c in
    Cursor.expect c Lexer.Colon;
    let e_else = parse_ternary c in
    [ arg cond; atom "?"; arg e_then; atom "`:"; arg e_else ]
    |> case_v ~var:"expr")
  else cond

and parse_and c =
  left_assoc ~op_token:Lexer.And ~op_atom:"&&" ~var:"expr" ~next:parse_leq c

and parse_leq c =
  left_assoc ~op_token:Lexer.Leq ~op_atom:"<=" ~var:"expr" ~next:parse_add c

and parse_add c =
  left_assoc ~op_token:Lexer.Plus ~op_atom:"+" ~var:"expr" ~next:parse_unary c

and parse_unary c : value =
  if Cursor.peek c = Lexer.Bang then (
    Cursor.advance c;
    let e = parse_unary c in
    [ atom "!"; arg e ] |> case_v ~var:"expr")
  else parse_postfix c

(* A primary may be followed by zero or more `(expr)` applications. *)
and parse_postfix c : value =
  let base = parse_primary c in
  let rec loop acc =
    if Cursor.peek c = Lexer.Lparen then
      let arg_e =
        surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen parse_expr c
      in
      loop ([ arg acc; atom "("; arg arg_e; atom ")" ] |> case_v ~var:"expr")
    else acc
  in
  loop base

and parse_primary c : value =
  match Cursor.peek c with
  | Lexer.Num n ->
      Cursor.advance c;
      v_lit_num n
  | Lexer.KwTrue ->
      Cursor.advance c;
      v_lit_bool true
  | Lexer.KwFalse ->
      Cursor.advance c;
      v_lit_bool false
  | Lexer.Ident s ->
      Cursor.advance c;
      v_id s
  | Lexer.Lparen ->
      surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen parse_expr c
  | Lexer.KwFun ->
      Cursor.advance c;
      let t_arg, id_str =
        surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen
          (fun c ->
            let t = parse_type c in
            (t, Cursor.expect_ident c))
          c
      in
      Cursor.expect c Lexer.Arrow;
      let t_ret = parse_type c in
      let body =
        surrounded ~left:Lexer.Lbrace ~right:Lexer.Rbrace parse_expr c
      in
      [
        atom "FUN";
        atom "(";
        arg t_arg;
        arg (v_id id_str);
        atom ")";
        atom "`->";
        arg t_ret;
        atom "{";
        arg body;
        atom "}";
      ]
      |> case_v ~var:"expr"
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "expected expression, got %s"
           (Lexer.string_of_token tok))

(* ----------------------------------------------------------------- *)
(* Statements                                                         *)
(* ----------------------------------------------------------------- *)

(* Sequencing is left-associative via `;`. *)
and parse_stmt c =
  left_assoc ~op_token:Lexer.Semi ~op_atom:"`;" ~var:"command"
    ~next:parse_stmt_atom c

and parse_stmt_atom c : value =
  match Cursor.peek c with
  | Lexer.KwSkip ->
      Cursor.advance c;
      [ atom "SKIP" ] |> case_v ~var:"command"
  | Lexer.KwIf ->
      Cursor.advance c;
      let cond = parse_expr c in
      Cursor.expect c Lexer.KwThen;
      let s1 = parse_stmt c in
      Cursor.expect c Lexer.KwElse;
      let s2 = parse_stmt c in
      Cursor.expect c Lexer.KwEnd;
      [
        atom "IF";
        arg cond;
        atom "THEN";
        arg s1;
        atom "ELSE";
        arg s2;
        atom "END";
      ]
      |> case_v ~var:"command"
  | Lexer.KwWhile ->
      Cursor.advance c;
      let cond = parse_expr c in
      Cursor.expect c Lexer.KwDo;
      let body = parse_stmt c in
      Cursor.expect c Lexer.KwEnd;
      [ atom "WHILE"; arg cond; atom "DO"; arg body; atom "END" ]
      |> case_v ~var:"command"
  | Lexer.KwRec ->
      Cursor.advance c;
      let fname = Cursor.expect_ident c in
      let t_arg, x_str =
        surrounded ~left:Lexer.Lparen ~right:Lexer.Rparen
          (fun c ->
            let t = parse_type c in
            (t, Cursor.expect_ident c))
          c
      in
      Cursor.expect c Lexer.Arrow;
      let t_ret = parse_type c in
      let body =
        surrounded ~left:Lexer.Lbrace ~right:Lexer.Rbrace parse_expr c
      in
      [
        atom "REC";
        arg (v_id fname);
        atom "(";
        arg t_arg;
        arg (v_id x_str);
        atom ")";
        atom "`->";
        arg t_ret;
        atom "{";
        arg body;
        atom "}";
      ]
      |> case_v ~var:"command"
  | Lexer.KwInt | Lexer.KwBool -> parse_var_decl c
  | Lexer.Lparen
    when Cursor.peek2 c = Lexer.KwInt || Cursor.peek2 c = Lexer.KwBool ->
      parse_var_decl c
  | Lexer.Ident _ when Cursor.peek2 c = Lexer.Eq ->
      let id_str = Cursor.expect_ident c in
      Cursor.expect c Lexer.Eq;
      let rhs = parse_expr c in
      [ arg (v_id id_str); atom "="; arg rhs ] |> case_v ~var:"command"
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "expected a command, got %s"
           (Lexer.string_of_token tok))

(* Surface form `T id = e`.  No trailing `;` is consumed: that `;` is the
   stmt-sequence separator parsed by [parse_stmt]. *)
and parse_var_decl c : value =
  let t = parse_type c in
  let id_str = Cursor.expect_ident c in
  Cursor.expect c Lexer.Eq;
  let rhs = parse_expr c in
  [ arg t; arg (v_id id_str); atom "="; arg rhs ] |> case_v ~var:"command"

(* ----------------------------------------------------------------- *)
(* Entry points                                                       *)
(* ----------------------------------------------------------------- *)

(* `prog = stmt`, so a program is just a stmt — no enclosing block. *)
let parse_prog c : value = parse_stmt c

(* Raises [Error.ImptyParseError] on failure. *)
let parse_string_exn ~filename (source : string) : value =
  let c = Cursor.make (Lexer.tokenize ~filename source) in
  let v = parse_prog c in
  (match Cursor.peek c with
  | Lexer.Eof -> ()
  | tok ->
      Error.error (Cursor.region c)
        (Printf.sprintf "unexpected trailing token %s"
           (Lexer.string_of_token tok)));
  v

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

(* ----------------------------------------------------------------- *)
(* Public facade — exception → result mapping for Spectec.Task.S      *)
(* ----------------------------------------------------------------- *)

let parse_file ~handler filename =
  handler @@ fun () ->
  try Ok (parse_string_exn ~filename (read_file filename))
  with Error.ImptyParseError (at, msg) ->
    Error (Spectec.Error.TaskParseError (at, msg))

let parse_string ~spec:_ ~filename content =
  try Ok [ parse_string_exn ~filename content ]
  with Error.ImptyParseError (at, msg) ->
    Error (Spectec.Error.TaskParseError (at, msg))

(* For now, just dump the IL value structure — good enough for debugging and
   round-trip inspection. A surface-syntax pretty printer can come later. *)
let unparse ~spec:_ values =
  values |> List.map Lang.Il.Print_debug.string_of_value |> String.concat "\n"
