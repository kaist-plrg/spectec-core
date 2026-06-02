open Common.Source

type error = Diag.t
type 'a result = ('a, error) Stdlib.result

exception ParseError of Diag.t

type code =
  (* lexer: bare-byte / encoding rejections *)
  | Stray_printable
  | Stray_control_char
  | Stray_non_ascii_char
  | Invalid_utf8
  (* lexer: literals *)
  | Hole_index_overflow
  | Unclosed_text_literal
  | Illegal_control_in_text_literal
  | Illegal_escape
  (* lexer: comments *)
  | Unclosed_block_comment
  | Invalid_utf8_in_comment
  (* parser: typdef body shape *)
  | Notation_type_expected
  | Struct_no_fields
  | Variant_no_cases
  | Syntax_empty_body
  | Syntax_no_ids
  (* parser: hints on plain typdef *)
  | Hint_on_plain_bar_single
  | Hint_on_plain_bar_multi
  | Hint_on_plain_no_bar_single
  | Hint_on_plain_no_bar_multi
  (* parser: menhir fallback *)
  | Unexpected_token

let string_of_code = function
  | Stray_printable -> "stray-printable"
  | Stray_control_char -> "stray-control-char"
  | Stray_non_ascii_char -> "stray-non-ascii-char"
  | Invalid_utf8 -> "invalid-utf8"
  | Hole_index_overflow -> "hole-index-overflow"
  | Unclosed_text_literal -> "unclosed-text-literal"
  | Illegal_control_in_text_literal -> "illegal-control-in-text-literal"
  | Illegal_escape -> "illegal-escape"
  | Unclosed_block_comment -> "unclosed-block-comment"
  | Invalid_utf8_in_comment -> "invalid-utf8-in-comment"
  | Notation_type_expected -> "notation-type-expected"
  | Struct_no_fields -> "struct-no-fields"
  | Variant_no_cases -> "variant-no-cases"
  | Syntax_empty_body -> "syntax-empty-body"
  | Syntax_no_ids -> "syntax-no-ids"
  | Hint_on_plain_bar_single -> "hint-on-plain-bar-single"
  | Hint_on_plain_bar_multi -> "hint-on-plain-bar-multi"
  | Hint_on_plain_no_bar_single -> "hint-on-plain-no-bar-single"
  | Hint_on_plain_no_bar_multi -> "hint-on-plain-no-bar-multi"
  | Unexpected_token -> "unexpected-token"

let render_code (c : code) : string = "parse/" ^ string_of_code c

let related_of_pairs (pairs : (region * string) list) : Diag.related list =
  List.map (fun (region, message) -> { Diag.region; message }) pairs

let error ?code ?detail ?(related = []) (at : region) (msg : string) =
  let d =
    Diag.error
      ?code:(Option.map render_code code)
      ?detail ~related:(related_of_pairs related) ~source:"parse" at msg
  in
  raise (ParseError d)

let to_diagnostic (d : Diag.t) : Diag.t = d
