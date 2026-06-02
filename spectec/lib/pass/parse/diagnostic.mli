(** Parser diagnostics.

    All call sites go through [error]. Optional [?code], [?detail], and
    [?related] arguments attach extra fields; sites that do not need them omit
    the labels. The payload is the lib value type {!Diag.t} directly. *)

open Common.Source

(** {1 Diagnostic payload} *)

type error = Diag.t
type 'a result = ('a, error) Stdlib.result

exception ParseError of Diag.t

(** {1 Stable per-site identifier}

    One constructor per live parser diagnostic call site. Rendered as
    ["parse/<dashed-name>"] inside the diagnostic's [code] field. *)

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

(** {1 Raising}

    [?code] tags the diagnostic with a stable site identifier; [?detail]
    attaches longer prose; [?related] attaches secondary source spans. Sites
    that need none of these omit the labels. *)

val error :
  ?code:code ->
  ?detail:string ->
  ?related:(region * string) list ->
  region ->
  string ->
  'a

(** {1 Boundary - payload to {!Diag.t}} *)

val to_diagnostic : error -> Diag.t
