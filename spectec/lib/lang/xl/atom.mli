(** Atoms: the terminal tokens of notation, shared across EL, IL and SL. An atom
    is either an object-language terminal that appears in the described program,
    or a piece of the spec's own meta-notation.

    The three string-carrying cases ([Keyword], [Tag], [Operator]) carry private
    payloads, so they can only be built through {!keyword}, {!tag} and
    {!operator}, which validate against what the lexer can read; the nullary
    constructors are built directly. *)

(** A validated upper identifier. *)
type upid = private string

(** Validated operator text. *)
type optext = private string

[@@@ocamlformat "disable"]

type t =
  | Keyword of upid                 (* concrete object word: INT *)
  | Tag of upid                     (* silent meta case label: _NUM *)
  | Operator of optext              (* concrete operator: '+', '->', ';' *)
  | Sub                             (* <: *)
  | Sup                             (* :> *)
  | Turnstile                       (* |- *)
  | Tilesturn                       (* -| *)
  | Arrow                           (* -> *)
  | ArrowSub                        (* ->_ *)
  | DoubleArrowSub                  (* =>_ *)
  | DoubleArrowLong                 (* ==> *)
  | SqArrow                         (* ~> *)
  | SqArrowStar                     (* ~>* *)
  | Dot                             (* . *)
  | Dot2                            (* .. *)
  | Dot3                            (* ... *)
  | Semicolon                       (* ; *)
  | Colon                           (* : *)
  | ColonEq                         (* := *)
  | Tilde2                          (* ~~ *)
  | Backslash                       (* \ *)
  | LAngle                          (* `< *)
  | RAngle                          (* >` *)
  | LParen                          (* `( *)
  | RParen                          (* )` *)
  | LBrack                          (* `[ *)
  | RBrack                          (* ]` *)
  | LBrace                          (* `{ *)
  | RBrace                          (* }` *)
[@@@ocamlformat "enable"]

val compare : t -> t -> int
val eq : t -> t -> bool

type assoc = Left | Right | Non

(** Precedence class of an atom used as notation. [Infix] levels mirror
    parser.mly: relop 1-4 are looser than infixop 5-9, higher binds tighter. *)
type kind =
  | Plain
  | Infix of { assoc : assoc; level : int }
  | BracketL
  | BracketR

val kind : t -> kind
val closer_of : t -> t option

(** Canonical, lossy glyph of an atom. See {!unparse} for the parse-faithful
    form. *)
val to_string : t -> string

(** Parse-faithful source form: round-trips through the lexer. *)
val unparse : t -> string

(** [keyword s] is [Keyword s]; raises [Invalid_argument] unless [s] is an upper
    identifier (upid, per pass/parse/lexer.mll). *)
val keyword : string -> t

(** [tag s] is [Tag s]; raises [Invalid_argument] unless [s] is an upper
    identifier. The silencing underscore is not part of [s]. *)
val tag : string -> t

(** [operator s] is [Operator s]; raises [Invalid_argument] if [s] contains a
    single quote or newline. *)
val operator : string -> t

(** [is_operator atom s] holds when [atom] is the operator spelled [s]. *)
val is_operator : t -> string -> bool
