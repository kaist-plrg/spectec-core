[@@@ocamlformat "disable"]

(* Concrete operators are written in single quotes ('+', '->', ';') and carried
   by the generic [Operator] atom. The curated notation operators (Arrow, Dot,
   ...) are abstract mixfix atoms written bare, and carry precedence in [kind].

   The matched grouping brackets keep dedicated constructors so [kind] can give
   them bracket precedence, and spelled with a leading backtick on the opener
   (`` `( ``, `` `< ``). *)

type t =
  | Keyword of string               (* concrete upper-case word: INT *)
  | Tag of string                   (* abstract upper-case word: _NUM *)
  | Operator of string              (* concrete operator: '+', '->', ';' *)
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
  | RAngle                          (* > *)
  | LParen                          (* `( *)
  | RParen                          (* ) *)
  | LBrack                          (* `[ *)
  | RBrack                          (* ] *)
  | LBrace                          (* `{ *)
  | RBrace                          (* } *)
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

(* Precedence mirrors parser.mly: relop atoms (levels 1-4) are looser than
   infixop atoms (levels 5-9). Higher level = tighter. An [Operator] atom is a
   literal terminal with no precedence, so it is Plain. *)

type assoc = Left | Right | Non

type kind =
  | Plain
  | Infix of { assoc : assoc; level : int }
  | BracketL
  | BracketR

let kind : t -> kind = function
  | LAngle | LParen | LBrack | LBrace -> BracketL
  | RAngle | RParen | RBrack | RBrace -> BracketR
  | Turnstile -> Infix { assoc = Non; level = 1 }
  | Tilesturn -> Infix { assoc = Non; level = 2 }
  | SqArrow | SqArrowStar -> Infix { assoc = Right; level = 3 }
  | Colon | Tilde2 -> Infix { assoc = Left; level = 4 }
  | ColonEq | DoubleArrowSub | DoubleArrowLong ->
      Infix { assoc = Right; level = 5 }
  | Arrow | ArrowSub -> Infix { assoc = Right; level = 6 }
  | Semicolon -> Infix { assoc = Left; level = 7 }
  | Dot | Dot2 | Dot3 -> Infix { assoc = Left; level = 8 }
  | Backslash -> Infix { assoc = Left; level = 9 }
  | _ -> Plain

let closer_of : t -> t option = function
  | LAngle -> Some RAngle
  | LParen -> Some RParen
  | LBrack -> Some RBrack
  | LBrace -> Some RBrace
  | _ -> None

(* Lossy pretty-printing, canonical glyph. Drives the IL/elab/struct output;
   Tag is suppressed by the IL printer. *)
let string_of_atom = function
  | Keyword id -> id
  | Tag id -> "_" ^ id
  | Operator s -> s
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonEq -> ":="
  | Tilde2 -> "~~"
  | Backslash -> "\\"
  | LAngle -> "<"
  | RAngle -> ">"
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"

(* Faithful source-form printer: emits the surface syntax the parser reads, so
   it round-trips through the lexer. Used by the EL unparser. *)
let string_of_atom_exact : t -> string = function
  | Keyword id -> id
  | Tag id -> "_" ^ id
  | Operator s -> "'" ^ s ^ "'"
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonEq -> ":="
  | Tilde2 -> "~~"
  | Backslash -> "\\"
  | LAngle -> "`<"
  | RAngle -> ">"
  | LParen -> "`("
  | RParen -> ")"
  | LBrack -> "`["
  | RBrack -> "]"
  | LBrace -> "`{"
  | RBrace -> "}"

let is_upid (s : string) : bool =
  String.length s > 0
  && (match s.[0] with 'A' .. 'Z' -> true | _ -> false)
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       s

(* Keyword payload is an upper identifier (upid in pass/parse/lexer.mll). *)
let keyword (s : string) : t =
  if is_upid s then Keyword s
  else invalid_arg ("Atom.keyword: expected upid: " ^ s)

(* Tag name is an upper identifier; the silencing underscore is not part of it. *)
let tag (s : string) : t =
  if is_upid s then Tag s else invalid_arg ("Atom.tag: expected upid: " ^ s)

(* Operator payload is any run with no single quote or newline. *)
let operator (s : string) : t =
  if String.contains s '\'' || String.contains s '\n' then
    invalid_arg ("Atom.operator: unquotable operator: " ^ s)
  else Operator s
