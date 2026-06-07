[@@@ocamlformat "disable"]

(* Concrete operators are written in single quotes ('+', '->', ';') and carried
   by the generic [Operator] atom. The curated notation operators (Arrow, Dot,
   ...) are abstract mixfix atoms written bare, and carry precedence in [kind].

   The matched grouping brackets keep dedicated constructors so [kind] can give
   them bracket precedence, and spelled with a leading backtick on the opener
   (`` `( ``, `` `< ``). *)

type t =
  | Atom of string                  (* concrete upper-case word: INT *)
  | SilentAtom of string            (* abstract upper-case word: _NUM *)
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
   SilentAtom is suppressed by the IL printer. *)
let string_of_atom = function
  | Atom id -> id
  | SilentAtom id -> "_" ^ id
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
  | Atom id -> id
  | SilentAtom id -> "_" ^ id
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

(* Internal atom builder, used by target frontends. *)
let of_string : string -> t = function
  | "<" -> LAngle
  | ">" -> RAngle
  | "(" -> LParen
  | ")" -> RParen
  | "[" -> LBrack
  | "]" -> RBrack
  | "{" -> LBrace
  | "}" -> RBrace
  | "``<" -> Operator "<"
  | "``>" -> Operator ">"
  | "``[" -> Operator "["
  | "``]" -> Operator "]"
  | "``{" -> Operator "{"
  | "``}" -> Operator "}"
  (* `_` + upper-case word is a silent atom; a lone `_` is the concrete
     underscore terminal, so it must fall through to Operator. *)
  | s when String.length s > 1 && s.[0] = '_' && s.[1] >= 'A' && s.[1] <= 'Z' ->
      SilentAtom (String.sub s 1 (String.length s - 1))
  | s when String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z' -> Atom s
  | s -> Operator s
