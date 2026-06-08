type t =
  | Keyword of string
  | Tag of string
  | Operator of string
  | Sub
  | Sup
  | Turnstile
  | Tilesturn
  | Arrow
  | ArrowSub
  | DoubleArrowSub
  | DoubleArrowLong
  | SqArrow
  | SqArrowStar
  | Dot
  | Dot2
  | Dot3
  | Semicolon
  | Colon
  | ColonEq
  | Tilde2
  | Backslash
  | LAngle
  | RAngle
  | LParen
  | RParen
  | LBrack
  | RBrack
  | LBrace
  | RBrace

let compare (atom_a : t) (atom_b : t) = Stdlib.compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

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

let to_string : t -> string = function
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

let unparse : t -> string = function
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

let keyword (s : string) : t =
  if is_upid s then Keyword s
  else invalid_arg ("Atom.keyword: expected upid: " ^ s)

let tag (s : string) : t =
  if is_upid s then Tag s else invalid_arg ("Atom.tag: expected upid: " ^ s)

let operator (s : string) : t =
  if String.contains s '\'' || String.contains s '\n' then
    invalid_arg ("Atom.operator: unquotable operator: " ^ s)
  else Operator s
