[@@@ocamlformat "disable"]

type t =
  | Atom of string       (* atomid *)
  | SilentAtom of string (* `atomid *)
  | Sub                  (* `<:` *)
  | Sup                  (* `:>` *)
  | Turnstile            (* `|-` *)
  | Tilesturn            (* `-|` *)
  | DoubleQuote          (* ``''` *)
  | Underscore           (* ``_` *)
  | Arrow                (* `->` *)
  | ArrowSub             (* `->_` *)
  | DoubleArrow          (* ``=>` *)
  | DoubleArrowSub       (* ``=>_` *)
  | SqArrow              (* `~>` *)
  | SqArrowStar          (* `~>*` *)
  | Dot                  (* ``.` *)
  | Dot2                 (* ``..` *)
  | Dot3                 (* ``...` *)
  | Comma                (* ``,` *)
  | Semicolon            (* ``;` *)
  | Colon                (* `:` *)
  | Hash                 (* ``#` *)
  | Dollar               (* ``$` *)
  | At                   (* ``@` *)
  | Quest                (* ``?` *)
  | Bang                 (* ``!` *)
  | BangEq               (* ``!=` *)
  | Tilde                (* ``~` *)
  | Tilde2               (* `~~` *)
  | LAngle               (* ``<` *)
  | LAngle2              (* `<<` *)
  | LAngleEq             (* ``<=` *)
  | LAngle2Eq            (* `<<=` *)
  | RAngle               (* ``>` *)
  | RAngle2              (* `>>` *)
  | RAngleEq             (* ``>=` *)
  | RAngle2Eq            (* `>>=` *)
  | LParen               (* ``(` *)
  | RParen               (* ``)` *)
  | LBrack               (* ``[` *)
  | RBrack               (* ``]` *)
  | LBrace               (* ``{` *)
  | LBraceHashRBrace     (* `{#}` *)
  | RBrace               (* ``}` *)
  | Plus                 (* ``+` *)
  | Plus2                (* ``++` *)
  | PlusEq               (* ``+=` *)
  | Minus                (* ``-` *)
  | MinusEq              (* ``-=` *)
  | Star                 (* ``*` *)
  | StarEq               (* ``*=` *)
  | Slash                (* ``/` *)
  | SlashEq              (* ``/=` *)
  | Backslash            (* ``\` *)
  | Percent              (* ``%` *)
  | PercentEq            (* ``%=` *)
  | Eq                   (* ``=` *)
  | Eq2                  (* `==` *)
  | Amp                  (* ``&` *)
  | Amp2                 (* ``&&` *)
  | Amp3                 (* ``&&&` *)
  | AmpEq                (* ``&=` *)
  | Up                   (* ``^` *)
  | UpEq                 (* ``^=` *)
  | Bar                  (* ``|` *)
  | Bar2                 (* ``||` *)
  | BarEq                (* ``|=` *)
  | SPlus                (* ``|+|` *)
  | SPlusEq              (* ``|+|=` *)
  | SMinus               (* ``|-|` *)
  | SMinusEq             (* ``|-|=` *)
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

let string_of_atom = function
  | Atom id -> id
  | SilentAtom id -> id
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | DoubleQuote -> "\""
  | Underscore -> "_"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "=>"
  | DoubleArrowSub -> "=>_"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Comma -> ","
  | Semicolon -> ";"
  | Colon -> ":"
  | Hash -> "#"
  | Dollar -> "$"
  | At -> "@"
  | Quest -> "?"
  | Bang -> "!"
  | BangEq -> "!="
  | Tilde -> "~"
  | Tilde2 -> "~~"
  | LAngle -> "<"
  | LAngle2 -> "<<"
  | LAngleEq -> "<="
  | LAngle2Eq -> "<<="
  | RAngle -> ">"
  | RAngle2 -> ">>"
  | RAngleEq -> ">="
  | RAngle2Eq -> ">>="
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | LBraceHashRBrace -> "{#}"
  | RBrace -> "}"
  | Plus -> "+"
  | Plus2 -> "++"
  | PlusEq -> "+="
  | Minus -> "-"
  | MinusEq -> "-="
  | Star -> "*"
  | StarEq -> "*="
  | Slash -> "/"
  | SlashEq -> "/="
  | Backslash -> "\\"
  | Percent -> "%"
  | PercentEq -> "%="
  | Eq -> "="
  | Eq2 -> "=="
  | Amp -> "&"
  | Amp2 -> "&&"
  | Amp3 -> "&&&"
  | AmpEq -> "&="
  | Up -> "^"
  | UpEq -> "^="
  | Bar -> "|"
  | Bar2 -> "||"
  | BarEq -> "|="
  | SPlus -> "|+|"
  | SPlusEq -> "|+|="
  | SMinus -> "|-|"
  | SMinusEq -> "|-|="
