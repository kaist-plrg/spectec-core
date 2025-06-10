[@@@ocamlformat "disable"]

type t =
  | Atom of string  (* atomid *)
  | Infinity        (* infinity *)
  | Dot             (* `.` , ``.` *)
  | Dot2            (* `..`, ``..` *)
  | Dot3            (* `...` *)
  | Underscore      (* ``_` *)
  | Semicolon       (* `;` *)
  | Backslash       (* `\` *)
  | Mem             (* `<-` *)
  | Arrow           (* `->` *)
  | Arrow2          (* ``=>` *)
  | ArrowSub        (* `->_` *)
  | Arrow2Sub       (* ``=>_` *)
  | Colon           (* `:` *)
  | Sub             (* `<:` *)
  | Sup             (* `:>` *)
  | Assign          (* `:=` *)
  | Equal           (* ``=` *)
  | LessEqual       (* ``<=` *)
  | GreaterEqual    (* ``>=` *)
  | Equiv           (* `==` *)
  | NotEquiv        (* ``!=` *)
  | Approx          (* `~~` *)
  | SqArrow         (* `~>` *)
  | SqArrowStar     (* `~>*` *)
  | Prec            (* `<<` *)
  | PrecEq          (* `<<=` *)
  | Succ            (* `>>` *)
  | SuccEq          (* `>>=` *)
  | Turnstile       (* `|-` *)
  | Tilesturn       (* `-|` *)
  | Not             (* ``~` *)
  | Quest           (* ``?` *)
  | Bang            (* ``!` *)
  | Plus            (* ``+` *)
  | PlusEq          (* ``+=` *)
  | SPlus           (* ``|+|` *)
  | SPlusEq         (* ``|+|=` *)
  | Minus           (* ``-` *)
  | MinusEq         (* ``-=` *)
  | SMinus          (* ``|-|` *)
  | SMinusEq        (* ``|-|=` *)
  | Star            (* ``*` *)
  | StarEq          (* ``*=` *)
  | Slash           (* ``/` *)
  | SlashEq         (* ``/=` *)
  | Mod             (* ``%` *)
  | ModEq           (* ``%=` *)
  | Hash            (* ``#` *)
  | Up              (* ``^` *)
  | UpEq            (* ``^=` *)
  | Amp             (* ``&` *)
  | AmpEq           (* ``&=` *)
  | Amp2            (* ``&&` *)
  | Amp3            (* ``&&&` *)
  | Bar             (* ``|` *)
  | BarEq           (* ``|=` *)
  | Bar2            (* ``||` *)
  | At              (* ``@` *)
  | Dollar          (* ``$` *)
  | Comma           (* ``,` *)
  | Cat             (* ``++` *)
  | BigAnd          (* `(/\)` *)
  | BigOr           (* `(\/)` *)
  | BigAdd          (* `(+)` *)
  | BigMul          (* `( * )` *)
  | BigCat          (* `(++)` *)
  | Invalid         (* `{#}` *)
  | LAngle
  | RAngle          (* ``<` `>` *)
  | LParen
  | RParen          (* ``(` `)` *)
  | LBrack
  | RBrack          (* ``[` `]` *)
  | LBrace
  | RBrace          (* ``{` `}` *)
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

let string_of_atom = function
  | Atom "_" -> "_"
  | Atom id -> id
  | Infinity -> "infinity"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Underscore -> "_"
  | Semicolon -> ";"
  | Backslash -> "\\"
  | Mem -> "<-"
  | Arrow -> "->"
  | Arrow2 -> "=>"
  | ArrowSub -> "->_"
  | Arrow2Sub -> "=>_"
  | Colon -> ":"
  | Sub -> "<:"
  | Sup -> ":>"
  | Assign -> ":="
  | Equal -> "="
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | Equiv -> "=="
  | NotEquiv -> "!="
  | Approx -> "~~"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Prec -> "<<"
  | PrecEq -> "<<="
  | Succ -> ">>"
  | SuccEq -> ">>="
  | Tilesturn -> "-|"
  | Turnstile -> "|-"
  | Not -> "~"
  | Quest -> "?"
  | Bang -> "!"
  | Plus -> "+"
  | PlusEq -> "+="
  | SPlus -> "|+|"
  | SPlusEq -> "|+|="
  | Minus -> "-"
  | MinusEq -> "-="
  | SMinus -> "|-|"
  | SMinusEq -> "|-|="
  | Star -> "*"
  | StarEq -> "*="
  | Slash -> "/"
  | SlashEq -> "/="
  | Mod -> "%"
  | ModEq -> "%="
  | Hash -> "#"
  | Up -> "^"
  | UpEq -> "^="
  | Amp -> "&"
  | AmpEq -> "&="
  | Amp2 -> "&&"
  | Amp3 -> "&&&"
  | Bar -> "|"
  | BarEq -> "|="
  | Bar2 -> "||"
  | At -> "@"
  | Dollar -> "$"
  | Comma -> ","
  | Cat -> "++"
  | BigAnd -> "(/\\)"
  | BigOr -> "(\\/)"
  | BigAdd -> "(+)"
  | BigMul -> "(*)"
  | BigCat -> "(++)"
  | Invalid -> "{#}"
  | LAngle -> "<"
  | RAngle -> ">"
  | LParen -> "("
  | LBrack -> "["
  | LBrace -> "{"
  | RParen -> ")"
  | RBrack -> "]"
  | RBrace -> "}"
