(** A parser for object-language concrete syntax, driven by the extracted
    {!Grammar.t}: [run] turns a token stream into the same IL value a
    hand-written parser would build, resolving operator precedence from the
    [tighter_than] hints on the grammar. *)

(** Classify one lexer token into the grammar terminal it stands for. [None] for
    tokens that are not grammar terminals, such as end-of-input. *)
type 'tok token_classifier = 'tok -> Grammar.Terminal.t option

exception Error of string

(** [run classify grammar start tokens] parses [tokens] as an instance of syntax
    [start]. Raises {!Error} when the tokens do not match the grammar
    (unexpected token, end of input, or leftover tokens), and [Failure] when the
    grammar itself is unsupported or malformed (a postfix operator, an unknown
    syntax reference). *)
val run : 'tok token_classifier -> Grammar.t -> string -> 'tok list -> Il.value
