(** The grammar of the defined target. *)

type primitive = Num of Xl.Num.typ | Bool | Text

(** What fills an [Il.Mixfix.Arg] position: a reference to another syntax
    ([Nonterminal]) or a primitive terminal. *)
type arg = Nonterminal of Il.id | Primitive of primitive

(** Whether the production is left- and/or right-recursive in its own syntax. *)
type recursion = Neither | Left | Right | Both

(** [Tighter a]: binds more tightly than the sibling production whose operator
    is [a]. *)
type precedence = Tighter of Xl.Atom.t

(** [Case] builds a node tagged with the syntax; [Alias] (a plain inclusion like
    [prog = command]) passes its single argument through unchanged. *)
type construction = Case | Alias

type production = {
  notation : arg Il.Mixfix.t;
  recursion : recursion;
  precedence : precedence option;
  construction : construction;
  origin : Il.id;
}

type syntax = { name : Il.id; productions : production list }
type t = syntax list

(** [extract ~start spec] is the grammar of the syntaxes reachable from [start],
    read from [spec]'s variant and plain-alias [syntax] declarations. Raises
    [Failure] if a reachable production uses a type the grammar cannot render as
    surface syntax. *)
val extract : start:string -> Il.spec -> t

val string_of_t : t -> string
