module Mixfix = Il.Mixfix
module Terminal = Grammar.Terminal
module Env = Map.Make (String)

type 'tok token_classifier = 'tok -> Terminal.t option

exception Error of string

(* A production falls into one parsing role, fixed by whether its notation
   recurses on its own syntax and at which edge. The role name and the structural
   [Grammar.recursion] name are opposite, because a prefix operator recurses to
   its right and a postfix to its left (the operand sits opposite the operator):

     Grammar.recursion   role      shape     parsed as        (by)
     -----------------   -------   -------   --------------   ----------------
     Neither             primary   n, (e)    a primary        parse_primary
     Right               prefix    - e       a prefix operand parse_operand
     Both                infix     e + e     an infix tail    extend_infix
     Left                postfix   e !       unsupported      check_no_postfix *)
let is_primary (prod : Grammar.production) = prod.recursion = Grammar.Neither
let is_prefix (prod : Grammar.production) = prod.recursion = Grammar.Right
let is_infix (prod : Grammar.production) = prod.recursion = Grammar.Both
let is_postfix (prod : Grammar.production) = prod.recursion = Grammar.Left

(* Operator atom and binding power resolved at compile time, so the parse loop
   never re-walks the tighter_than chain. *)
type operator = { prod : Grammar.production; atom : Xl.Atom.t; rank : int }

(* A syntax's productions bucketed by parsing role (see the table above). *)
type compiled_syntax = {
  name : string;
  primaries : Grammar.production list;
  prefixes : operator list;
  infixes : operator list;
}

type table = compiled_syntax Env.t

type 'tok state = {
  mutable toks : 'tok list;
  classify : 'tok token_classifier;
  table : table;
}

let value_of_production (prod : Grammar.production) (args : Il.value list) :
    Il.value =
  match (prod.construction, args) with
  (* A plain inclusion is transparent: its value is the argument, not a wrapper. *)
  | Grammar.Alias, [ arg ] -> arg
  | Grammar.Alias, _ ->
      failwith "Parse: alias production must have exactly one argument"
  | Grammar.Case, _ ->
      (* Typed at the production's origin, not the enclosing syntax, so a case
         inlined from another syntax keeps its own type. *)
      let _, cases =
        List.fold_left_map
          (fun remaining -> function
            | Mixfix.Atom atom -> (remaining, Il.Case.atom atom.it)
            | Mixfix.Arg _ -> (
                match remaining with
                | arg :: rest -> (rest, Il.Case.arg arg)
                | [] -> failwith "Parse: production argument count mismatch"))
          args prod.notation
      in
      Il.Case.case_v ~var:prod.origin.it cases

let check_no_postfix (grammar : Grammar.t) =
  List.iter
    (fun (syntax : Grammar.syntax) ->
      if List.exists is_postfix syntax.productions then
        failwith
          (Printf.sprintf "Parse: postfix productions in %s are not supported"
             syntax.name.it))
    grammar

let compile (grammar : Grammar.t) : table =
  let operator_of (syntax : Grammar.syntax) prod =
    Grammar.operator_atom prod
    |> Option.map (fun atom -> { prod; atom; rank = Grammar.rank syntax atom })
  in
  let compile_syntax (syntax : Grammar.syntax) : compiled_syntax =
    let operators fixity =
      List.filter fixity syntax.productions
      |> List.filter_map (operator_of syntax)
    in
    {
      name = syntax.name.it;
      primaries = List.filter is_primary syntax.productions;
      prefixes = operators is_prefix;
      infixes = operators is_infix;
    }
  in
  List.fold_left
    (fun tbl (syntax : Grammar.syntax) ->
      Env.add syntax.name.it (compile_syntax syntax) tbl)
    Env.empty grammar

(* Token stream *)

let advance state =
  match state.toks with [] -> () | _ :: rest -> state.toks <- rest

(* The one place [classify] runs: every query below inspects the terminal it
   returns, so no caller re-classifies a raw token. *)
let peek_terminal state =
  match state.toks with [] -> None | tok :: _ -> state.classify tok

let terminal_is_atom (atom : Xl.Atom.t) = function
  | Terminal.Atom a -> Xl.Atom.eq a atom
  | _ -> false

let terminal_is_primitive prim = function
  | Terminal.Primitive (p, _) -> p = prim
  | _ -> false

let expect state (atom : Xl.Atom.t) =
  match peek_terminal state with
  | Some t when terminal_is_atom atom t -> advance state
  | _ -> raise (Error ("expected " ^ Xl.Atom.to_string atom))

let parse_primitive state prim =
  match peek_terminal state with
  | Some (Terminal.Primitive (p, v)) when p = prim ->
      advance state;
      v
  | _ -> raise (Error "primitive token mismatch")

(* Grammar queries *)

let find_syntax state name : compiled_syntax =
  match Env.find_opt name state.table with
  | Some compiled -> compiled
  | None -> failwith ("Parse: grammar references unknown syntax " ^ name)

let find_operator state min_rank (operators : operator list) =
  match peek_terminal state with
  | Some (Terminal.Atom a) ->
      List.find_opt
        (fun op -> Xl.Atom.eq op.atom a && op.rank >= min_rank)
        operators
  | _ -> None

(* A production that leads with a syntax reference can be started by
   whatever can start that syntax. [seen] guards the mutual recursion
   from looping on a cyclic reference. *)
let rec starts_with state seen (terminal : Terminal.t)
    (prod : Grammar.production) : bool =
  match
    List.find_opt
      (function Mixfix.Atom { it = Xl.Atom.Tag _; _ } -> false | _ -> true)
      prod.notation
  with
  | Some (Mixfix.Atom atom) -> terminal_is_atom atom.it terminal
  | Some (Mixfix.Arg (Grammar.Primitive prim)) ->
      terminal_is_primitive prim terminal
  | Some (Mixfix.Arg (Grammar.Nonterminal sub)) ->
      syntax_starts_with state seen sub.it terminal
  | None -> false

(* What can start a syntax: one of its primaries or a prefix operator. Infixes
   lead with the syntax itself, so they never start it. *)
and syntax_starts_with state seen name (terminal : Terminal.t) : bool =
  (not (List.mem name seen))
  &&
  let compiled = find_syntax state name in
  let starters =
    compiled.primaries
    @ List.map (fun (op : operator) -> op.prod) compiled.prefixes
  in
  List.exists (starts_with state (name :: seen) terminal) starters

(* Parse algorithm: one function per accepted role (table above), where a
   phrase is one complete instance of a syntax. Each level parses its own role
   and calls the level below for the rest.

     parse_phrase   a phrase   = an operand, then infix operators (extend_infix)
     parse_operand  an operand = a prefix operator, else a primary
     parse_primary  a primary  = a parenthesized phrase, else the production
                                 whose leading token matches the input

   An infix production like [e + e] starts with [e]. Parsing it directly would
   call parse_phrase again without consuming a token, leading to an infinite
   loop. Instead, parse_phrase reads one operand and then any trailing infix
   operators. That operand is a primary or a prefix operator (parse_operand),
   never a whole [e + e], so the leading [e] is read without a recursive call
   to parse_phrase; the [+ e] is handled afterward.

   Each operand is parsed with a [min_rank] and consumes only operators of that
   rank or higher. A right operand uses [op.rank + 1], so that it consumes
   operators tighter than [op] but not [op] itself or anything looser. This
   gives precedence and left-association: [a + b * c] parses as [a + (b * c)],
   and [a + b + c] as [(a + b) + c]. *)

let rec parse_phrase state compiled min_rank =
  let operand = parse_operand state compiled min_rank in
  extend_infix state compiled min_rank operand

and parse_operand state compiled min_rank =
  match find_operator state min_rank compiled.prefixes with
  | Some op -> parse_production state compiled op.rank op.prod
  | None -> parse_primary state compiled

and parse_primary state compiled =
  match peek_terminal state with
  | Some (Terminal.Atom a) when Xl.Atom.eq a Xl.Atom.LParen ->
      parse_grouped state compiled
  | Some terminal -> (
      match
        List.find_opt
          (starts_with state [ compiled.name ] terminal)
          compiled.primaries
      with
      | Some prod -> parse_production state compiled 0 prod
      | None -> raise (Error "no production matches the current token"))
  | None -> raise (Error "unexpected end of input")

(* parse_primary only peeked the [(] and did not consume it. The inner phrase
   parses at [min_rank] 0, since parentheses start a fresh phrase. *)
and parse_grouped state compiled =
  advance state;
  let inner = parse_phrase state compiled 0 in
  expect state Xl.Atom.RParen;
  inner

and parse_production state compiled operand_rank (prod : Grammar.production) =
  let parse_mixeme = function
    (* A tag names the case; it has no surface token to consume. *)
    | Mixfix.Atom { it = Xl.Atom.Tag _; _ } -> None
    | Mixfix.Atom atom ->
        expect state atom.it;
        None
    | Mixfix.Arg (Grammar.Primitive prim) -> Some (parse_primitive state prim)
    | Mixfix.Arg (Grammar.Nonterminal sub) ->
        (* A self-recursive argument stays in the current syntax at
           [operand_rank]; any other looks up its own syntax and restarts at
           rank 0. *)
        Some
          (if sub.it = compiled.name then
             parse_phrase state compiled operand_rank
           else parse_phrase state (find_syntax state sub.it) 0)
  in
  List.filter_map parse_mixeme prod.notation |> value_of_production prod

(* Left-associative: the right operand binds one rank tighter ([op.rank + 1]), so
   equal precedence groups left. *)
and extend_infix state compiled min_rank left =
  match find_operator state min_rank compiled.infixes with
  | None -> left
  | Some op ->
      advance state;
      let right = parse_phrase state compiled (op.rank + 1) in
      extend_infix state compiled min_rank
        (value_of_production op.prod [ left; right ])

let run (classify : 'tok token_classifier) (grammar : Grammar.t)
    (start : string) (tokens : 'tok list) : Il.value =
  check_no_postfix grammar;
  let state = { toks = tokens; classify; table = compile grammar } in
  let value = parse_phrase state (find_syntax state start) 0 in
  match state.toks with
  | [] -> value
  | _ -> raise (Error "leftover tokens after parse")
