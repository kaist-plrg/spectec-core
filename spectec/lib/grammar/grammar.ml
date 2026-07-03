open Common.Source

type primitive = Num of Xl.Num.typ | Bool | Text
type arg = Nonterminal of Il.id | Primitive of primitive
type recursion = Neither | Left | Right | Both
type precedence = Tighter of Xl.Atom.t
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

let arg_of_typ (typ : Il.typ) : arg =
  match typ.it with
  | Il.VarT { synid; _ } -> Nonterminal synid
  | Il.NumT numtyp -> Primitive (Num numtyp)
  | Il.BoolT -> Primitive Bool
  | Il.TextT -> Primitive Text
  | _ ->
      failwith
        ("grammar: unsupported object-syntax type " ^ Il.Typ.to_string typ)

let is_self_arg (self : Il.id) : arg Il.Mixfix.mixeme -> bool = function
  | Il.Mixfix.Arg (Nonterminal id) -> id.it = self.it
  | _ -> false

let recursion_of (self : Il.id) (notation : arg Il.Mixfix.t) : recursion =
  let head_is_self = function [] -> false | e :: _ -> is_self_arg self e in
  let left = head_is_self notation in
  let right = head_is_self (List.rev notation) in
  match (left, right) with
  | true, false -> Left
  | false, true -> Right
  | true, true -> Both
  | false, false -> Neither

let is_plain_inclusion (notation : arg Il.Mixfix.t) : bool =
  Il.Mixfix.atoms notation = [] && Il.Mixfix.arity notation = 1

let production_of_notation (self : Il.id) ~(origin : Il.id)
    ~(precedence : precedence option) (notation : arg Il.Mixfix.t) : production
    =
  {
    notation;
    recursion = recursion_of self notation;
    precedence;
    construction = (if is_plain_inclusion notation then Alias else Case);
    origin;
  }

let precedence_of_typcase (typcase : Il.typcase) : precedence option =
  Option.map (fun atom -> Tighter atom) (Hints.Precedence.find typcase.hints)

let production_of_typcase (self : Il.id) (typcase : Il.typcase) : production =
  production_of_notation self ~origin:typcase.origin.it.synid
    ~precedence:(precedence_of_typcase typcase)
    (Il.Mixfix.map arg_of_typ typcase.notation.it)

let syntax_of_def (def : Il.def) : syntax option =
  match def.it with
  | Il.TypD { synid; deftyp = { it = Il.VariantT typcases; _ }; _ } ->
      Some
        {
          name = synid;
          productions = List.map (production_of_typcase synid) typcases;
        }
  | Il.TypD { synid; deftyp = { it = Il.PlainT typ; _ }; _ } ->
      Some
        {
          name = synid;
          productions =
            [
              production_of_notation synid ~origin:synid ~precedence:None
                [ Il.Mixfix.Arg (arg_of_typ typ) ];
            ];
        }
  | _ -> None

type syntax_usage = { name : string; uses : string list }
type usage_graph = syntax_usage list

let usage_graph_of (spec : Il.spec) : usage_graph =
  let uses_of_typ (typ : Il.typ) =
    match typ.it with Il.VarT { synid; _ } -> [ synid.it ] | _ -> []
  in
  let usage_of_def (def : Il.def) : syntax_usage option =
    match def.it with
    | Il.TypD { synid; deftyp = { it = Il.VariantT typcases; _ }; _ } ->
        Some
          {
            name = synid.it;
            uses =
              List.concat_map
                (fun (typcase : Il.typcase) ->
                  List.concat_map uses_of_typ
                    (Il.Mixfix.args typcase.notation.it))
                typcases;
          }
    | Il.TypD { synid; deftyp = { it = Il.PlainT typ; _ }; _ } ->
        Some { name = synid.it; uses = uses_of_typ typ }
    | _ -> None
  in
  List.filter_map usage_of_def spec

let reachable (graph : usage_graph) (start : string) : string list =
  let uses_of name =
    match List.find_opt (fun u -> u.name = name) graph with
    | Some u -> u.uses
    | None -> []
  in
  let rec visit seen name =
    if List.mem name seen then seen
    else List.fold_left visit (name :: seen) (uses_of name)
  in
  visit [] start

let extract ~(start : string) (spec : Il.spec) : t =
  let keep = reachable (usage_graph_of spec) start in
  let is_reachable (def : Il.def) =
    match def.it with
    | Il.TypD { synid; _ } -> List.mem synid.it keep
    | _ -> false
  in
  List.filter_map
    (fun def -> if is_reachable def then syntax_of_def def else None)
    spec

let string_of_primitive = function
  | Num numtyp -> Xl.Num.string_of_typ numtyp
  | Bool -> "bool"
  | Text -> "text"

let string_of_arg = function
  | Nonterminal id -> "<" ^ id.it ^ ">"
  | Primitive prim -> "<" ^ string_of_primitive prim ^ ">"

let string_of_mixeme : arg Il.Mixfix.mixeme -> string = function
  | Il.Mixfix.Atom atom -> Xl.Atom.unparse atom.it
  | Il.Mixfix.Arg arg -> string_of_arg arg

let string_of_recursion = function
  | Neither -> "neither"
  | Left -> "left"
  | Right -> "right"
  | Both -> "both"

let string_of_precedence = function
  | None -> ""
  | Some (Tighter atom) -> "tighter than " ^ Xl.Atom.unparse atom

let string_of_production (self : Il.id) (production : production) : string =
  let notation =
    String.concat " " (List.map string_of_mixeme production.notation)
  in
  let from =
    if production.origin.it = self.it then ""
    else Printf.sprintf "(from %s)" production.origin.it
  in
  let suffix =
    [ string_of_precedence production.precedence; from ]
    |> List.filter (fun s -> s <> "")
  in
  let suffix =
    match suffix with [] -> "" | parts -> "  " ^ String.concat "  " parts
  in
  Printf.sprintf "  | %-44s %-7s%s" notation
    (string_of_recursion production.recursion)
    suffix

let string_of_syntax (syntax : syntax) : string =
  Printf.sprintf "syntax %s:\n%s" syntax.name.it
    (String.concat "\n"
       (List.map (string_of_production syntax.name) syntax.productions))

let string_of_t (grammar : t) : string =
  String.concat "\n\n" (List.map string_of_syntax grammar)
