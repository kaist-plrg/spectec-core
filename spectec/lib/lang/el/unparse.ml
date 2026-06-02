(* Canonical EL printer. Internally builds [PPrint.document] values;
   the public surface renders them to a [Format.formatter] or
   [string]. *)

open Xl
open Types
module P = PPrint

let ( ^^ ) = P.( ^^ )

(* Render geometry. [ribbon_fraction = 1.0] disables PPrint's separate
   ribbon constraint, so only [line_width] determines wrapping. *)
let line_width = 80
let ribbon_fraction = 1.0

(* Inline-or-wrap: print [head body] on one line if it fits; otherwise
   [head] on its own line and [body] on the next indented by 2. *)
let head_body ~head ~body = P.prefix 2 1 head body

(* Primitives *)

let to_doc_num num = P.string (Num.string_of_num num)
let to_doc_text text = P.string (Printf.sprintf "\"%s\"" (String.escaped text))
let to_doc_varid (id : id) = P.string id.it
let to_doc_typid (id : id) = P.string id.it
let to_doc_relid (id : id) = P.string id.it

let to_doc_ruleid (id : id) =
  if id.it = "" then P.empty else P.string "/" ^^ P.string id.it

let to_doc_defid (id : id) = P.string "$" ^^ P.string id.it
let to_doc_atom (atom : atom) = P.string (Atom.string_of_atom_exact atom.it)
let to_doc_iter = function Opt -> P.string "?" | List -> P.string "*"

let to_doc_unop = function
  | #Bool.unop as op -> P.string (Bool.string_of_unop op)
  | #Num.unop as op -> P.string (Num.string_of_unop op)

let to_doc_binop = function
  | #Bool.binop as op -> P.string (Bool.string_of_binop op)
  | #Num.binop as op -> P.string (Num.string_of_binop op)

let to_doc_cmpop = function
  | #Bool.cmpop as op -> P.string (Bool.string_of_cmpop op)
  | #Num.cmpop as op -> P.string (Num.string_of_cmpop op)

(* Three expression layers with disjoint acceptance rules. A child that
   would not parse at the current layer is lifted into [$(...)]. *)
type ctx = Arith | Notation | Atom

let is_arith_binop : binop -> bool = function
  | #Num.binop -> true
  | #Bool.binop -> false

let is_arith_cmpop : cmpop -> bool = function
  | #Num.cmpop -> true
  | #Bool.cmpop -> false

(* Relop and relational-style infixop atoms (`|-`, `:`, `~>`, `==>`,
   `->`, ...): preferred break points. *)
let is_breakable_atom (a : Atom.t) : bool =
  match a with
  | Turnstile | Tilesturn | SqArrow | SqArrowStar | Sub | Sup | Colon _
  | ColonEq _ | Tilde2 _ | Arrow _ | ArrowSub | DoubleArrow | DoubleArrowSub
  | DoubleArrowLong ->
      true
  | _ -> false

(* Flatten right-nested chains of breakable atoms (e.g. `Γ |- e : τ`)
   so every operator lands at the same column when the chain breaks. *)
let rec collect_infix_chain_exp (exp : exp) : exp * (atom * exp) list =
  match exp.it with
  | InfixE (l, atom, r) when is_breakable_atom atom.it ->
      let head, tail = collect_infix_chain_exp r in
      (l, (atom, head) :: tail)
  | _ -> (exp, [])

let rec collect_infix_chain_typ (typ : typ) : typ * (atom * typ) list =
  match typ with
  | NotationT { it = InfixT (l, atom, r); _ } when is_breakable_atom atom.it ->
      let head, tail = collect_infix_chain_typ r in
      (l, (atom, head) :: tail)
  | _ -> (typ, [])

let infix_chain (to_doc : 'a -> P.document)
    ((head, segs) : 'a * (atom * 'a) list) : P.document =
  let segment (a, e) = P.break 1 ^^ to_doc_atom a ^^ P.space ^^ to_doc e in
  P.group (to_doc head ^^ P.nest 2 (P.concat_map segment segs))

let break_at_op l op r = P.group (l ^^ P.break 1 ^^ op ^^ P.space ^^ r)

(* [group] makes the list all-or-nothing; [align] indents continuation
   lines under the first element. *)
let comma_list to_doc items =
  P.align (P.group (P.separate_map (P.comma ^^ P.break 1) to_doc items))

let space_list to_doc items =
  P.align (P.group (P.separate_map (P.break 1) to_doc items))

(* Types *)

let rec to_doc_typ = function
  | PlainT plaintyp -> to_doc_plaintyp plaintyp
  | NotationT nottyp -> to_doc_nottyp nottyp

and to_doc_plaintyp (plaintyp : plaintyp) =
  match plaintyp.it with
  | BoolT -> P.string "bool"
  | NumT numtyp -> P.string (Num.string_of_typ numtyp)
  | TextT -> P.string "text"
  | VarT (typid, []) -> to_doc_typid typid
  | VarT (typid, targs) ->
      to_doc_typid typid ^^ P.angles (comma_list to_doc_targ targs)
  | ParenT plaintyp -> P.parens (to_doc_plaintyp plaintyp)
  | TupleT plaintyps -> P.parens (comma_list to_doc_plaintyp plaintyps)
  | IterT (plaintyp, iter) -> to_doc_plaintyp plaintyp ^^ to_doc_iter iter

and to_doc_nottyp (nottyp : nottyp) =
  match nottyp.it with
  | AtomT atom -> to_doc_atom atom
  | SeqT typs -> space_list to_doc_typ typs
  | InfixT (NotationT { it = SeqT []; _ }, atom, typ_r) ->
      to_doc_atom atom ^^ P.space ^^ to_doc_typ typ_r
  | InfixT (_, atom, _) when is_breakable_atom atom.it ->
      infix_chain to_doc_typ (collect_infix_chain_typ (NotationT nottyp))
  | InfixT (typ_l, atom, typ_r) ->
      to_doc_typ typ_l ^^ P.space ^^ to_doc_atom atom ^^ P.space
      ^^ to_doc_typ typ_r
  | BrackT (atom_l, typ, atom_r) ->
      to_doc_atom atom_l ^^ to_doc_typ typ ^^ to_doc_atom atom_r

and to_doc_deftyp (deftyp : deftyp) =
  match deftyp.it with
  | PlainTD plaintyp -> to_doc_plaintyp plaintyp
  | StructTD typfields -> P.braces (comma_list to_doc_typfield typfields)
  | VariantTD typcases ->
      P.concat_map
        (fun c -> P.hardline ^^ P.string "  | " ^^ to_doc_typcase c)
        typcases

and to_doc_typfield (atom, plaintyp, hints) =
  to_doc_atom atom ^^ P.space ^^ to_doc_plaintyp plaintyp ^^ to_doc_hints hints

and to_doc_typcase (typ, hints) =
  match hints with
  | [] -> to_doc_typ typ
  | _ ->
      P.group
        (to_doc_typ typ
        ^^ P.nest 2 (P.concat_map (fun h -> P.break 1 ^^ to_doc_hint h) hints))

and to_doc_targ (targ : targ) = to_doc_plaintyp targ

and to_doc_targs = function
  | [] -> P.empty
  | targs -> P.angles (comma_list to_doc_targ targs)

(* Expressions *)

and to_doc_exp exp = to_doc_exp_ctx ~ctx:Notation exp
and to_doc_exp_arith exp = to_doc_exp_ctx ~ctx:Arith exp

and to_doc_exp_ctx ~ctx (exp : exp) =
  let in_arith = to_doc_exp_arith in
  let in_notation = to_doc_exp in
  let in_atom = to_doc_exp_ctx ~ctx:Atom in
  let lift_binop op = ctx = Atom || (ctx = Notation && is_arith_binop op) in
  let lift_cmpop op = ctx = Atom || (ctx = Notation && is_arith_cmpop op) in
  let lift_unop () = ctx = Atom in
  match exp.it with
  | BoolE b -> P.string (Stdlib.string_of_bool b)
  | NumE (`DecOp, `Nat n) -> P.string (Bigint.to_string n)
  | NumE (`HexOp, `Nat n) ->
      P.string (Printf.sprintf "0x%X" (Bigint.to_int_exn n))
  | NumE (_, n) -> to_doc_num n
  | TextE text -> to_doc_text text
  | VarE id -> to_doc_varid id
  | UnE (op, e) when lift_unop () ->
      P.string "$(" ^^ to_doc_unop op ^^ in_arith e ^^ P.string ")"
  | UnE (op, e) -> to_doc_unop op ^^ in_arith e
  | BinE (l, op, r) when lift_binop op ->
      P.string "$("
      ^^ break_at_op (in_arith l) (to_doc_binop op) (in_arith r)
      ^^ P.string ")"
  | BinE (l, op, r) when ctx = Notation ->
      break_at_op (in_notation l) (to_doc_binop op) (in_notation r)
  | BinE (l, op, r) -> break_at_op (in_arith l) (to_doc_binop op) (in_arith r)
  | CmpE (l, op, r) when lift_cmpop op ->
      P.string "$("
      ^^ break_at_op (in_arith l) (to_doc_cmpop op) (in_arith r)
      ^^ P.string ")"
  | CmpE (l, op, r) when ctx = Notation ->
      break_at_op (in_notation l) (to_doc_cmpop op) (in_notation r)
  | CmpE (l, op, r) -> break_at_op (in_arith l) (to_doc_cmpop op) (in_arith r)
  | ArithE e -> P.string "$(" ^^ in_arith e ^^ P.string ")"
  | EpsE -> P.string "eps"
  | ListE exps -> P.brackets (comma_list in_notation exps)
  | ConsE (l, r) -> in_notation l ^^ P.string " :: " ^^ in_notation r
  | CatE (l, r) -> in_notation l ^^ P.string " ++ " ^^ in_notation r
  | IdxE (b, i) -> in_arith b ^^ P.brackets (in_arith i)
  | SliceE (b, l, h) ->
      in_arith b ^^ P.brackets (in_arith l ^^ P.string " : " ^^ in_arith h)
  | LenE e -> P.bar ^^ in_notation e ^^ P.bar
  | MemE (e, s) -> in_notation e ^^ P.string " <- " ^^ in_notation s
  | StrE fields ->
      P.braces
        (comma_list
           (fun (atom, e) -> to_doc_atom atom ^^ P.space ^^ in_atom e)
           fields)
  | DotE (e, atom) -> in_arith e ^^ P.dot ^^ to_doc_atom atom
  | UpdE (b, path, e) ->
      in_arith b
      ^^ P.brackets (to_doc_path path ^^ P.string " = " ^^ in_notation e)
  | ParenE e -> P.parens (in_arith e)
  | TupleE exps -> P.parens (comma_list in_notation exps)
  | CallE (id, targs, args) ->
      to_doc_defid id ^^ to_doc_targs targs ^^ to_doc_args args
  | IterE (e, iter) -> in_arith e ^^ to_doc_iter iter
  | SubE (e, plaintyp) ->
      in_notation e ^^ P.string " <: " ^^ to_doc_plaintyp plaintyp
  | AtomE atom -> to_doc_atom atom
  | SeqE exps -> space_list in_atom exps
  | InfixE ({ it = SeqE []; _ }, atom, r) ->
      (* Prefix infix: empty LHS, no break point, no leading space. *)
      to_doc_atom atom ^^ P.space ^^ in_notation r
  | InfixE (_, atom, _) when is_breakable_atom atom.it ->
      infix_chain in_notation (collect_infix_chain_exp exp)
  | InfixE (l, atom, r) ->
      in_notation l ^^ P.space ^^ to_doc_atom atom ^^ P.space ^^ in_notation r
  | BrackE (a_l, e, a_r) -> to_doc_atom a_l ^^ in_notation e ^^ to_doc_atom a_r
  | HoleE (`Num i) -> P.string (Printf.sprintf "%%%d" i)
  | HoleE `Next -> P.string "%"
  | HoleE `Rest -> P.string "%%"
  | HoleE `None -> P.string "!%"
  | FuseE (l, r) -> in_arith l ^^ P.sharp ^^ in_arith r
  | UnparenE e -> P.string "##" ^^ in_arith e
  | LatexE s ->
      P.string "latex(\"" ^^ P.string (String.escaped s) ^^ P.string "\")"

(* Paths *)

and to_doc_path (path : path) =
  match path.it with
  | RootP -> P.empty
  | IdxP (p, e) -> to_doc_path p ^^ P.brackets (to_doc_exp_arith e)
  | SliceP (p, l, h) ->
      to_doc_path p
      ^^ P.brackets (to_doc_exp_arith l ^^ P.string " : " ^^ to_doc_exp_arith h)
  | DotP (p, atom) -> to_doc_path p ^^ P.dot ^^ to_doc_atom atom

(* Parameters *)

and to_doc_param (param : param) =
  match param.it with
  | ExpP plaintyp -> to_doc_plaintyp plaintyp
  | DefP (defid, tparams, params, plaintyp) ->
      P.string "def " ^^ to_doc_defid defid ^^ to_doc_tparams tparams
      ^^ to_doc_params params ^^ P.string " : " ^^ to_doc_plaintyp plaintyp

and to_doc_params = function
  | [] -> P.empty
  | params -> P.parens (comma_list to_doc_param params)

and to_doc_tparam (tparam : tparam) = P.string tparam.it

and to_doc_tparams = function
  | [] -> P.empty
  | tparams -> P.angles (comma_list to_doc_tparam tparams)

(* Arguments *)

and to_doc_arg (arg : arg) =
  match arg.it with
  | ExpA exp -> to_doc_exp exp
  | DefA defid -> P.string "def " ^^ to_doc_defid defid

and to_doc_args = function
  | [] -> P.empty
  | args -> P.parens (comma_list to_doc_arg args)

(* Premises *)

and to_doc_prem (prem : prem) =
  match prem.it with
  | VarPr (id, plaintyp) ->
      to_doc_varid id ^^ P.string " : " ^^ to_doc_plaintyp plaintyp
  | RulePr (id, exp) ->
      head_body ~head:(to_doc_relid id ^^ P.string ":") ~body:(to_doc_exp exp)
  | RuleNotPr (id, exp) ->
      head_body ~head:(to_doc_relid id ^^ P.string ":/") ~body:(to_doc_exp exp)
  | IfPr exp -> P.string "if " ^^ P.align (to_doc_exp exp)
  | ElsePr -> P.string "otherwise"
  | IterPr (({ it = IterPr _; _ } as inner), iter) ->
      to_doc_prem inner ^^ to_doc_iter iter
  | IterPr (inner, iter) -> P.parens (to_doc_prem inner) ^^ to_doc_iter iter
  | DebugPr exp -> P.string "debug " ^^ P.align (to_doc_exp exp)

and to_doc_prems prems =
  P.concat_map
    (fun prem -> P.hardline ^^ P.string "  -- " ^^ P.align (to_doc_prem prem))
    prems

(* Hints *)

and to_doc_hint (hint : hint) =
  match hint.hintexp.it with
  | SeqE [] -> P.string "hint(" ^^ to_doc_varid hint.hintid ^^ P.string ")"
  | _ ->
      P.string "hint(" ^^ to_doc_varid hint.hintid ^^ P.space
      ^^ to_doc_exp hint.hintexp ^^ P.string ")"

and to_doc_hints hints =
  P.concat_map (fun hint -> P.space ^^ to_doc_hint hint) hints

and to_doc_hints_block hints =
  P.concat_map
    (fun hint -> P.hardline ^^ P.string "  " ^^ to_doc_hint hint)
    hints

(* [VariantTD] is always multi-line; every other definition shape uses
   [head_body] (signature, body) so it fits inline or wraps with the
   separator leading the body. *)
let to_doc_def (def : def) =
  match def.it with
  | SynD syns ->
      P.string "syntax "
      ^^ comma_list
           (fun (typid, tparams) ->
             to_doc_typid typid ^^ to_doc_tparams tparams)
           syns
  | TypD (typid, tparams, deftyp, hints) -> (
      let head =
        P.string "syntax " ^^ to_doc_typid typid ^^ to_doc_tparams tparams
        ^^ to_doc_hints hints ^^ P.string " ="
      in
      match deftyp.it with
      | VariantTD typcases ->
          head
          ^^ P.concat_map
               (fun c -> P.hardline ^^ P.string "  | " ^^ to_doc_typcase c)
               typcases
      | PlainTD plaintyp -> head_body ~head ~body:(to_doc_plaintyp plaintyp)
      | StructTD typfields ->
          head_body ~head
            ~body:(P.braces (comma_list to_doc_typfield typfields)))
  | VarD (varid, plaintyp, hints) ->
      head_body
        ~head:(P.string "var " ^^ to_doc_varid varid ^^ P.string " :")
        ~body:(to_doc_plaintyp plaintyp ^^ to_doc_hints hints)
  | RelD (relid, nottyp, hints) ->
      head_body
        ~head:(P.string "relation " ^^ to_doc_relid relid ^^ P.string ":")
        ~body:(to_doc_nottyp nottyp)
      ^^ to_doc_hints_block hints
  | RuleD (relid, ruleid, exp, prems) ->
      P.string "rule " ^^ to_doc_relid relid ^^ to_doc_ruleid ruleid
      ^^ P.string ":" ^^ P.hardline ^^ P.string "  "
      ^^ P.align (to_doc_exp exp)
      ^^ to_doc_prems prems
  | BuiltinDecD (defid, tparams, params, plaintyp, hints) ->
      head_body
        ~head:
          (P.string "builtin dec " ^^ to_doc_defid defid
         ^^ to_doc_tparams tparams ^^ to_doc_params params)
        ~body:(P.string ": " ^^ to_doc_plaintyp plaintyp)
      ^^ to_doc_hints_block hints
  | DecD (defid, tparams, params, plaintyp, hints) ->
      head_body
        ~head:
          (P.string "dec " ^^ to_doc_defid defid ^^ to_doc_tparams tparams
         ^^ to_doc_params params)
        ~body:(P.string ": " ^^ to_doc_plaintyp plaintyp)
      ^^ to_doc_hints_block hints
  | DefD (defid, tparams, args, exp, prems) ->
      head_body
        ~head:
          (P.string "def " ^^ to_doc_defid defid ^^ to_doc_tparams tparams
         ^^ to_doc_args args)
        ~body:(P.string "= " ^^ to_doc_exp exp)
      ^^ to_doc_prems prems
  | BuiltinGeneratorD (defid, plaintyp, hints) ->
      head_body
        ~head:(P.string "builtin generator " ^^ to_doc_defid defid)
        ~body:(P.string ": " ^^ to_doc_plaintyp plaintyp)
      ^^ to_doc_hints_block hints
  | PropertyD (relid, prems, goal, hints) ->
      P.string "property " ^^ to_doc_relid relid ^^ to_doc_hints hints
      ^^ P.string ":" ^^ P.hardline ^^ P.string "  "
      ^^ P.align (to_doc_prem goal)
      ^^ to_doc_prems prems
  | SepD -> P.empty

(* [SepD] is dropped; one blank line between every pair of remaining defs. *)
let to_doc_spec (spec : spec) =
  let defs =
    List.filter
      (fun (d : def) -> match d.it with SepD -> false | _ -> true)
      spec
  in
  P.separate (P.hardline ^^ P.hardline) (List.map to_doc_def defs) ^^ P.hardline

(* Public boundary *)

let pp_of_doc to_doc fmt x =
  P.ToFormatter.pretty ribbon_fraction line_width fmt (to_doc x)

let string_of_doc to_doc x =
  let buf = Buffer.create 1024 in
  P.ToBuffer.pretty ribbon_fraction line_width buf (to_doc x);
  Buffer.contents buf

let pp_num = pp_of_doc to_doc_num
let pp_text = pp_of_doc to_doc_text
let pp_varid = pp_of_doc to_doc_varid
let pp_typid = pp_of_doc to_doc_typid
let pp_relid = pp_of_doc to_doc_relid
let pp_ruleid = pp_of_doc to_doc_ruleid
let pp_defid = pp_of_doc to_doc_defid
let pp_atom = pp_of_doc to_doc_atom
let pp_iter = pp_of_doc to_doc_iter
let pp_typ = pp_of_doc to_doc_typ
let pp_plaintyp = pp_of_doc to_doc_plaintyp
let pp_nottyp = pp_of_doc to_doc_nottyp
let pp_deftyp = pp_of_doc to_doc_deftyp
let pp_exp = pp_of_doc to_doc_exp
let pp_path = pp_of_doc to_doc_path
let pp_param = pp_of_doc to_doc_param
let pp_tparam = pp_of_doc to_doc_tparam
let pp_arg = pp_of_doc to_doc_arg
let pp_prem = pp_of_doc to_doc_prem
let pp_hint = pp_of_doc to_doc_hint
let pp_def = pp_of_doc to_doc_def
let pp_spec = pp_of_doc to_doc_spec
let string_of_num = string_of_doc to_doc_num
let string_of_text = string_of_doc to_doc_text
let string_of_varid = string_of_doc to_doc_varid
let string_of_typid = string_of_doc to_doc_typid
let string_of_relid = string_of_doc to_doc_relid
let string_of_ruleid = string_of_doc to_doc_ruleid
let string_of_defid = string_of_doc to_doc_defid
let string_of_atom = string_of_doc to_doc_atom
let string_of_iter = string_of_doc to_doc_iter
let string_of_typ = string_of_doc to_doc_typ
let string_of_plaintyp = string_of_doc to_doc_plaintyp
let string_of_nottyp = string_of_doc to_doc_nottyp
let string_of_deftyp = string_of_doc to_doc_deftyp
let string_of_exp = string_of_doc to_doc_exp
let string_of_path = string_of_doc to_doc_path
let string_of_param = string_of_doc to_doc_param
let string_of_tparam = string_of_doc to_doc_tparam
let string_of_arg = string_of_doc to_doc_arg
let string_of_prem = string_of_doc to_doc_prem
let string_of_hint = string_of_doc to_doc_hint
let string_of_def = string_of_doc to_doc_def
let string_of_spec = string_of_doc to_doc_spec
