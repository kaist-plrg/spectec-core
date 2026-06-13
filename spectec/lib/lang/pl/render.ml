(** AsciiDoc prose renderer for PL. The splice frame is the splicer's
    responsibility; this module only emits inner fragments. *)

open Common.Source
open Types
open Annot
module F = Format
module Mixfix = Il.Mixfix
module Atom = Xl.Atom

(* Asciidoc rendering context *)

type context = { in_code : bool; in_link : bool }

let in_prose = { in_code = false; in_link = false }
let in_code = { in_code = true; in_link = false }
let in_link = { in_code = false; in_link = true }
let code (ctx : context) = { ctx with in_code = true }
let link (ctx : context) = { ctx with in_link = true }

(* Asciidoc utils *)

let adoc_subscript (s : string) = "~" ^ s ^ "~"
let adoc_superscript (s : string) = "^" ^ s ^ "^"
let adoc_mono (s : string) = "`" ^ s ^ "`"

(* Unconstrained (triple `+`): the constrained single-`+` form only pairs at
   word boundaries, so a bracket abutting other markup leaks its delimiters. *)
let adoc_passthrough (s : string) = "+++" ^ s ^ "+++"

(* Escape a literal string for inline AsciiDoc: a character the inline
   substitutor would re-read becomes its named entity (or a passthrough span
   where none exists); benign characters stay bare so the source reads
   naturally. *)
let adoc_escape (s : string) : string =
  let escape_char = function
    | '*' -> "{asterisk}"
    | '+' -> "{plus}"
    | '<' -> "{lt}"
    | '&' -> "{amp}"
    | '[' -> "{startsb}"
    | ']' -> "{endsb}"
    | '^' -> "{caret}"
    | '~' -> "{tilde}"
    | '`' -> "{backtick}"
    | '\\' -> "{backslash}"
    | ('{' | '}' | '#' | '_') as c -> adoc_passthrough (String.make 1 c)
    | c -> String.make 1 c
  in
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_string buf (escape_char c)) s;
  Buffer.contents buf

let adoc_as_code (ctx : context) (s : string) : string =
  if ctx.in_code then s else adoc_mono s

let adoc_ordered_bullet level =
  F.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '.')

let adoc_unordered_bullet level =
  F.asprintf "%s%s " (String.make level ' ') (String.make (level + 1) '*')

let adoc_link ~(link : string) (text : string) : string =
  let brackets = String.contains text '[' || String.contains text ']' in
  let angles = String.contains text '<' || String.contains text '>' in
  match (brackets, angles) with
  | false, _ -> "xref:" ^ link ^ "[" ^ text ^ "]"
  | true, false -> "<<" ^ link ^ "," ^ text ^ ">>"
  | true, true -> text

let adoc_as_link (ctx : context) ~link (s : string) : string =
  if ctx.in_link then s else adoc_link ~link s

let reindent_lines ?(level = 0) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  String.concat ("\n" ^ adoc_unordered_bullet level) lines

let unindent_lines (s : string) : string =
  s |> String.split_on_char '\n' |> String.concat ""

let render_list (items : string list) : string =
  match items with
  | [] -> ""
  | [ item ] -> item
  | [ a; b ] -> a ^ " and " ^ b
  | _ ->
      let items_rev = List.rev items in
      let init, last = (items_rev |> List.tl |> List.rev, List.hd items_rev) in
      String.concat ", " init ^ ", and " ^ last

let is_underscored (id : id) : bool = String.length id.it > 0 && id.it.[0] = '_'

(* Identifiers / atoms *)

let string_of_relid = Sl.Print.string_of_relid
let string_of_defid = Sl.Print.string_of_defid

let render_varid (ctx : context) (id_var : id) =
  if is_underscored id_var then adoc_passthrough "_" |> adoc_as_code ctx
  else
    let slices = String.split_on_char '_' id_var.it in
    match slices with
    | [] -> assert false
    | [ var_type ] -> var_type |> adoc_as_code ctx
    | var_type :: subs ->
        var_type ^ (subs |> String.concat "_" |> adoc_subscript)
        |> adoc_as_code ctx

(* Curated notation atoms only; concrete operators render literally. *)
let unicode_of_atom : Xl.Atom.t -> string option = function
  | Turnstile -> Some "\xE2\x8A\xA2" (* U+22A2 RIGHT TACK *)
  | Tilesturn -> Some "\xE2\x8A\xA3" (* U+22A3 LEFT TACK *)
  | Sub -> Some "\xE2\x8A\x91" (* U+2291 SQUARE IMAGE OF OR EQUAL TO *)
  | Sup -> Some "\xE2\x8A\x92" (* U+2292 SQUARE ORIGINAL OF OR EQUAL TO *)
  | Arrow | ArrowSub -> Some "\xE2\x86\x92" (* U+2192 RIGHTWARDS ARROW *)
  | DoubleArrowSub -> Some "\xE2\x87\x92" (* U+21D2 RIGHTWARDS DOUBLE ARROW *)
  | DoubleArrowLong ->
      Some "\xE2\x9F\xB9" (* U+27F9 LONG RIGHTWARDS DOUBLE ARROW *)
  | SqArrow -> Some "\xE2\x86\x9D" (* U+219D RIGHTWARDS WAVE ARROW *)
  | SqArrowStar -> Some "\xE2\x86\x9D*"
  | _ -> None

let code_of_atom (atom : Mixfix.atom) =
  match atom.it with
  | Xl.Atom.Tag _ -> ""
  | a -> (
      match unicode_of_atom a with
      | Some s -> s
      | None -> adoc_escape (Xl.Atom.to_string a))

let code_of_mixop (mixop : mixop) : string =
  let arity = Mixfix.arity mixop in
  let placeholders = List.init arity (fun _ -> "%") in
  Mixfix.assemble ~string_of_atom:code_of_atom mixop placeholders |> String.trim

let code_of_iter (iter : Sl.iter) : string =
  match iter with
  | List -> "{asterisk}" |> adoc_superscript
  | Opt -> "?" |> adoc_superscript

let code_of_iterexp ((iter, _) : iterexp) = code_of_iter iter

let code_of_typ (ctx : context) (typ : typ) : string =
  Sl.Print.string_of_typ typ |> adoc_as_code ctx

let render_required_typ (ctx : context) (typ : typ) : string =
  let name = Sl.Print.string_of_typ typ in
  let article =
    if name <> "" && String.contains "aeiouAEIOU" name.[0] then "an" else "a"
  in
  article ^ " " ^ code_of_typ ctx typ

let render_unop = Sl.Print.string_of_unop

let render_binop (ctx : context) (binop : binop) =
  if ctx.in_code then Sl.Print.string_of_binop binop
  else
    match binop with
    | `AndOp -> "and"
    | `OrOp -> "or"
    | `ImplOp -> "implies"
    | `EquivOp -> "is equivalent to"
    | _ -> Sl.Print.string_of_binop binop

let render_cmpop (ctx : context) (cmpop : cmpop) =
  if ctx.in_code then Sl.Print.string_of_cmpop cmpop
  else
    match cmpop with
    | `EqOp -> "is equal to"
    | `NeOp -> "is not equal to"
    | `LtOp -> "is less than"
    | `GtOp -> "is greater than"
    | `LeOp -> "is less than or equal to"
    | `GeOp -> "is greater than or equal to"

let render_alter_hint ?(caps = false) (ctx : context) (h : Hints.Alter.t)
    (render_base : string -> string) (render : context -> 'a -> string)
    (items : 'a list) : string =
  let render_atom (atom : Mixfix.atom) : string =
    code_of_atom atom |> adoc_as_code ctx
  in
  items
  |> Hints.Alter.alternate ~base_text:render_base ~base_atom:render_atom h
       (fun a -> render ctx a)
  |> fun s -> if caps then String.capitalize_ascii s else s

(* Expressions *)

let rec render_exp (ctx : context) (exp : exp) : string =
  let in_code_ctx = code ctx in
  match exp.node.it with
  | BoolE b -> string_of_bool b |> adoc_as_code ctx
  | NumE n -> Sl.Print.string_of_num n |> adoc_as_code ctx
  | TextE text -> "\"" ^ String.escaped text ^ "\"" |> adoc_as_code ctx
  | VarE id_var -> render_varid in_code_ctx id_var |> adoc_as_code ctx
  | UnE (unop, _, exp_inner) -> (
      match render_negated_exp_opt ctx exp_inner with
      | Some s -> s
      | None when (not ctx.in_code) && unop = `NotOp ->
          (* In prose context `~` would land inside a mono span and get
             read as a subscript delimiter by AsciiDoc, breaking the
             surrounding markup. Use the English word and let the inner
             expression carry its own mono wrap. *)
          "not " ^ render_exp ctx exp_inner
      | None ->
          render_unop unop ^ render_exp in_code_ctx exp_inner
          |> adoc_as_code ctx)
  | BinE (`ImplOp, _, exp_l, exp_r) when not ctx.in_code ->
      "if " ^ render_exp ctx exp_l ^ ", then " ^ render_exp ctx exp_r
  | BinE (binop, _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_binop ctx binop ^ " "
      ^ render_exp ctx exp_r
  | CmpE (cmpop, _, exp_l, exp_r) ->
      render_exp ctx exp_l ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx exp_r
  | UpCastE (_, exp_inner) | DownCastE (_, exp_inner) ->
      render_exp ctx exp_inner
  | SubE (exp_inner, typ) ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp_inner)
        (code_of_typ ctx typ)
  | MatchE (exp_inner, pattern) ->
      F.asprintf "%s is %s" (render_exp ctx exp_inner)
        (prose_of_pattern ctx pattern)
  | TupleE exps -> "( " ^ render_exps ctx ~sep:", " exps ^ " )"
  | CaseE notexp -> (
      if ctx.in_code then code_of_notexp ctx notexp
      else
        match exp.hints.prose with
        | Some hints ->
            let exps = Mixfix.args notexp in
            render_alter_hint ctx hints (reindent_lines ~level:0) render_exp
              exps
        | None -> code_of_notexp ctx notexp)
  | StrE expfields ->
      adoc_passthrough "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp_f) ->
               code_of_atom atom ^ " " ^ render_exp ctx exp_f)
             expfields)
      ^ adoc_passthrough "}"
  | OptE (Some exp_inner) -> render_exp ctx exp_inner
  | OptE None -> "·" |> adoc_as_code ctx
  | ListE [] -> "·" |> adoc_as_code ctx
  | ListE [ exp_inner ] -> render_exp in_code_ctx exp_inner |> adoc_as_code ctx
  | ListE exps ->
      adoc_passthrough "[" ^ " "
      ^ render_exps in_code_ctx ~sep:", " exps
      ^ " " ^ adoc_passthrough "]"
      |> adoc_as_code ctx
  | ConsE (exp_h, exp_t) ->
      render_exp in_code_ctx exp_h
      ^ " {two-colons} "
      ^ render_exp in_code_ctx exp_t
      |> adoc_as_code ctx
  | CatE (exp_l, exp_r) ->
      if ctx.in_code then render_exp ctx exp_l ^ " {pp} " ^ render_exp ctx exp_r
      else render_exp ctx exp_l ^ " concatenated with " ^ render_exp ctx exp_r
  | MemE (exp_e, exp_s) ->
      render_exp ctx exp_e ^ " is in " ^ render_exp ctx exp_s
  | LenE exp_inner -> "the length of " ^ render_exp ctx exp_inner
  | DotE (exp_b, atom) ->
      render_exp in_code_ctx exp_b ^ "." ^ code_of_atom atom |> adoc_as_code ctx
  | IdxE (exp_b, exp_i) ->
      render_exp in_code_ctx exp_b ^ "[" ^ render_exp in_code_ctx exp_i ^ "]"
      |> adoc_as_code ctx
  | SliceE (exp_b, exp_l, exp_h) ->
      render_exp in_code_ctx exp_b
      ^ "["
      ^ render_exp in_code_ctx exp_l
      ^ " : "
      ^ render_exp in_code_ctx exp_h
      ^ "]"
      |> adoc_as_code ctx
  | UpdE (exp_b, path, exp_f) ->
      if ctx.in_code then
        render_exp in_code_ctx exp_b
        ^ "["
        ^ render_path in_code_ctx path
        ^ " = "
        ^ render_exp in_code_ctx exp_f
        ^ "]"
        |> adoc_as_code ctx
      else
        (render_exp in_code_ctx exp_b |> adoc_as_code ctx)
        ^ " with "
        ^ (render_path in_code_ctx path |> adoc_as_code ctx)
        ^ " set to "
        ^ (render_exp in_code_ctx exp_f |> adoc_as_code ctx)
  | CallE (id, targs, args) -> (
      let hint_in = exp.hints.prose_in in
      let hint_true = exp.hints.prose_true in
      if ctx.in_code then
        string_of_defid id
        ^ Sl.Print.string_of_targs targs
        ^ render_args (ctx |> link |> code) args
        |> adoc_as_link ctx ~link:id.it
        |> adoc_as_code ctx
      else
        match (hint_in, hint_true) with
        | Some hints, _ | _, Some hints ->
            render_alter_hint (link ctx) hints (reindent_lines ~level:0)
              render_arg args
            |> adoc_as_link ctx ~link:id.it
        | None, None ->
            string_of_defid id
            ^ Sl.Print.string_of_targs targs
            ^ render_args (ctx |> link |> code) args
            |> adoc_as_link ctx ~link:id.it
            |> adoc_as_code ctx)
  | IterE (exp_inner, (_, [])) -> render_exp ctx exp_inner
  | IterE (({ node = { it = VarE _; _ }; _ } as exp_inner), iterexp)
  | IterE (({ node = { it = TupleE _; _ }; _ } as exp_inner), iterexp) ->
      render_exp in_code_ctx exp_inner ^ code_of_iterexp iterexp
      |> adoc_as_code ctx
  | IterE (exp_inner, iterexp) ->
      let sexp = render_exp in_code_ctx exp_inner in
      if String.contains sexp ' ' then
        "( " ^ sexp ^ " )" ^ code_of_iterexp iterexp |> adoc_as_code ctx
      else sexp ^ code_of_iterexp iterexp |> adoc_as_code ctx

and render_negated_exp_opt (ctx : context) (exp_inner : exp) : string option =
  match exp_inner.node.it with
  | MatchE (exp_e, pattern) ->
      Some
        (F.asprintf "%s is not %s" (render_exp ctx exp_e)
           (prose_of_pattern ctx pattern))
  | SubE (exp_e, typ) ->
      Some
        (F.asprintf "%s does not have type %s"
           (render_exp_as_code ctx exp_e)
           (code_of_typ ctx typ))
  | MemE (exp_e, exp_s) ->
      Some
        (F.asprintf "%s is not in %s"
           (render_exp_as_code ctx exp_e)
           (render_exp_as_code ctx exp_s))
  | CallE (id, _targs, args) when not ctx.in_code -> (
      match exp_inner.hints.prose_false with
      | Some hints ->
          Some
            (render_alter_hint (link ctx) hints (reindent_lines ~level:0)
               render_arg args
            |> adoc_as_link ctx ~link:id.it)
      | None ->
          Some
            (render_unop `NotOp ^ render_exp (code ctx) exp_inner
            |> adoc_as_code ctx))
  | _ -> None

and render_exp_as_code (ctx : context) (exp : exp) : string =
  render_exp (code ctx) exp |> adoc_as_code ctx

and render_exps (ctx : context) ?sep (exps : exp list) =
  match (ctx.in_code, sep) with
  | _, Some s -> String.concat s (List.map (render_exp ctx) exps)
  | true, None -> String.concat ", " (List.map (render_exp ctx) exps)
  | false, None -> render_list (List.map (render_exp ctx) exps)

and code_of_notexp (ctx : context) (notexp : notexp) : string =
  let mixop = Mixfix.to_mixop notexp in
  let args = Mixfix.args notexp in
  let sexps = List.map (render_exp in_code) args in
  Mixfix.assemble ~string_of_atom:code_of_atom mixop sexps |> adoc_as_code ctx

and code_of_pattern (pattern : pattern) =
  match pattern with
  | Il.CaseP mixop -> code_of_mixop mixop
  | Il.ListP `Cons -> "_ :: _"
  | Il.ListP (`Fixed len) -> F.asprintf "[ _/%d ]" len
  | Il.ListP `Nil -> "[]"
  | Il.OptP `Some -> "(_)"
  | Il.OptP `None -> "()"

and prose_of_pattern (ctx : context) (pattern : pattern) : string =
  match pattern with
  | Il.ListP `Nil -> "an empty list"
  | Il.ListP `Cons -> "a non-empty list"
  | Il.ListP (`Fixed len) -> F.asprintf "a list of length %d" len
  | Il.OptP `None -> "none"
  | Il.OptP `Some -> "defined"
  | Il.CaseP _ -> code_of_pattern pattern |> adoc_as_code ctx

and render_path (ctx : context) (path : path) : string =
  match path.it with
  | RootP -> ""
  | IdxP (path, e) -> render_path ctx path ^ "[" ^ render_exp ctx e ^ "]"
  | SliceP (path, e_l, e_h) ->
      render_path ctx path ^ "[" ^ render_exp ctx e_l ^ " : "
      ^ render_exp ctx e_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> code_of_atom atom
  | DotP (path, atom) -> render_path ctx path ^ "." ^ code_of_atom atom

and render_arg (ctx : context) (arg : arg) =
  match arg.it with
  | ExpA exp -> render_exp ctx exp
  | DefA defid -> string_of_defid defid |> adoc_as_code ctx

and render_args (ctx : context) (args : arg list) =
  match args with
  | [] -> ""
  | _ -> "(" ^ String.concat ", " (List.map (render_arg ctx) args) ^ ")"

(* Parameters *)

let render_param (ctx : context) (param : param) : string =
  match param.it with
  | ExpP (_typ, exp) -> render_exp ctx exp
  | DefP defid -> string_of_defid defid |> adoc_as_code ctx

let render_params (ctx : context) (params : param list) : string =
  match params with
  | [] -> ""
  | _ -> "(" ^ String.concat ", " (List.map (render_param ctx) params) ^ ")"

(* Guards *)

let render_guard (ctx : context) (exp_scrut : exp) (guard : guard) : string =
  match guard with
  | BoolG true -> render_exp ctx exp_scrut
  | BoolG false ->
      let scrut_node = exp_scrut.node in
      let neg_inner =
        UnE (`NotOp, `BoolT, exp_scrut) $$ (scrut_node.at, scrut_node.note)
      in
      render_exp ctx { node = neg_inner; hints = empty }
  | CmpG (cmpop, _, e) ->
      render_exp ctx exp_scrut ^ " " ^ render_cmpop ctx cmpop ^ " "
      ^ render_exp ctx e
  | SubG typ ->
      F.asprintf "%s has type %s"
        (render_exp_as_code ctx exp_scrut)
        (code_of_typ ctx typ)
  | MatchG pattern ->
      F.asprintf "%s is %s" (render_exp ctx exp_scrut)
        (prose_of_pattern ctx pattern)
  | MemG e -> render_exp ctx exp_scrut ^ " is in " ^ render_exp ctx e
  | CheckLetSubG (_, target) | CheckLetMatchG (_, target) ->
      F.asprintf "%s is %s" (render_exp ctx exp_scrut) (render_exp ctx target)

let render_condition (ctx : context) (cond : exp) : string =
  match cond.node.it with
  | CmpE (cmpop, _, exp_l, exp_r) -> (
      let l = render_exp ctx exp_l and r = render_exp ctx exp_r in
      match cmpop with
      | `EqOp -> F.asprintf "%s must equal %s" l r
      | `NeOp -> F.asprintf "%s must not equal %s" l r
      | `LtOp -> F.asprintf "%s must be less than %s" l r
      | `GtOp -> F.asprintf "%s must be greater than %s" l r
      | `LeOp -> F.asprintf "%s must be less than or equal to %s" l r
      | `GeOp -> F.asprintf "%s must be greater than or equal to %s" l r)
  | MatchE (exp_l, pattern) ->
      F.asprintf "%s must be %s" (render_exp ctx exp_l)
        (prose_of_pattern ctx pattern)
  | SubE (exp_l, typ) ->
      F.asprintf "%s must have type %s"
        (render_exp_as_code ctx exp_l)
        (code_of_typ ctx typ)
  | _ -> F.asprintf "Check that %s" (render_exp ctx cond)

let render_iterexp_suffix (ctx : context) (iterexps : iterexp list) : string =
  match iterexps with
  | [] -> ""
  | _ ->
      let vars = List.concat_map (fun (_, vars) -> vars) iterexps in
      if vars = [] then ""
      else
        let render_in_var ({ varid; iters; _ } : var) =
          let it_ctx = in_code in
          let var_text =
            if is_underscored varid then
              adoc_passthrough "_" |> adoc_as_code ctx
            else
              render_varid it_ctx varid
              ^ String.concat "" (List.map code_of_iter iters)
              |> adoc_as_code ctx
          in
          let list_text =
            (if is_underscored varid then adoc_passthrough "_"
             else
               render_varid it_ctx varid
               ^ String.concat "" (List.map code_of_iter iters)
               ^ code_of_iter List)
            |> adoc_as_code ctx
          in
          F.asprintf "%s in %s" var_text list_text
        in
        ", for all " ^ (vars |> List.map render_in_var |> render_list)

(* Relations *)

let render_rel_title_math (ctx : context) ((mixop, inputs) : mixop * int list)
    (exps : exp list) : string =
  let sexps = List.map (render_exp in_code) exps in
  let num_outputs = Mixfix.arity mixop - List.length sexps in
  let holes = List.init num_outputs (fun _ -> "%") in
  let padded = Hints.Input.combine inputs sexps holes in
  Mixfix.assemble ~string_of_atom:code_of_atom mixop padded |> adoc_as_code ctx

(* Instructions *)

let rec render_instr ?(level = 0) ?(unordered = false) (instr : instr) : string
    =
  let bullet =
    if unordered then adoc_unordered_bullet level else adoc_ordered_bullet level
  in
  let hints = instr.hints in
  match instr.node.it with
  | IfI (cond, iterexps, block, _phantom) ->
      let check_line =
        F.asprintf "%s%s%s." bullet
          (render_condition in_prose cond)
          (render_iterexp_suffix in_prose iterexps)
      in
      if block = [] then check_line else check_line ^ render_instrs ~level block
  | IfHoldI (id_rel, notexp, iterexps, block, _phantom) ->
      let head =
        let mixop = Mixfix.to_mixop notexp in
        let exps = Mixfix.args notexp in
        match hints.prose_true with
        | Some h ->
            render_alter_hint in_link h (reindent_lines ~level:0) render_exp
              exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        | None ->
            (code_of_notexp in_prose (Mixfix.fill mixop exps)
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
            ^ " holds"
      in
      F.asprintf "%sIf %s%s:%s" bullet head
        (render_iterexp_suffix in_prose iterexps)
        (render_instrs ~level:(level + 1) block)
  | IfNotHoldI (id_rel, notexp, iterexps, block, _phantom) ->
      let head =
        let mixop = Mixfix.to_mixop notexp in
        let exps = Mixfix.args notexp in
        match hints.prose_false with
        | Some h ->
            render_alter_hint in_link h (reindent_lines ~level:0) render_exp
              exps
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        | None ->
            (code_of_notexp in_prose (Mixfix.fill mixop exps)
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
            ^ " does not hold"
      in
      F.asprintf "%sIf %s%s:%s" bullet head
        (render_iterexp_suffix in_prose iterexps)
        (render_instrs ~level:(level + 1) block)
  | CaseI (exp_scrut, cases, _phantom) -> (
      match cases with
      | [ (guard, arm_body) ] ->
          F.asprintf "%sCheck that %s:%s" bullet
            (render_guard in_prose exp_scrut guard)
            (render_instrs ~level:(level + 1) arm_body)
      | _ ->
          let render_arm idx ((guard, arm_body) : case) =
            let kw = if idx = 0 then "If" else "Otherwise, if" in
            F.asprintf "%s%s %s:%s" bullet kw
              (render_guard in_prose exp_scrut guard)
              (render_instrs ~level:(level + 1) arm_body)
          in
          String.concat "\n" (List.mapi render_arm cases))
  | OtherwiseI inner ->
      F.asprintf "%sOtherwise:\n%s" bullet
        (render_instr ~level:(level + 1) inner)
  | TryI arms ->
      let arm_level = level + 1 in
      let body_level = level + 2 in
      let render_arm arm =
        F.asprintf "%s{empty}%s"
          (adoc_ordered_bullet arm_level)
          (render_instrs ~level:body_level arm)
      in
      F.asprintf "%sTry:\n%s" bullet
        (String.concat "\n" (List.map render_arm arms))
  | LetI (exp_l, exp_r, iterexps) ->
      F.asprintf "%sLet %s be %s%s." bullet
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)
        (render_iterexp_suffix in_prose iterexps)
  | RuleI (id_rel, notexp, iterexps) ->
      let mixop = Mixfix.to_mixop notexp in
      let exps = Mixfix.args notexp in
      let hint_in = hints.prose_in in
      let hint_out = hints.prose_out in
      let inputs = hints.rel_inputs |> Option.value ~default:[] in
      let exps_in, exps_out = Hints.Input.split inputs exps in
      let rule_body =
        match (hint_in, hint_out, exps_out) with
        | Some h_in, _, [] when hint_out = None ->
            (* Predicate-shaped relation called as a result-less premise. *)
            render_alter_hint in_link h_in unindent_lines render_exp exps_in
            |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
        | Some h_in, hint_out_opt, _ ->
            let prose_out =
              match hint_out_opt with
              | Some h_out ->
                  render_alter_hint in_link h_out unindent_lines render_exp
                    exps_out
              | None -> render_exps in_prose exps_out
            in
            let prose_in =
              render_alter_hint in_link h_in unindent_lines render_exp exps_in
              |> adoc_as_link in_prose ~link:(string_of_relid id_rel)
            in
            F.asprintf "Let %s be the result of %s" prose_out prose_in
        | _ ->
            F.asprintf "Let %s"
              (code_of_notexp in_prose (Mixfix.fill mixop exps)
              |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
      in
      F.asprintf "%s%s%s." bullet rule_body
        (render_iterexp_suffix in_prose iterexps)
  | ResultI exps -> (
      match (hints.prose_out, exps) with
      | Some h, _ ->
          F.asprintf "%sResult in %s." bullet
            (render_alter_hint in_prose h (reindent_lines ~level:0) render_exp
               exps)
      | None, [] -> bullet ^ "The relation holds."
      | None, _ ->
          F.asprintf "%sResult in %s." bullet (render_exps in_prose exps))
  | ReturnI e -> F.asprintf "%sReturn %s." bullet (render_exp in_prose e)
  | DebugI e -> F.asprintf "%s(debug: %s)" bullet (render_exp in_prose e)
  | DestructI (fields, exp_source) -> (
      let projections =
        List.filter_map
          (fun (name_opt, exp_target) ->
            Option.map (fun name -> (name, exp_target)) name_opt)
          fields
      in
      match projections with
      | [ (name, exp_target) ] ->
          F.asprintf "%sLet %s be the %s of %s." bullet
            (render_exp in_prose exp_target)
            name
            (render_exp in_prose exp_source)
      | _ ->
          let names = List.map fst projections in
          let exps_target = List.map snd projections in
          F.asprintf "%sLet %s be %s of %s." bullet
            (render_exps in_prose exps_target)
            (render_list (List.map (fun s -> "the " ^ s) names))
            (render_exp in_prose exp_source))
  | CheckLetI (exp_l, exp_r, block) ->
      let head =
        match exp_l.node.it with
        | CaseE _ ->
            F.asprintf "%s%s must be %s." bullet
              (render_exp in_prose exp_r)
              (render_exp in_prose exp_l)
        | _ ->
            F.asprintf "%sLet %s be %s, which must be %s." bullet
              (render_exp_as_code in_prose exp_l)
              (render_exp in_prose exp_r)
              (render_required_typ in_prose (exp_l.node.note $ exp_l.node.at))
      in
      if block = [] then head else head ^ render_instrs ~level block
  | OptionGetI (exp_l, exp_r) ->
      F.asprintf "%sLet %s be %s, which must exist." bullet
        (render_exp_as_code in_prose exp_l)
        (render_exp in_prose exp_r)

and render_instrs ?(level = 0) (instrs : instr list) : string =
  match instrs with
  | [
   ({ node = { it = ReturnI ({ node = { it = BoolE _; _ }; _ } as e); _ }; _ } :
     instr);
  ] ->
      F.asprintf " return %s." (render_exp_as_code in_prose e)
  | _ -> "\n" ^ (List.map (render_instr ~level) instrs |> String.concat "\n")

(* Definitions *)

let render_rel_title_adoc (hints : hints) (id_rel : id)
    (sig_ : mixop * int list) (exps : exp list) : string =
  (* PL [RelD] carries only the input expressions; [prose_out] is
     consulted at call sites and [ResultI] instead. *)
  match (hints.prose_in, hints.prose_true) with
  | Some h_in, _ ->
      F.asprintf "%s:\n\n%s%s."
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h_in (reindent_lines ~level:1)
           render_exp exps)
  | _, Some h_true ->
      F.asprintf "%s:\n\n%s%s"
        (Sl.Print.string_of_relid id_rel
        |> adoc_as_link in_prose ~link:(string_of_relid id_rel))
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h_true (reindent_lines ~level:0)
           render_exp exps)
  | _ ->
      F.asprintf "%s: %s"
        (Sl.Print.string_of_relid id_rel)
        (render_rel_title_math in_prose sig_ exps)
      |> adoc_as_link in_prose ~link:(string_of_relid id_rel)

let render_func_header (hints : hints) (id_func : id) (tparams : tparam list)
    (args : arg list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some h, _ | _, Some h ->
      render_alter_hint ~caps:true in_prose h (reindent_lines ~level:0)
        render_arg args
      |> adoc_as_link in_prose ~link:id_func.it
  | None, None ->
      string_of_defid id_func
      ^ Sl.Print.string_of_tparams tparams
      ^ render_args (in_link |> code) args
      |> adoc_as_link in_prose ~link:id_func.it

let render_func_title_adoc (hints : hints) (id_func : id)
    (tparams : tparam list) (args : arg list) : string =
  match (hints.prose_in, hints.prose_true) with
  | Some h, _ | _, Some h ->
      F.asprintf "%s:\n\n%s%s"
        (string_of_defid id_func |> adoc_as_link in_prose ~link:id_func.it)
        (adoc_unordered_bullet 0)
        (render_alter_hint ~caps:true in_prose h (reindent_lines ~level:0)
           render_arg args)
  | None, None ->
      (string_of_defid id_func |> adoc_as_link in_prose ~link:id_func.it)
      ^ Sl.Print.string_of_tparams tparams
      ^ render_args (in_link |> code) args

let strip_leading_newline (s : string) : string =
  if String.length s > 0 && s.[0] = '\n' then
    String.sub s 1 (String.length s - 1)
  else s

let render_elseblock (elseblock_opt : elseblock option) : string =
  match elseblock_opt with
  | None | Some [] -> ""
  | Some block -> render_instrs ~level:0 block

let render_defined_rel_def (hints : hints)
    ((id_rel, sig_, exps, block, elseblock_opt) :
      id * (mixop * int list) * exp list * instr list * elseblock option) :
    string =
  render_rel_title_adoc hints id_rel sig_ exps
  ^ "\n\n"
  ^ strip_leading_newline (render_instrs block)
  ^ render_elseblock elseblock_opt

let render_defined_func_def (hints : hints)
    ((id_func, tparams, args, block, elseblock_opt) :
      id * tparam list * arg list * instr list * elseblock option) : string =
  render_func_header hints id_func tparams args
  ^ "\n\n"
  ^ strip_leading_newline (render_instrs block)
  ^ render_elseblock elseblock_opt

let render_def (def : def) : string option =
  let hints = def.hints in
  match def.node.it with
  | TypD _ -> None
  | RelD (id_rel, sig_, exps, block, elseblock_opt) ->
      Some
        (render_defined_rel_def hints
           (id_rel, sig_, exps, block, elseblock_opt))
  | BuiltinDecD (id_func, tparams, args) ->
      Some (render_func_header hints id_func tparams args)
  | DecD (id_func, tparams, args, block, elseblock_opt) ->
      Some
        (render_defined_func_def hints
           (id_func, tparams, args, block, elseblock_opt))

let render_defs (defs : def list) : string =
  defs |> List.filter_map render_def |> String.concat "\n\n"

let render_spec (spec : spec) : string = render_defs spec
