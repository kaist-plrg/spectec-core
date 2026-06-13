open Types
open Common.Domain
open Common.Source

(* Free identifiers of PL expressions and instructions. Like the IL and EL
   variants this collects identifiers without scoping, so over instructions it
   is an occurrence set rather than a strict free-variable set. *)

type t = IdSet.t

let empty = IdSet.empty
let singleton = IdSet.singleton
let ( + ) = IdSet.union

let union_map (free : 'a -> t) (xs : 'a list) : t =
  List.fold_left (fun acc x -> free x + acc) empty xs

(* Expressions *)

let rec free_exp (exp : exp) : t =
  match exp.node.it with
  | VarE id -> singleton id
  | BoolE _ | NumE _ | TextE _ -> empty
  | UnE (_, _, e)
  | LenE e
  | DotE (e, _)
  | UpCastE (_, e)
  | DownCastE (_, e)
  | SubE (e, _)
  | MatchE (e, _) ->
      free_exp e
  | BinE (_, _, e1, e2)
  | CmpE (_, _, e1, e2)
  | ConsE (e1, e2)
  | CatE (e1, e2)
  | MemE (e1, e2)
  | IdxE (e1, e2) ->
      free_exp e1 + free_exp e2
  | SliceE (e1, e2, e3) -> free_exp e1 + free_exp e2 + free_exp e3
  | TupleE es | ListE es -> free_exps es
  | CaseE notexp -> free_exps (Il.Mixfix.args notexp)
  | StrE fields -> fields |> List.map snd |> free_exps
  | OptE (Some e) -> free_exp e
  | OptE None -> empty
  | UpdE (e1, path, e2) -> free_exp e1 + free_path path + free_exp e2
  | CallE (_, _, args) -> free_args args
  | IterE (e, iterexp) -> free_exp e + free_iterexp iterexp

and free_exps (exps : exp list) : t = union_map free_exp exps

and free_path (path : path) : t =
  match path.it with
  | RootP -> empty
  | IdxP (p, e) -> free_path p + free_exp e
  | SliceP (p, e1, e2) -> free_path p + free_exp e1 + free_exp e2
  | DotP (p, _) -> free_path p

and free_arg (arg : arg) : t =
  match arg.it with ExpA e -> free_exp e | DefA _ -> empty

and free_args (args : arg list) : t = union_map free_arg args

and free_iterexp ((_, vars) : iterexp) : t =
  union_map (fun ({ varid; _ } : var) -> singleton varid) vars

and free_iterexps (iterexps : iterexp list) : t =
  union_map free_iterexp iterexps

(* Instructions *)

let rec free_instr (instr : instr) : t =
  match instr.node.it with
  | IfI (cond, iterexps, block, _) ->
      free_exp cond + free_iterexps iterexps + free_block block
  | IfHoldI (_, notexp, iterexps, block, _)
  | IfNotHoldI (_, notexp, iterexps, block, _) ->
      free_exps (Il.Mixfix.args notexp)
      + free_iterexps iterexps + free_block block
  | CaseI (scrut, cases, _) -> free_exp scrut + union_map free_case cases
  | OtherwiseI inner -> free_instr inner
  | TryI arms -> union_map free_block arms
  | LetI (e_l, e_r, iterexps) ->
      free_exp e_l + free_exp e_r + free_iterexps iterexps
  | RuleI (_, notexp, iterexps) ->
      free_exps (Il.Mixfix.args notexp) + free_iterexps iterexps
  | ResultI es -> free_exps es
  | ReturnI e | DebugI e -> free_exp e
  | DestructI (fields, src) ->
      (fields |> List.map snd |> free_exps) + free_exp src
  | CheckLetI (e_l, e_r, block) ->
      free_exp e_l + free_exp e_r + free_block block
  | OptionGetI (e_l, e_r) -> free_exp e_l + free_exp e_r

and free_block (instrs : instr list) : t = union_map free_instr instrs
and free_case ((guard, block) : case) : t = free_guard guard + free_block block

and free_guard (guard : guard) : t =
  match guard with
  | BoolG _ | SubG _ | MatchG _ -> empty
  | CmpG (_, _, e) | MemG e -> free_exp e
  | CheckLetSubG (_, e) | CheckLetMatchG (_, e) -> free_exp e
