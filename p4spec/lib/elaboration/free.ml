open Domain.Dom
open Il.Ast
open Attempt
open Envs
open Util.Source

(* Binding environment *)

module Bind = struct
  type t = Single of typ' * Dom.Dim.t | Multi of typ' * Dom.Dim.t

  let get_typ = function Single (typ, _) -> typ | Multi (typ, _) -> typ
  let get_dim = function Single (_, dim) -> dim | Multi (_, dim) -> dim
  let to_string t = t |> get_dim |> Dom.Dim.to_string

  let add_dim (iter : iter) = function
    | Single (typ, iters) -> Single (typ, iters @ [ iter ])
    | Multi (typ, iters) -> Multi (typ, iters @ [ iter ])
end

module BEnv = MakeIdEnv (Bind)

type t = BEnv.t

let transfer (binds : t) : VEnv.t = BEnv.map Bind.get_dim binds
let empty = BEnv.empty
let singleton id typ = BEnv.add id (Bind.Single (typ, [])) BEnv.empty

let union (binds_a : t) (binds_b : t) : t attempt =
  let ids =
    (binds_a |> BEnv.bindings |> List.map fst)
    @ (binds_b |> BEnv.bindings |> List.map fst)
    |> IdSet.of_list
  in
  IdSet.fold
    (fun id binds ->
      let* binds = binds in
      let bind_a = BEnv.find_opt id binds_a in
      let bind_b = BEnv.find_opt id binds_b in
      match (bind_a, bind_b) with
      | Some bind_a, Some bind_b ->
          let typ_a = Bind.get_typ bind_a in
          let dim_a = Bind.get_dim bind_a in
          let _typ_b = Bind.get_typ bind_b in
          let dim_b = Bind.get_dim bind_b in
          if Dom.Dim.equiv dim_a dim_b then
            Ok (BEnv.add id (Bind.Multi (typ_a, dim_a)) binds)
          else
            fail id.at
              (Format.asprintf
                 "inconsistent dimensions for multiple binds of %s: (left) %s, \
                  (right) %s"
                 (Id.to_string id) (Bind.to_string bind_a)
                 (Bind.to_string bind_b))
      | Some bind_a, None -> Ok (BEnv.add id bind_a binds)
      | None, Some bind_b -> Ok (BEnv.add id bind_b binds)
      | None, None -> assert false)
    ids (Ok empty)

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers can only occur in
   trivially invertible constructs, or binder patterns *)

(* Expressions *)

let bind_nontrivial (at : region) (construct : string) (binds : t) : t attempt =
  if BEnv.is_empty binds then Ok empty
  else
    fail at
      (Format.asprintf "invalid binding position(s) for %s in non-invertible %s"
         (BEnv.to_string binds) construct)

let rec bind_exp (ctx : Ctx.t) (exp : exp) : (exp * t) attempt =
  let* binds = bind_exp' ctx.venv exp in
  Ok (exp, binds)

and bind_exp' (bounds : VEnv.t) (exp : exp) : t attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok empty
  | VarE id ->
      if VEnv.mem id bounds then Ok empty else Ok (singleton id exp.note)
  | UnE (_, _, exp) ->
      let* binds = bind_exp' bounds exp in
      bind_nontrivial exp.at "unary operator" binds
  | BinE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* binds = union bind_l bind_r in
      bind_nontrivial exp.at "binary operator" binds
  | CmpE (_, _, exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* binds = union bind_l bind_r in
      bind_nontrivial exp.at "comparison operator" binds
  | TupleE exps -> bind_exps' bounds exps
  | CaseE notexp -> notexp |> snd |> bind_exps' bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (bind_exp' bounds)
      |> Option.value ~default:(Ok empty)
  | StrE expfields -> expfields |> List.map snd |> bind_exps' bounds
  | DotE (exp, _) ->
      let* binds = bind_exp' bounds exp in
      bind_nontrivial exp.at "dot operator" binds
  | ListE exps -> bind_exps' bounds exps
  | ConsE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      union bind_l bind_r
  | CatE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* binds = union bind_l bind_r in
      bind_nontrivial exp.at "concatenation operator" binds
  | MemE (exp_l, exp_r) ->
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_r = bind_exp' bounds exp_r in
      let* binds = union bind_l bind_r in
      bind_nontrivial exp.at "set membership operator" binds
  | LenE exp ->
      let* binds = bind_exp' bounds exp in
      bind_nontrivial exp.at "length operator" binds
  | IdxE (exp_b, exp_i) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_i = bind_exp' bounds exp_i in
      let* binds = union bind_b bind_i in
      bind_nontrivial exp.at "indexing operator" binds
  | SliceE (exp_b, exp_l, exp_h) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_h = bind_exp' bounds exp_h in
      let* binds = union bind_b bind_l in
      let* binds = union binds bind_h in
      bind_nontrivial exp.at "slicing operator" binds
  | UpdE (exp_b, path, exp_f) ->
      let* bind_b = bind_exp' bounds exp_b in
      let* bind_f = bind_exp' bounds exp_f in
      let* bind_p = bind_path' bounds path in
      let* binds = union bind_b bind_f in
      let* binds = union binds bind_p in
      bind_nontrivial exp.at "update operator" binds
  | CallE (_, _, args) ->
      let* binds = bind_args' bounds args in
      bind_nontrivial exp.at "call operator" binds
  | IterE (_, (_, (_ :: _ as binds))) ->
      fail exp.at
        ("iterated expression should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var binds))
  | IterE (exp, (iter, [])) ->
      let* binds = bind_exp' bounds exp in
      let binds = BEnv.map (Bind.add_dim iter) binds in
      Ok binds
  | CastE (exp, _) -> bind_exp' bounds exp

and bind_exps (ctx : Ctx.t) (exps : exp list) : (exp list * t) attempt =
  let* binds = bind_exps' ctx.venv exps in
  Ok (exps, binds)

and bind_exps' (bounds : VEnv.t) (exps : exp list) : t attempt =
  match exps with
  | [] -> Ok empty
  | exp :: exps ->
      let* bind_h = bind_exp' bounds exp in
      let* bind_t = bind_exps' bounds exps in
      union bind_h bind_t

(* Paths *)

and bind_path (ctx : Ctx.t) (path : path) : (path * t) attempt =
  let* binds = bind_path' ctx.venv path in
  Ok (path, binds)

and bind_path' (bounds : VEnv.t) (path : path) : t attempt =
  match path.it with
  | RootP -> Ok empty
  | IdxP (path, exp) ->
      let* bind_p = bind_path' bounds path in
      let* bind_e = bind_exp' bounds exp in
      let* binds = union bind_p bind_e in
      bind_nontrivial path.at "indexing operator" binds
  | SliceP (path, exp_l, exp_h) ->
      let* bind_p = bind_path' bounds path in
      let* bind_l = bind_exp' bounds exp_l in
      let* bind_h = bind_exp' bounds exp_h in
      let* binds = union bind_p bind_l in
      let* binds = union binds bind_h in
      bind_nontrivial path.at "slice operator" binds
  | DotP (path, _) ->
      let* binds = bind_path' bounds path in
      bind_nontrivial path.at "dot operator" binds

(* Arguments *)

and bind_arg (ctx : Ctx.t) (arg : arg) : (arg * t) attempt =
  let* binds = bind_arg' ctx.venv arg in
  Ok (arg, binds)

and bind_arg' (bounds : VEnv.t) (arg : arg) : t attempt =
  match arg.it with ExpA exp -> bind_exp' bounds exp | DefA _ -> Ok empty

and bind_args (ctx : Ctx.t) (args : arg list) : (arg list * t) attempt =
  let* binds = bind_args' ctx.venv args in
  Ok (args, binds)

and bind_args' (bounds : VEnv.t) (args : arg list) : t attempt =
  match args with
  | [] -> Ok empty
  | arg :: args ->
      let* bind_h = bind_arg' bounds arg in
      let* bind_t = bind_args' bounds args in
      union bind_h bind_t

(* Collect binding identifiers,
   while enforcing the invariant that relation inputs should always be bound,
   and also disambiguate `=` in if premises, of whether it is a comparison or a binding *)

let rec bind_prem (ctx : Ctx.t) (prem : prem) : (prem * t) attempt =
  match prem.it with
  | RulePr (id, notexp) ->
      let* binds = bind_rule_prem ctx prem.at id notexp in
      Ok (prem, binds)
  | IfPr exp -> bind_if_prem ctx prem.at exp
  | ElsePr -> Ok (prem, empty)
  | LetPr _ -> fail prem.at "let premise should appear only after bind analysis"
  | IterPr (_, (_, (_ :: _ as binds))) ->
      fail prem.at
        ("iterated premise should initially have no annotations, but got "
        ^ (List.map Il.Print.string_of_var binds |> String.concat ", "))
  | IterPr (prem, (iter, [])) -> bind_iter_prem ctx prem.at prem iter

and bind_rule_prem (ctx : Ctx.t) (at : region) (id : id) (notexp : notexp) :
    t attempt =
  let inputs = Ctx.find_rel ctx id |> snd in
  let exps_input, exps_output =
    notexp |> snd
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)
  in
  let* binds_input = bind_exps' ctx.venv exps_input in
  if not (BEnv.is_empty binds_input) then
    fail at ("rule input has free variable(s): " ^ BEnv.to_string binds_input)
  else bind_exps' ctx.venv exps_output

and bind_if_eq_prem (ctx : Ctx.t) (at : region) (note : region * typ')
    (optyp : optyp) (exp_l : exp) (exp_r : exp) : (prem * t) attempt =
  let* bind_l = bind_exp' ctx.venv exp_l in
  let* bind_r = bind_exp' ctx.venv exp_r in
  match (BEnv.is_empty bind_l, BEnv.is_empty bind_r) with
  | true, true ->
      let prem = IfPr (CmpE (`EqOp, optyp, exp_l, exp_r) $$ note) $ at in
      Ok (prem, empty)
  | false, true ->
      let prem = LetPr (exp_l, exp_r) $ at in
      Ok (prem, bind_l)
  | true, false ->
      let prem = LetPr (exp_r, exp_l) $ at in
      Ok (prem, bind_r)
  | false, false ->
      fail at
        (Format.asprintf
           "cannot bind on both sides of an equality: (left) %s, (right) %s"
           (BEnv.to_string bind_l) (BEnv.to_string bind_r))

and bind_if_cond_prem (ctx : Ctx.t) (at : region) (exp : exp) : unit attempt =
  let* binds = bind_exp' ctx.venv exp in
  if not (BEnv.is_empty binds) then
    fail at ("condition has free variable(s): " ^ BEnv.to_string binds)
  else Ok ()

and bind_if_prem (ctx : Ctx.t) (at : region) (exp : exp) : (prem * t) attempt =
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      bind_if_eq_prem ctx at (exp.at, exp.note) optyp exp_l exp_r
  | _ ->
      let* _ = bind_if_cond_prem ctx at exp in
      let prem = IfPr exp $ at in
      Ok (prem, empty)

and bind_iter_prem (ctx : Ctx.t) (at : region) (prem : prem) (iter : iter) :
    (prem * t) attempt =
  let* prem, binds = bind_prem ctx prem in
  let binds = BEnv.map (Bind.add_dim iter) binds in
  let prem = IterPr (prem, (iter, [])) $ at in
  Ok (prem, binds)

(* Renaming environment *)

module Rename = struct
  type t = IdSet.t

  let to_string = IdSet.to_string ~with_braces:false
end

module REnv = MakeIdEnv (Rename)

(* Rename multiple binds, leaving only the leftmost occurrence intact *)

(* Expressions *)

let rec rename_exp (binds_multi : IdSet.t) (rename : REnv.t) (exp : exp) :
    exp * REnv.t =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> (exp, rename)
  | VarE id -> (
      match (IdSet.mem id binds_multi, REnv.find_opt id rename) with
      | false, Some _ -> assert false
      | true, Some renames ->
          let id_rename =
            Format.asprintf "_%s_%d" (Id.to_string id) (IdSet.cardinal renames)
            $ id.at
          in
          let renames = IdSet.add id_rename renames in
          let rename = REnv.add id renames rename in
          let exp = VarE id_rename $$ (at, note) in
          (exp, rename)
      | true, None -> (exp, REnv.add id IdSet.empty rename)
      | false, None -> (exp, rename))
  | UnE (unop, optyp, exp) ->
      let exp, rename = rename_exp binds_multi rename exp in
      let exp = UnE (unop, optyp, exp) $$ (at, note) in
      (exp, rename)
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_r, rename = rename_exp binds_multi rename exp_r in
      let exp = BinE (binop, optyp, exp_l, exp_r) $$ (at, note) in
      (exp, rename)
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_r, rename = rename_exp binds_multi rename exp_r in
      let exp = CmpE (cmpop, optyp, exp_l, exp_r) $$ (at, note) in
      (exp, rename)
  | TupleE exps ->
      let exps, rename = rename_exps binds_multi rename exps in
      let exp = TupleE exps $$ (at, note) in
      (exp, rename)
  | CaseE notexp ->
      let mixop, exps = notexp in
      let exps, rename = rename_exps binds_multi rename exps in
      let notexp = (mixop, exps) in
      let exp = CaseE notexp $$ (at, note) in
      (exp, rename)
  | OptE exp_opt -> (
      match exp_opt with
      | Some exp ->
          let exp, rename = rename_exp binds_multi rename exp in
          let exp = OptE (Some exp) $$ (at, note) in
          (exp, rename)
      | None -> (exp, rename))
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let exps, rename = rename_exps binds_multi rename exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (exp, rename)
  | DotE (exp, atom) ->
      let exp, rename = rename_exp binds_multi rename exp in
      let exp = DotE (exp, atom) $$ (at, note) in
      (exp, rename)
  | ListE exps ->
      let exps, rename = rename_exps binds_multi rename exps in
      let exp = ListE exps $$ (at, note) in
      (exp, rename)
  | ConsE (exp_l, exp_r) ->
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_r, rename = rename_exp binds_multi rename exp_r in
      let exp = ConsE (exp_l, exp_r) $$ (at, note) in
      (exp, rename)
  | CatE (exp_l, exp_r) ->
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_r, rename = rename_exp binds_multi rename exp_r in
      let exp = CatE (exp_l, exp_r) $$ (at, note) in
      (exp, rename)
  | MemE (exp_l, exp_r) ->
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_r, rename = rename_exp binds_multi rename exp_r in
      let exp = MemE (exp_l, exp_r) $$ (at, note) in
      (exp, rename)
  | LenE exp ->
      let exp, rename = rename_exp binds_multi rename exp in
      let exp = LenE exp $$ (at, note) in
      (exp, rename)
  | IdxE (exp_b, exp_i) ->
      let exp_b, rename = rename_exp binds_multi rename exp_b in
      let exp_i, rename = rename_exp binds_multi rename exp_i in
      let exp = IdxE (exp_b, exp_i) $$ (at, note) in
      (exp, rename)
  | SliceE (exp_b, exp_l, exp_h) ->
      let exp_b, rename = rename_exp binds_multi rename exp_b in
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_h, rename = rename_exp binds_multi rename exp_h in
      let exp = SliceE (exp_b, exp_l, exp_h) $$ (at, note) in
      (exp, rename)
  | UpdE (exp_b, path, exp_f) ->
      let exp_b, rename = rename_exp binds_multi rename exp_b in
      let exp_f, rename = rename_exp binds_multi rename exp_f in
      let path, rename = rename_path binds_multi rename path in
      let exp = UpdE (exp_b, path, exp_f) $$ (at, note) in
      (exp, rename)
  | CallE (id, targs, args) ->
      let args, rename = rename_args binds_multi rename args in
      let exp = CallE (id, targs, args) $$ (at, note) in
      (exp, rename)
  | IterE (_, (_, (_ :: _ as binds))) ->
      error at
        ("iterated expression should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var binds))
  | IterE (exp, (iter, [])) ->
      let exp, rename = rename_exp binds_multi rename exp in
      let exp = IterE (exp, (iter, [])) $$ (at, note) in
      (exp, rename)
  | CastE (exp, typ) ->
      let exp, rename = rename_exp binds_multi rename exp in
      let exp = CastE (exp, typ) $$ (at, note) in
      (exp, rename)

and rename_exps (binds_multi : IdSet.t) (rename : REnv.t) (exps : exp list) :
    exp list * REnv.t =
  List.fold_left
    (fun (exps, rename) exp ->
      let exp, rename = rename_exp binds_multi rename exp in
      (exps @ [ exp ], rename))
    ([], rename) exps

(* Paths *)

and rename_path (binds_multi : IdSet.t) (rename : REnv.t) (path : path) :
    path * REnv.t =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> (path, rename)
  | IdxP (path, exp) ->
      let path, rename = rename_path binds_multi rename path in
      let exp, rename = rename_exp binds_multi rename exp in
      let path = IdxP (path, exp) $$ (at, note) in
      (path, rename)
  | SliceP (path, exp_l, exp_h) ->
      let path, rename = rename_path binds_multi rename path in
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_h, rename = rename_exp binds_multi rename exp_h in
      let path = SliceP (path, exp_l, exp_h) $$ (at, note) in
      (path, rename)
  | DotP (path, atom) ->
      let path, rename = rename_path binds_multi rename path in
      let path = DotP (path, atom) $$ (at, note) in
      (path, rename)

(* Arguments *)

and rename_arg (binds_multi : IdSet.t) (rename : REnv.t) (arg : arg) :
    arg * REnv.t =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp, rename = rename_exp binds_multi rename exp in
      let arg = ExpA exp $ at in
      (arg, rename)
  | DefA _ -> (arg, rename)

and rename_args (binds_multi : IdSet.t) (rename : REnv.t) (args : arg list) :
    arg list * REnv.t =
  List.fold_left
    (fun (args, rename) arg ->
      let arg, rename = rename_arg binds_multi rename arg in
      (args @ [ arg ], rename))
    ([], rename) args

(* Premise *)

let rec rename_prem (binds_multi : IdSet.t) (rename : REnv.t) (prem : prem) :
    prem * REnv.t =
  let at = prem.at in
  match prem.it with
  | RulePr (id, notexp) ->
      let mixop, exps = notexp in
      let exps, rename = rename_exps binds_multi rename exps in
      let notexp = (mixop, exps) in
      let prem = RulePr (id, notexp) $ at in
      (prem, rename)
  | IfPr exp ->
      let exp, rename = rename_exp binds_multi rename exp in
      let prem = IfPr exp $ at in
      (prem, rename)
  | ElsePr -> (prem, rename)
  | LetPr (exp_l, exp_r) ->
      let exp_l, rename = rename_exp binds_multi rename exp_l in
      let exp_r, rename = rename_exp binds_multi rename exp_r in
      let prem = LetPr (exp_l, exp_r) $ at in
      (prem, rename)
  | IterPr (_, (_, (_ :: _ as binds))) ->
      error at
        ("iterated premise should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var binds))
  | IterPr (prem, (iter, [])) ->
      let prem, rename = rename_prem binds_multi rename prem in
      let prem = IterPr (prem, (iter, [])) $ at in
      (prem, rename)

(* Binding and renaming altogether *)

let bind (ctx : Ctx.t) (binder : Ctx.t -> 'a -> ('a * t) attempt)
    (renamer : IdSet.t -> REnv.t -> 'a -> 'a * REnv.t) (construct : 'a) :
    ('a * VEnv.t * prem list) attempt =
  (* Identify binding identifiers *)
  let* construct, binds = binder ctx construct in
  (* Rename multiple binds *)
  let binds_multi =
    binds |> BEnv.bindings
    |> List.filter_map (fun (id, bind) ->
           match bind with Bind.Multi _ -> Some id | Bind.Single _ -> None)
    |> IdSet.of_list
  in
  let construct, renamer = renamer binds_multi REnv.empty construct in
  (* Update binding identifiers, taking renamed identifiers into account *)
  let venv =
    binds |> BEnv.map Bind.get_dim
    |> REnv.fold
         (fun id renames venv ->
           let ids_rename = IdSet.elements renames in
           let iters = BEnv.find id binds |> Bind.get_dim in
           List.fold_left
             (fun venv id_rename -> VEnv.add id_rename iters venv)
             venv ids_rename)
         renamer
  in
  (* Generate sideconditions *)
  let sideconditions =
    REnv.fold
      (fun id renames sideconditions ->
        let typ = BEnv.find id binds |> Bind.get_typ in
        let id_rename, ids_rename =
          renames |> IdSet.elements |> fun ids -> (List.hd ids, List.tl ids)
        in
        let exp =
          let exp =
            let exp_l = VarE id $$ (id.at, typ) in
            let exp_r = VarE id_rename $$ (id_rename.at, typ) in
            CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT)
          in
          List.fold_left
            (fun exp_l id_rename ->
              let exp_r =
                let exp_l = VarE id $$ (id.at, typ) in
                let exp_r = VarE id_rename $$ (id_rename.at, typ) in
                CmpE (`EqOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT)
              in
              BinE (`AndOp, `BoolT, exp_l, exp_r) $$ (no_region, BoolT))
            exp ids_rename
        in
        let sidecondition = IfPr exp $ no_region in
        let iters = BEnv.find id binds |> Bind.get_dim in
        let sidecondition =
          List.fold_left
            (fun sidecondition iter ->
              IterPr (sidecondition, (iter, [])) $ no_region)
            sidecondition iters
        in
        sideconditions @ [ sidecondition ])
      renamer []
  in
  Ok (construct, venv, sideconditions)
