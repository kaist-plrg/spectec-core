open Domain.Lib
open Il.Ast
open Runtime_static
open Error
open Envs
open Util.Source

(* Dimension analysis :

   Annotate iteration constructs with what variables are iterated over
    - Check that iteration is non-empty
    - Check that the dimension of binding variables are not ambiguous,
      i.e., guided by at least one bound iteration variable
    - Check that the dimension of binding variables is coherent *)

(* Constructors *)

let empty = VEnv.empty
let singleton id typ = VEnv.add id (typ, []) VEnv.empty

let union (occurs_a : VEnv.t) (occurs_b : VEnv.t) : VEnv.t =
  VEnv.union
    (fun _ (typ_a, iters_a) (typ_b, iters_b) ->
      (* (TODO) Also check that types are equivalent *)
      if List.length iters_a < List.length iters_b then Some (typ_a, iters_a)
      else Some (typ_b, iters_b))
    occurs_a occurs_b

let collect_itervars (bounds : VEnv.t) (occurs : VEnv.t) (iter : iter) :
    (id * typ * iter list) list =
  occurs |> VEnv.bindings
  |> List.filter_map (fun (id, typ) ->
         let typ_expect = VEnv.find id bounds in
         if Typ.sub (Typ.add_iter iter typ) typ_expect then
           let typ, iters = typ in
           Some (id, typ, iters)
         else None)

(* Expression *)

let rec annotate_exp (bounds : VEnv.t) (exp : exp) : VEnv.t * exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> (empty, exp)
  | VarE id ->
      if VEnv.mem id bounds then (singleton id (note $ at), exp)
      else error exp.at ("free identifier: " ^ Id.to_string id)
  | UnE (op, optyp, exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = UnE (op, optyp, exp) $$ (at, note) in
      (occurs, exp)
  | BinE (op, bintyp, exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = BinE (op, bintyp, exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | CmpE (op, cmptyp, exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = CmpE (op, cmptyp, exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | UpCastE (typ, exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = UpCastE (typ, exp) $$ (at, note) in
      (occurs, exp)
  | DownCastE (typ, exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = DownCastE (typ, exp) $$ (at, note) in
      (occurs, exp)
  | MatchE (exp, pattern) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = MatchE (exp, pattern) $$ (at, note) in
      (occurs, exp)
  | SubE (exp, typ) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = SubE (exp, typ) $$ (at, note) in
      (occurs, exp)
  | TupleE exps ->
      let occurs, exps = annotate_exps bounds exps in
      let exp = TupleE exps $$ (at, note) in
      (occurs, exp)
  | CaseE notexp ->
      let mixop, exps = notexp in
      let occurs, exps = annotate_exps bounds exps in
      let notexp = (mixop, exps) in
      let exp = CaseE notexp $$ (at, note) in
      (occurs, exp)
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let occurs, exps = annotate_exps bounds exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      (occurs, exp)
  | OptE (Some exp) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp_opt = Some exp in
      let exp = OptE exp_opt $$ (at, note) in
      (occurs, exp)
  | OptE None -> (empty, exp)
  | ListE exps ->
      let occurs, exps = annotate_exps bounds exps in
      let exp = ListE exps $$ (at, note) in
      (occurs, exp)
  | ConsE (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = ConsE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | CatE (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = CatE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | MemE (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let exp = MemE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      (occurs, exp)
  | LenE exp ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = LenE exp $$ (at, note) in
      (occurs, exp)
  | DotE (exp, atom) ->
      let occurs, exp = annotate_exp bounds exp in
      let exp = DotE (exp, atom) $$ (at, note) in
      (occurs, exp)
  | IdxE (exp_b, exp_i) ->
      let occurs_b, exp_b = annotate_exp bounds exp_b in
      let occurs_i, exp_i = annotate_exp bounds exp_i in
      let exp = IdxE (exp_b, exp_i) $$ (at, note) in
      let occurs = union occurs_b occurs_i in
      (occurs, exp)
  | SliceE (exp_b, exp_l, exp_h) ->
      let occurs_b, exp_b = annotate_exp bounds exp_b in
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_h, exp_h = annotate_exp bounds exp_h in
      let exp = SliceE (exp_b, exp_l, exp_h) $$ (at, note) in
      let occurs = union (union occurs_b occurs_l) occurs_h in
      (occurs, exp)
  | UpdE (exp_b, path, exp_f) ->
      let occurs_b, exp_b = annotate_exp bounds exp_b in
      let occurs_f, exp_f = annotate_exp bounds exp_f in
      let occurs_p, path = annotate_path bounds path in
      let exp = UpdE (exp_b, path, exp_f) $$ (at, note) in
      let occurs = union (union occurs_b occurs_f) occurs_p in
      (occurs, exp)
  | CallE (id, targs, args) ->
      let occurs, args = annotate_args bounds args in
      let exp = CallE (id, targs, args) $$ (at, note) in
      (occurs, exp)
  | HoldE (id, notexp) ->
      let mixop, exps = notexp in
      let occurs, exps = annotate_exps bounds exps in
      let notexp = (mixop, exps) in
      let exp = HoldE (id, notexp) $$ (at, note) in
      (occurs, exp)
  | IterE (_, ((_, _ :: _) as iterexp)) ->
      error exp.at
        (Format.asprintf
           "iterated expression should initially have no annotations, but got \
            %s"
           (Il.Print.string_of_iterexp iterexp))
  | IterE (exp, (iter, [])) -> (
      let occurs, exp = annotate_exp bounds exp in
      let itervars = collect_itervars bounds occurs iter in
      match itervars with
      | [] -> error at "empty iteration"
      | _ ->
          let exp = IterE (exp, (iter, itervars)) $$ (at, note) in
          let occurs =
            List.fold_left
              (fun occurs (id, typ, iters) ->
                VEnv.add id (typ, iters @ [ iter ]) occurs)
              occurs itervars
          in
          (occurs, exp))

and annotate_exps (bounds : VEnv.t) (exps : exp list) : VEnv.t * exp list =
  match exps with
  | [] -> (empty, [])
  | exp :: exps ->
      let occurs_h, exp_h = annotate_exp bounds exp in
      let occurs_t, exps_t = annotate_exps bounds exps in
      let exps = exp_h :: exps_t in
      let occurs = union occurs_h occurs_t in
      (occurs, exps)

(* Path *)

and annotate_path (bounds : VEnv.t) (path : path) : VEnv.t * path =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> (empty, path)
  | IdxP (path, exp) ->
      let occurs_p, path = annotate_path bounds path in
      let occurs_e, exp = annotate_exp bounds exp in
      let path = IdxP (path, exp) $$ (at, note) in
      let occurs = union occurs_p occurs_e in
      (occurs, path)
  | SliceP (path, exp_l, exp_h) ->
      let occurs_p, path = annotate_path bounds path in
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_h, exp_h = annotate_exp bounds exp_h in
      let path = SliceP (path, exp_l, exp_h) $$ (at, note) in
      let occurs = union (union occurs_p occurs_l) occurs_h in
      (occurs, path)
  | DotP (path, atom) ->
      let occurs, path = annotate_path bounds path in
      let path = DotP (path, atom) $$ (at, note) in
      (occurs, path)

(* Argument *)

and annotate_arg (bounds : VEnv.t) (arg : arg) : VEnv.t * arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let occurs, exp = annotate_exp bounds exp in
      let arg = ExpA exp $ at in
      (occurs, arg)
  | DefA _ -> (empty, arg)

and annotate_args (bounds : VEnv.t) (args : arg list) : VEnv.t * arg list =
  match args with
  | [] -> (empty, [])
  | arg :: args ->
      let occurs_h, arg_h = annotate_arg bounds arg in
      let occurs_t, args_t = annotate_args bounds args in
      let args = arg_h :: args_t in
      let occurs = union occurs_h occurs_t in
      (occurs, args)

(* Premise *)

and annotate_prem (binds : VEnv.t) (bounds : VEnv.t) (prem : prem) :
    VEnv.t * prem =
  let at = prem.at in
  match prem.it with
  | RulePr (id, notexp) ->
      let mixop, exps = notexp in
      let occurs, exps = annotate_exps bounds exps in
      let notexp = (mixop, exps) in
      let prem = RulePr (id, notexp) $ at in
      (occurs, prem)
  | IfPr exp ->
      let occurs, exp = annotate_exp bounds exp in
      let prem = IfPr exp $ at in
      (occurs, prem)
  | ElsePr -> (empty, prem)
  | LetPr (exp_l, exp_r) ->
      let occurs_l, exp_l = annotate_exp bounds exp_l in
      let occurs_r, exp_r = annotate_exp bounds exp_r in
      let prem = LetPr (exp_l, exp_r) $ at in
      let occurs = union occurs_l occurs_r in
      (occurs, prem)
  | IterPr (_, (_, _ :: _)) ->
      error at "iterated premise should initially have no annotations"
  | IterPr (prem, (iter, [])) -> (
      let occurs, prem = annotate_prem binds bounds prem in
      let itervars = collect_itervars bounds occurs iter in
      match itervars with
      | [] -> error at "empty iteration"
      | _
        when List.for_all
               (fun (id, typ, iters) ->
                 match VEnv.find_opt id binds with
                 | Some (typ_bind, iters_bind) ->
                     Typ.sub (typ.it, iters) (typ_bind, iters_bind)
                 | None -> false)
               itervars ->
          error at
            ("cannot determine dimension of binding identifier(s) only: "
            ^ String.concat ", " (List.map Il.Print.string_of_var itervars)
            ^ " "
            ^ Il.Print.string_of_prem prem)
      | _ ->
          let prem = IterPr (prem, (iter, itervars)) $ at in
          let occurs =
            List.fold_left
              (fun occurs (id, typ, iters) ->
                VEnv.add id (typ, iters @ [ iter ]) occurs)
              occurs itervars
          in
          (occurs, prem))

and annotate_prems (binds : VEnv.t) (bounds : VEnv.t) (prems : prem list) :
    VEnv.t * prem list =
  match prems with
  | [] -> (empty, [])
  | prem :: prems ->
      let occurs_h, prem_h = annotate_prem binds bounds prem in
      let occurs_t, prems_t = annotate_prems binds bounds prems in
      let prems = prem_h :: prems_t in
      let occurs = union occurs_h occurs_t in
      (occurs, prems)

(* Analysis *)

let analyze (annotate : VEnv.t -> 'a -> VEnv.t * 'a) (bounds : VEnv.t)
    (construct : 'a) : 'a =
  let occurs, construct = annotate bounds construct in
  VEnv.iter
    (fun id typ ->
      let typ_expect = VEnv.find id bounds in
      if not (Typ.equiv typ typ_expect) then
        error id.at
          (Format.asprintf
             "mismatched iteration dimensions for identifier `%s`: expected \
              %s, but got %s"
             (Id.to_string id) (Typ.to_string typ_expect) (Typ.to_string typ)))
    occurs;
  construct

let analyze_exp (bounds : VEnv.t) (exp : exp) : exp =
  analyze annotate_exp bounds exp

let analyze_exps (bounds : VEnv.t) (exps : exp list) : exp list =
  analyze annotate_exps bounds exps

let analyze_args (bounds : VEnv.t) (args : arg list) : arg list =
  analyze annotate_args bounds args

let analyze_prem (binds : VEnv.t) (bounds : VEnv.t) (prem : prem) : prem =
  analyze (annotate_prem binds) bounds prem

let analyze_prems (binds : VEnv.t) (bounds : VEnv.t) (prems : prem list) :
    prem list =
  analyze (annotate_prems binds) bounds prems

let analyze_sideconditions (bounds : VEnv.t) (sideconditions : prem list) :
    prem list =
  analyze (annotate_prems VEnv.empty) bounds sideconditions
