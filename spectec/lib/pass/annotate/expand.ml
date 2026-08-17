(* Lifts each call nested inside a larger expression into a named binding that
   precedes it, so a rendered prose step names one operation instead of several.

   On one instruction, the loop is:

   1. Find a call to lift. The [Transform] walk descends and [rewrite_call]
      takes the first call nested below the instruction's own outermost call,
      which is left alone since naming it would only rename it.

   2. [rewrite_call] replaces that call with a fresh variable and starts a
      record:
        exp_orig = the call removed
        var_base = the fresh variable put in its place
        iterexps = the iterations crossed so far (none yet)

   3. As the walk returns back out, [Transform.enter_iter] logs each iteration
      crossed. When an [IterE] ranges over a variable the lifted call read, the
      call leaves that iteration, so [enter_iter] appends the iteration to
      [iterexps] and makes the outer [IterE] range over [var_base] instead of
      the original variable.

   4. [replace_call_exp] builds the let from [var_base] and [iterexps]: the let
      binds [var_base] at one iteration level per entry in [iterexps], takes
      [exp_orig] as its right-hand side, and sits before the instruction.

   5. [lift_all] repeats the loop until no liftable call remains.

   For example [unions_set( free_type(typeIR)* )] has [free_type(typeIR)] nested
   under one iteration, and becomes
        Let bound* be free_type(typeIR)   (for all typeIR)
        ... unions_set( bound* ) ... *)

open Common.Domain
open Common.Source
open Lang
open Lang.Sl
module Mixfix = Il.Mixfix
module VarSet = Vars.VarSet
module RelMap = Map.Make (Id)

let ( let* ) = Option.bind

module Transform = struct
  open Lang.Il

  let ( ++ ) = VarSet.union

  let first_some (attempts : (unit -> 'a option) list) : 'a option =
    List.find_map (fun attempt -> attempt ()) attempts

  (* Result of lifting one call. [free_lifted] and [free_rest] are used only by
     [enter_iter], never by the caller: the free variables of the lifted call
     and of the untouched siblings. Their difference decides which iteration
     variables an enclosing [IterE] must still carry. *)
  type iter_state = {
    exp_orig : exp;
    var_base : var;
    iterexps : iterexp list;
    ids_used : IdSet.t;
    free_lifted : VarSet.t;
    free_rest : VarSet.t;
  }

  let record_untouched (vars : VarSet.t) (res : ('a * iter_state) option) :
      ('a * iter_state) option =
    Option.map
      (fun (node, state) ->
        (node, { state with free_rest = state.free_rest ++ vars }))
      res

  (* Rewrite the first node that accepts, recording the rest as read. *)
  let transform_list (f_transform : 'a -> ('a * iter_state) option)
      (f_free : 'a -> VarSet.t) (nodes : 'a list) :
      ('a list * iter_state) option =
    let step (res, vars_untouched, nodes_rev) node =
      match res with
      | Some _ -> (res, vars_untouched ++ f_free node, node :: nodes_rev)
      | None -> (
          match f_transform node with
          | Some (node, _) as res -> (res, vars_untouched, node :: nodes_rev)
          | None -> (None, vars_untouched ++ f_free node, node :: nodes_rev))
    in
    let res, vars_untouched, nodes_rev =
      List.fold_left step (None, VarSet.empty, []) nodes
    in
    let* _, state = res in
    record_untouched vars_untouched (Some (List.rev nodes_rev, state))

  (* Try the root of [exp] before its children, taking the first rewrite that
     accepts. [f_update_acc] refreshes the accumulator [acc] at each step, so a
     rewrite can decide by position as well as by shape. *)
  let transform_first_with_iters
      (f_transform : 'acc -> exp -> (exp * iter_state) option)
      (f_update_acc : 'acc -> exp -> 'acc) (acc : 'acc) (exp : exp) :
      (exp * iter_state) option =
    let rec transform_exp acc (exp : exp) : (exp * iter_state) option =
      let acc = f_update_acc acc exp in
      let { it; at; note; _ } = exp in
      let rebuild it' state = (it' $$ (at, note), state) in
      let try_children () =
        match it with
        | BoolE _ | NumE _ | TextE _ | VarE _ | OptE None -> None
        | UnE (unop, optyp, exp) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (UnE (unop, optyp, exp)) state)
        | BinE (binop, optyp, exp_l, exp_r) -> (
            match transform_exps acc [ exp_l; exp_r ] with
            | Some ([ exp_l; exp_r ], state) ->
                Some (rebuild (BinE (binop, optyp, exp_l, exp_r)) state)
            | _ -> None)
        | CmpE (cmpop, optyp, exp_l, exp_r) -> (
            match transform_exps acc [ exp_l; exp_r ] with
            | Some ([ exp_l; exp_r ], state) ->
                Some (rebuild (CmpE (cmpop, optyp, exp_l, exp_r)) state)
            | _ -> None)
        | UpCastE (typ, exp) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (UpCastE (typ, exp)) state)
        | DownCastE (typ, exp) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (DownCastE (typ, exp)) state)
        | SubE (exp, typ) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (SubE (exp, typ)) state)
        | MatchE (exp, pattern) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (MatchE (exp, pattern)) state)
        | TupleE exps ->
            let* exps, state = transform_exps acc exps in
            Some (rebuild (TupleE exps) state)
        | CaseE notexp ->
            let mixop, exps = Mixfix.split notexp in
            let* exps, state = transform_exps acc exps in
            Some (rebuild (CaseE (Mixfix.fill mixop exps)) state)
        | StrE expfields ->
            let atoms, exps = List.split expfields in
            let* exps, state = transform_exps acc exps in
            Some (rebuild (StrE (List.combine atoms exps)) state)
        | OptE (Some exp) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (OptE (Some exp)) state)
        | ListE exps ->
            let* exps, state = transform_exps acc exps in
            Some (rebuild (ListE exps) state)
        | ConsE (exp_h, exp_t) -> (
            match transform_exps acc [ exp_h; exp_t ] with
            | Some ([ exp_h; exp_t ], state) ->
                Some (rebuild (ConsE (exp_h, exp_t)) state)
            | _ -> None)
        | CatE (exp_l, exp_r) -> (
            match transform_exps acc [ exp_l; exp_r ] with
            | Some ([ exp_l; exp_r ], state) ->
                Some (rebuild (CatE (exp_l, exp_r)) state)
            | _ -> None)
        | MemE (exp_e, exp_s) -> (
            match transform_exps acc [ exp_e; exp_s ] with
            | Some ([ exp_e; exp_s ], state) ->
                Some (rebuild (MemE (exp_e, exp_s)) state)
            | _ -> None)
        | LenE exp ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (LenE exp) state)
        | DotE (exp, atom) ->
            let* exp, state = transform_exp acc exp in
            Some (rebuild (DotE (exp, atom)) state)
        | IdxE (exp_b, exp_i) -> (
            match transform_exps acc [ exp_b; exp_i ] with
            | Some ([ exp_b; exp_i ], state) ->
                Some (rebuild (IdxE (exp_b, exp_i)) state)
            | _ -> None)
        | SliceE (exp_b, exp_l, exp_h) -> (
            match transform_exps acc [ exp_b; exp_l; exp_h ] with
            | Some ([ exp_b; exp_l; exp_h ], state) ->
                Some (rebuild (SliceE (exp_b, exp_l, exp_h)) state)
            | _ -> None)
        | UpdE (exp_b, path, exp_f) ->
            first_some
              [
                (fun () ->
                  let* exp_b, state =
                    transform_exp acc exp_b
                    |> record_untouched
                         (Vars.free_path path ++ Vars.free_exp exp_f)
                  in
                  Some (rebuild (UpdE (exp_b, path, exp_f)) state));
                (fun () ->
                  let* path, state =
                    transform_path acc path
                    |> record_untouched
                         (Vars.free_exp exp_b ++ Vars.free_exp exp_f)
                  in
                  Some (rebuild (UpdE (exp_b, path, exp_f)) state));
                (fun () ->
                  let* exp_f, state =
                    transform_exp acc exp_f
                    |> record_untouched
                         (Vars.free_exp exp_b ++ Vars.free_path path)
                  in
                  Some (rebuild (UpdE (exp_b, path, exp_f)) state));
              ]
        | CallE (id, targs, args) ->
            let* args, state = transform_args acc args in
            Some (rebuild (CallE (id, targs, args)) state)
        | IterE (exp, (iter, vars)) ->
            let* exp, state = transform_exp acc exp in
            let state, vars = enter_iter state iter vars in
            Some (rebuild (IterE (exp, (iter, vars))) state)
      in
      first_some [ (fun () -> f_transform acc exp); try_children ]
    (* Record that the rewrite sits one iteration deeper, and report the
       variables the iteration still has to carry. One iteration can bind
       several variables at once (as in [f(a, b)*]), so the whole group gains a
       single level rather than one level per variable. *)
    and enter_iter (state : iter_state) (iter : iter) (vars : var list) :
        iter_state * var list =
      let vars_lifted =
        VarSet.filter
          (fun var -> List.exists (Vars.Var.equal var) vars)
          state.free_lifted
      in
      if VarSet.is_empty vars_lifted then (state, vars)
      else
        let is_lifted var = VarSet.mem var vars_lifted in
        let deepen var = { var with iters = var.iters @ [ iter ] } in
        (* When only the rewritten expression read a variable, the new variable
           replaces it. One still read by the untouched siblings stays
           alongside it. *)
        let vars_kept =
          List.filter
            (fun var -> (not (is_lifted var)) || VarSet.mem var state.free_rest)
            vars
        in
        (* the new variable at the dimensions accumulated below this iteration *)
        let var_here =
          {
            state.var_base with
            iters = state.var_base.iters @ List.map fst state.iterexps;
          }
        in
        let state =
          {
            state with
            free_lifted =
              VarSet.map
                (fun var -> if is_lifted var then deepen var else var)
                state.free_lifted;
            iterexps = state.iterexps @ [ (iter, List.filter is_lifted vars) ];
          }
        in
        (state, var_here :: vars_kept)
    and transform_exps acc (exps : exp list) : (exp list * iter_state) option =
      transform_list (transform_exp acc) Vars.free_exp exps
    and transform_arg acc (arg : arg) : (arg * iter_state) option =
      let { it; at; _ } = arg in
      match it with
      | ExpA exp ->
          let* exp, state = transform_exp acc exp in
          Some (ExpA exp $ at, state)
      | DefA _ -> None
    and transform_args acc (args : arg list) : (arg list * iter_state) option =
      transform_list (transform_arg acc) Vars.free_arg args
    and transform_path acc (path : path) : (path * iter_state) option =
      let { it; at; note; _ } = path in
      let rebuild it' state = (it' $$ (at, note), state) in
      match it with
      | RootP -> None
      | IdxP (path, exp_i) ->
          first_some
            [
              (fun () ->
                let* path, state =
                  transform_path acc path
                  |> record_untouched (Vars.free_exp exp_i)
                in
                Some (rebuild (IdxP (path, exp_i)) state));
              (fun () ->
                let* exp_i, state =
                  transform_exp acc exp_i
                  |> record_untouched (Vars.free_path path)
                in
                Some (rebuild (IdxP (path, exp_i)) state));
            ]
      | SliceP (path, exp_l, exp_h) ->
          first_some
            [
              (fun () ->
                let* path, state =
                  transform_path acc path
                  |> record_untouched
                       (Vars.free_exp exp_l ++ Vars.free_exp exp_h)
                in
                Some (rebuild (SliceP (path, exp_l, exp_h)) state));
              (fun () ->
                let* exp_l, state =
                  transform_exp acc exp_l
                  |> record_untouched
                       (Vars.free_path path ++ Vars.free_exp exp_h)
                in
                Some (rebuild (SliceP (path, exp_l, exp_h)) state));
              (fun () ->
                let* exp_h, state =
                  transform_exp acc exp_h
                  |> record_untouched
                       (Vars.free_path path ++ Vars.free_exp exp_l)
                in
                Some (rebuild (SliceP (path, exp_l, exp_h)) state));
            ]
      | DotP (path, atom) ->
          let* path, state = transform_path acc path in
          Some (rebuild (DotP (path, atom)) state)
    in
    transform_exp acc exp
end

(* Whether a call at the expression being visited may be lifted into a let.

   An instruction's own outermost call stays where it is, because hoisting it
   would only rename it. [Root] marks the position before any expression has
   been visited, [Kept] the call found there, and [Liftable] everything nested
   deeper. *)
type lift = Root | Kept | Liftable

let advance (lift : lift) (exp : exp) : lift =
  match exp.it with
  | Il.CallE _ -> ( match lift with Root -> Kept | _ -> Liftable)
  | Il.IterE _ -> lift
  | _ -> Liftable

(* Variables bound by the iterations an instruction runs under. A call reading
   one of them cannot be lifted above the instruction. *)
let iter_locals (iterexps : iterexp list) : IdSet.t =
  List.fold_left
    (fun ids (_, vars) ->
      List.fold_left
        (fun ids ({ Il.varid; _ } : var) -> IdSet.add varid ids)
        ids vars)
    IdSet.empty iterexps

let reads_iter_local (iter_locals : IdSet.t) (args : arg list) : bool =
  Vars.free_args args
  |> VarSet.exists (fun ({ Il.varid; _ } : var) -> IdSet.mem varid iter_locals)

let rewrite_call (iter_locals : IdSet.t) (ids_used : IdSet.t) (lift : lift)
    (exp : exp) : (exp * Transform.iter_state) option =
  match (lift, exp.it) with
  | Liftable, Il.CallE (_, _, args)
    when args <> [] && not (reads_iter_local iter_locals args) ->
      let var_base = Il.Fresh.fresh_var_from_exp ids_used exp in
      Some
        ( Il.Var.as_exp var_base,
          {
            Transform.exp_orig = exp;
            var_base;
            iterexps = [];
            ids_used = IdSet.add var_base.varid ids_used;
            free_lifted = Vars.free_args args;
            free_rest = VarSet.empty;
          } )
  | _ -> None

(* Lift the first liftable call out of [exp] into a preceding let, replacing it
   with that let's variable. *)
let replace_call_exp ~(lift : lift) ?(iter_locals = IdSet.empty)
    (ids_used : IdSet.t) (exp : exp) : ((instr -> instr) * exp * IdSet.t) option
    =
  let* exp, state =
    Transform.transform_first_with_iters
      (rewrite_call iter_locals ids_used)
      advance lift exp
  in
  let { Transform.var_base; iterexps; exp_orig; ids_used; _ } = state in
  (* Wrap one let-iteration per iteration the call was lifted out of, starting
     from the callee's own dimensions and binding one more at each level. *)
  let iterexps_let, _ =
    List.fold_left
      (fun (iterexps_let, var_bind) (iter, vars_iterated) ->
        let iterexp = (iter, vars_iterated @ [ var_bind ]) in
        let var_bind =
          { var_bind with Il.iters = var_bind.Il.iters @ [ iter ] }
        in
        (iterexps_let @ [ iterexp ], var_bind))
      ([], var_base) iterexps
  in
  let wrap_in_let body =
    LetI (Il.Var.as_exp var_base, exp_orig, iterexps_let, [ body ]) $ no_region
  in
  Some (wrap_in_let, exp, ids_used)

let rec replace_call_exps_first ~(lift : lift) ?(iter_locals = IdSet.empty)
    (ids_used : IdSet.t) (exps : exp list) :
    ((instr -> instr) * exp list * IdSet.t) option =
  match exps with
  | [] -> None
  | exp_h :: exps_t -> (
      match replace_call_exp ~lift ~iter_locals ids_used exp_h with
      | Some (wrap_in_let, exp_h, ids_used) ->
          Some (wrap_in_let, exp_h :: exps_t, ids_used)
      | None ->
          replace_call_exps_first ~lift ~iter_locals ids_used exps_t
          |> Option.map (fun (wrap_in_let, exps_t, ids_used) ->
                 (wrap_in_let, exp_h :: exps_t, ids_used)))

(* One lifting step on a single instruction, leaving its sub-blocks alone. *)
let expand_nested_calls (relmodes : (exp, unit) Il.Mode.t RelMap.t)
    (ids_used : IdSet.t) (instr : instr) :
    ((instr -> instr) * instr * IdSet.t) option =
  let rebuild it = it $ instr.at in
  match instr.it with
  | LetI (exp_l, exp_r, iterexps, block) ->
      let* wrap_in_let, exp_r, ids_used =
        replace_call_exp ~lift:Root ~iter_locals:(iter_locals iterexps) ids_used
          exp_r
      in
      Some
        (wrap_in_let, rebuild (LetI (exp_l, exp_r, iterexps, block)), ids_used)
  | RelI { call; iterexps; block } ->
      (* Only the relation's inputs are ordinary expressions. Its outputs are
         positions the call binds. *)
      let* mode = RelMap.find_opt call.relid relmodes in
      let mixop, exps = Mixfix.split call.notexp in
      let exps_input, exps_output = Il.Mode.partition mode exps in
      let* wrap_in_let, exps_input, ids_used =
        replace_call_exps_first ~lift:Kept ~iter_locals:(iter_locals iterexps)
          ids_used exps_input
      in
      let exps = Il.Mode.interleave mode ~ins:exps_input ~outs:exps_output in
      let call = { call with notexp = Mixfix.fill mixop exps } in
      Some (wrap_in_let, rebuild (RelI { call; iterexps; block }), ids_used)
  | RelAssertI { call; expect; iterexps; block; phantom } ->
      (* An assertion binds nothing, so every position is an ordinary expression. *)
      let mixop, exps = Mixfix.split call.notexp in
      let* wrap_in_let, exps, ids_used =
        replace_call_exps_first ~lift:Kept ~iter_locals:(iter_locals iterexps)
          ids_used exps
      in
      let call = { call with notexp = Mixfix.fill mixop exps } in
      Some
        ( wrap_in_let,
          rebuild (RelAssertI { call; expect; iterexps; block; phantom }),
          ids_used )
  | ResultI exps ->
      let* wrap_in_let, exps, ids_used =
        replace_call_exps_first ~lift:Root ids_used exps
      in
      Some (wrap_in_let, rebuild (ResultI exps), ids_used)
  | ReturnI exp ->
      let* wrap_in_let, exp, ids_used =
        replace_call_exp ~lift:Root ids_used exp
      in
      Some (wrap_in_let, rebuild (ReturnI exp), ids_used)
  | _ -> None

let rec expand_block relmodes (ids_used : IdSet.t) (block : block) :
    IdSet.t * block =
  List.fold_left_map (expand_instr relmodes) ids_used block

and expand_instr relmodes (ids_used : IdSet.t) (instr : instr) : IdSet.t * instr
    =
  let ids_used, instr = expand_sub_blocks relmodes ids_used instr in
  let rec lift_all ids_used instr =
    match expand_nested_calls relmodes ids_used instr with
    | Some (wrap_in_let, instr, ids_used) ->
        (* The instruction may still hold further calls, and so may the let
           that was just lifted out of it. *)
        let ids_used, instr = lift_all ids_used instr in
        lift_all ids_used (wrap_in_let instr)
    | None -> (ids_used, instr)
  in
  lift_all ids_used instr

and expand_sub_blocks relmodes (ids_used : IdSet.t) (instr : instr) :
    IdSet.t * instr =
  let rebuild it = it $ instr.at in
  match instr.it with
  | RelI { call; iterexps; block } ->
      let ids_used, block = expand_block relmodes ids_used block in
      (ids_used, rebuild (RelI { call; iterexps; block }))
  | RelAssertI { call; expect; iterexps; block; phantom } ->
      let ids_used, block = expand_block relmodes ids_used block in
      (ids_used, rebuild (RelAssertI { call; expect; iterexps; block; phantom }))
  | IfI (exp_cond, iterexps, block, phantom) ->
      let ids_used, block = expand_block relmodes ids_used block in
      (ids_used, rebuild (IfI (exp_cond, iterexps, block, phantom)))
  | CaseI (exp, cases, phantom) ->
      let ids_used, cases =
        List.fold_left_map
          (fun ids_used (guard, block) ->
            let ids_used, block = expand_block relmodes ids_used block in
            (ids_used, (guard, block)))
          ids_used cases
      in
      (ids_used, rebuild (CaseI (exp, cases, phantom)))
  | OtherwiseI instr_inner ->
      let ids_used, instr_inner = expand_instr relmodes ids_used instr_inner in
      (ids_used, rebuild (OtherwiseI instr_inner))
  | LetI (exp_l, exp_r, iterexps, block) ->
      let ids_used, block = expand_block relmodes ids_used block in
      (ids_used, rebuild (LetI (exp_l, exp_r, iterexps, block)))
  | DebugI (exp, instr_inner) ->
      let ids_used, instr_inner = expand_instr relmodes ids_used instr_inner in
      (ids_used, rebuild (DebugI (exp, instr_inner)))
  | ResultI _ | ReturnI _ -> (ids_used, instr)

let expand_def relmodes (def : def) : def =
  let ids_used = Free.free_def def in
  let expand_top block = expand_block relmodes ids_used block |> snd in
  let it =
    match def.it with
    | RelD (id, mode, block, elseblock_opt) ->
        RelD (id, mode, expand_top block, Option.map expand_top elseblock_opt)
    | DecD (id, tparams, args, block, elseblock_opt) ->
        DecD
          ( id,
            tparams,
            args,
            expand_top block,
            Option.map expand_top elseblock_opt )
    | it -> it
  in
  it $ def.at

let collect_rel_modes (spec : spec) : (exp, unit) Il.Mode.t RelMap.t =
  List.fold_left
    (fun relmodes def ->
      match def.it with
      | RelD (id, mode, _, _) -> RelMap.add id mode relmodes
      | _ -> relmodes)
    RelMap.empty spec

let expand_spec (spec : spec) : spec =
  let relmodes = collect_rel_modes spec in
  List.map (expand_def relmodes) spec
