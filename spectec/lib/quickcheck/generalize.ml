open Il_gen
open Lang.Il

let rec sequence_gen = function
  | [] -> Gen.return []
  | g :: gs ->
      Gen.bind g (fun x -> Gen.map (fun xs -> x :: xs) (sequence_gen gs))

let rec cross_product = function
  | [] -> [ ([], []) ]
  | (i, paths) :: rest ->
      List.concat_map
        (fun (d, s, g) ->
          List.map
            (fun (s_acc, g_acc) -> ((i, (d, s)) :: s_acc, (i, g) :: g_acc))
            (cross_product rest))
        paths

let rec powerset = function
  | [] -> [ [] ]
  | x :: xs ->
      let rest = powerset xs in
      rest @ List.map (fun s -> x :: s) rest

let nonempty_subsets l = List.filter (fun s -> s <> []) (powerset l)

let build_generalizations sub_paths_map combos make_display make_gen =
  List.concat_map
    (fun indices ->
      let selected =
        List.map (fun i -> (i, List.assoc i sub_paths_map)) indices
      in
      List.map
        (fun (s_env, g_env) ->
          (* Min depth across positions: "deep in ALL positions" > "deep in one position only".
         This ensures [nat]+[bool] (min=1) ranks above [nat]+[expr] (min=0). *)
          let min_d =
            List.fold_left (fun acc (_, (d, _)) -> min acc d) max_int s_env
          in
          let gens = List.map (fun i -> List.assoc i g_env) indices in
          let gen =
            Gen.map
              (fun values -> make_gen (List.combine indices values))
              (sequence_gen gens)
          in
          (min_d + 1, make_display s_env, gen))
        (cross_product selected))
    combos

let patch_mixfix lookup fallback vc =
  let _, patched =
    List.fold_left
      (fun (arg_idx, acc) part ->
        match part with
        | Mixfix.Atom a -> (arg_idx, Mixfix.Atom a :: acc)
        | Mixfix.Arg _ ->
            let v =
              match lookup arg_idx with Some v -> v | None -> fallback arg_idx
            in
            (arg_idx + 1, Mixfix.Arg v :: acc))
      (0, []) vc
  in
  List.rev patched

let rec generalize_paths (spec : spec) (v : Value.t) :
    (int * string * Value.t Gen.t) list =
  let open Common.Source in
  let t = v.note.typ in
  let root =
    match gen_of_typ spec (t $ no_region) with
    | gen -> [ (0, "[" ^ Print.string_of_typ (t $ no_region) ^ "]", gen) ]
    | exception (Invalid_argument _ | Failure _) -> []
  in

  let sub_paths =
    match v.it with
    | StructV fields ->
        let sub_paths_map =
          List.mapi (fun i (_, vj) -> (i, generalize_paths spec vj)) fields
        in
        let combos = nonempty_subsets (List.mapi (fun i _ -> i) fields) in
        let make_display s_env =
          let fields' =
            List.mapi
              (fun j (aj, vj) ->
                match List.assoc_opt j s_env with
                | Some (_, sub_str) -> (aj, Value.text sub_str)
                | None -> (aj, vj))
              fields
          in
          Print.string_of_value (Value.make_val t (StructV fields'))
        in
        let make_gen val_env =
          let fields' =
            List.mapi
              (fun j (aj, vj) ->
                match List.assoc_opt j val_env with
                | Some new_v -> (aj, new_v)
                | None -> (aj, vj))
              fields
          in
          Value.make_val t (StructV fields')
        in
        build_generalizations sub_paths_map combos make_display make_gen
    | TupleV vs ->
        let sub_paths_map =
          List.mapi (fun i vi -> (i, generalize_paths spec vi)) vs
        in
        let combos = nonempty_subsets (List.mapi (fun i _ -> i) vs) in
        let make_display s_env =
          let vs' =
            List.mapi
              (fun j vj ->
                match List.assoc_opt j s_env with
                | Some (_, sub_str) -> Value.text sub_str
                | None -> vj)
              vs
          in
          Print.string_of_value (Value.make_val t (TupleV vs'))
        in
        let make_gen val_env =
          let vs' =
            List.mapi
              (fun j vj ->
                match List.assoc_opt j val_env with
                | Some new_v -> new_v
                | None -> vj)
              vs
          in
          Value.make_val t (TupleV vs')
        in
        build_generalizations sub_paths_map combos make_display make_gen
    | CaseV vc ->
        let args = Mixfix.args vc in
        let sub_paths_map =
          List.mapi (fun i vi -> (i, generalize_paths spec vi)) args
        in
        let combos = nonempty_subsets (List.mapi (fun i _ -> i) args) in
        let fallback i = List.nth args i in
        let make_display s_env =
          let display_args =
            patch_mixfix
              (fun i ->
                match List.assoc_opt i s_env with
                | Some (_, s) -> Some (Value.text s)
                | None -> None)
              fallback vc
          in
          Print.string_of_value (Value.make_val t (CaseV display_args))
        in
        let make_gen val_env =
          let patched_args =
            patch_mixfix (fun i -> List.assoc_opt i val_env) fallback vc
          in
          Value.make_val t (CaseV patched_args)
        in
        build_generalizations sub_paths_map combos make_display make_gen
    | _ -> []
  in
  let generalization_score s =
    String.fold_left (fun acc c -> if c = '[' then acc + 1 else acc) 0 s
  in
  root
  @
  (* Primary: score descending (more positions generalized = tried first by generalize_loop).
     Secondary: depth descending so that "deep in all positions" (e.g. [nat]+[bool])
     beats "deep in one, shallow in another" (e.g. [nat]+[expr]) within the same score. *)
  List.stable_sort
    (fun (d1, s1, _) (d2, s2, _) ->
      let c = compare (generalization_score s2) (generalization_score s1) in
      if c <> 0 then c else compare d2 d1)
    sub_paths

let show_env (bindings : (id' * value) list) : string =
  String.concat ", "
    (List.map (fun (id, v) -> id ^ "=" ^ Print.string_of_value v) bindings)

let generalize_env spec (counter_env : (id' * value) list) :
    (string * (id' * value) list Gen.t) list =
  let n = List.length counter_env in
  if n = 0 then []
  else
    let candidates =
      List.concat_map
        (fun (i, (_, v_i)) ->
          let sub_paths = generalize_paths spec v_i in
          List.map
            (fun (_, display, path_gen) ->
              let label =
                String.concat ", "
                  (List.mapi
                     (fun j (id_j, v_j) ->
                       if j = i then id_j ^ "=" ^ display
                       else id_j ^ "=" ^ Print.string_of_value v_j)
                     counter_env)
              in
              let gen' =
                Gen.map
                  (fun new_vi ->
                    List.mapi
                      (fun j (id_j, v_j) ->
                        if j = i then (id_j, new_vi) else (id_j, v_j))
                      counter_env)
                  path_gen
              in
              (label, gen'))
            sub_paths)
        (List.mapi (fun i p -> (i, p)) counter_env)
    in
    candidates
