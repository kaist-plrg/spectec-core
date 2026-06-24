type ('i, 'o) arg = In of 'i | Out of 'o
type ('i, 'o) t = ('i, 'o) arg Mixfix.t
type dir = Input | Output

let of_dirs notation dirs =
  let args = Mixfix.args notation in
  let tagged =
    try
      List.map2
        (fun a d -> match d with Input -> In a | Output -> Out a)
        args dirs
    with Invalid_argument _ -> raise (Mixfix.Arity_mismatch "Mode.of_dirs")
  in
  Mixfix.fill (Mixfix.to_mixop notation) tagged

let fill t ~ins ~outs =
  let rec go args ins outs =
    match (args, ins, outs) with
    | [], [], [] -> []
    | In _ :: args', i :: ins', _ -> In i :: go args' ins' outs
    | Out _ :: args', _, o :: outs' -> Out o :: go args' ins outs'
    | _ -> raise (Mixfix.Arity_mismatch "Mode.fill")
  in
  let mixop = Mixfix.to_mixop t in
  let tagged = go (Mixfix.args t) ins outs in
  Mixfix.fill mixop tagged

let inputs t =
  Mixfix.args t |> List.filter_map (function In v -> Some v | Out _ -> None)

let outputs t =
  Mixfix.args t |> List.filter_map (function Out v -> Some v | In _ -> None)

let with_inputs t ins =
  let n_outs = List.length (outputs t) in
  fill t ~ins ~outs:(List.init n_outs (fun _ -> ()))

let notation t = Mixfix.map (function In v | Out v -> v) t

let is_predicate t =
  List.for_all (function In _ -> true | Out _ -> false) (Mixfix.args t)

let partition t values =
  let rec go args values =
    match (args, values) with
    | [], [] -> ([], [])
    | In _ :: args', v :: values' ->
        let ins, outs = go args' values' in
        (v :: ins, outs)
    | Out _ :: args', v :: values' ->
        let ins, outs = go args' values' in
        (ins, v :: outs)
    | _ -> raise (Mixfix.Arity_mismatch "Mode.partition")
  in
  go (Mixfix.args t) values

let interleave t ~ins ~outs =
  let rec go args ins outs =
    match (args, ins, outs) with
    | [], [], [] -> []
    | In _ :: args', i :: ins', _ -> i :: go args' ins' outs
    | Out _ :: args', _, o :: outs' -> o :: go args' ins outs'
    | _ -> raise (Mixfix.Arity_mismatch "Mode.interleave")
  in
  go (Mixfix.args t) ins outs

let render ?(pad_brackets = false) ~string_of_atom ~string_of_in ~string_of_out
    t =
  Mixfix.render ~pad_brackets ~string_of_atom
    ~string_of_arg:(function
      | In v -> string_of_in v | Out v -> string_of_out v)
    t

let render_inputs ~sep ~string_of_arg t =
  String.concat sep (List.map string_of_arg (inputs t))
