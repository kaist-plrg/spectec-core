open Util.Source

(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module IdSet = struct
  include Set.Make (Id)

  let pp fmt s =
    let pp_id fmt id = Format.fprintf fmt "%a" Id.pp id in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_id) (elements s)

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" Id.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let subset eq_v m1 m2 =
    List.for_all
      (fun (k, v1) ->
        match find_opt k m2 with Some v2 -> eq_v v1 v2 | None -> false)
      (bindings m1)

  let eq eq_v m1 m2 = subset eq_v m1 m2 && subset eq_v m2 m1
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap

(* Function identifiers *)

module FId = struct
  type t = name * param list
  and name = string
  and param = string * bool

  let pp_name fmt name = Format.fprintf fmt "%s" name
  let compare = compare
  let pp_param fmt (id, _) = Format.fprintf fmt "%s" id

  let pp fmt (name, params) =
    Format.fprintf fmt "%a(%a)" pp_name name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         pp_param)
      params

  let to_fid (id : El.Ast.id) (params : El.Ast.param list) =
    let params =
      List.map
        (fun param ->
          let id, _, _, value_default, _ = param.it in
          (id.it, Option.is_some value_default))
        params
    in
    (id.it, params)

  let to_names (args : El.Ast.arg list) =
    List.map
      (fun arg ->
        match arg.it with Lang.Ast.NameA (id, _) -> Some id.it | _ -> None)
      args
end

module FIdSet = struct
  include Set.Make (FId)

  let eq = equal

  let pp fmt s =
    let pp_fid fmt fid = Format.fprintf fmt "%a" FId.pp fid in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_fid) (elements s)

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module FIdMap = struct
  include Map.Make (FId)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" FId.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let subset eq_v m1 m2 =
    List.for_all
      (fun (k, v1) ->
        match find_opt k m2 with Some v2 -> eq_v v1 v2 | None -> false)
      (bindings m1)

  let eq eq_v m1 m2 = subset eq_v m1 m2 && subset eq_v m2 m1
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Constructor identifiers *)

module CId = FId
module CIdSet = FIdSet
module CIdMap = FIdMap

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let pp fmt env = IdMap.pp V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "Key not found: %a\n" Id.pp id |> failwith
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let pp fmt env = FIdMap.pp V.pp fmt env

  (* Lookup for matching def site to def site *)
  (* (TODO) This must also impose the restriction that default values must be coherent, if specified *)

  let find_opt (fid : FId.t) fenv = List.assoc_opt fid (bindings fenv)

  let find (fid : FId.t) fenv =
    match find_opt fid fenv with
    | Some func -> func
    | None -> Format.asprintf "Key not found: %a\n" FId.pp fid |> failwith

  (* Lookups for matching call site to def site *)

  (* Overloaded lookup, allowing defaults *)

  let check_named_args args =
    if not (List.for_all Option.is_some args || List.for_all Option.is_none args)
    then (
      Format.printf
        "(check_named_args) Either all or no arguments must specify the \
         parameter name\n";
      assert false)

  (* (6.8.1) Justification

     Following is a summary of the constraints imposed by the parameter directions:

      - If parameters with default values do not appear at the end of the list of parameters,
        invocations that use the default values must use named arguments. *)

  (* (8.20) Method invocations and function calls

     A function call or method invocation can optionally specify for each argument
     the corresponding parameter name.
     It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. *)

  let check_func_name fname fname' = fname = fname'
  let check_arity_more args params = List.length args > List.length params
  let check_arity args params = List.length args = List.length params
  let check_args_named arg_names = arg_names <> []

  let find_match_named func arg_names params =
    let param_names = List.map fst params in
    let param_names = List.sort String.compare param_names in
    if List.for_all2 ( = ) arg_names param_names then Some (func, []) else None

  let find_match_named_default func arg_names params =
    let param_names = List.map fst params in
    let param_names = List.sort String.compare param_names in
    let param_missing_names =
      List.filter
        (fun param_name -> not (List.mem param_name arg_names))
        param_names
    in
    let arg_names =
      arg_names @ param_missing_names |> List.sort String.compare
    in
    find_match_named func arg_names params
    |> Option.map (fun (func, _) -> (func, param_missing_names))

  let find_match_unnamed_default func args params =
    let params_default =
      List.filteri (fun i _ -> i >= List.length args) params
    in
    let params_names, params_default = List.split params_default in
    if List.for_all Fun.id params_default then Some (func, params_names)
    else None

  let find_overloaded_opt (fname, args) fenv =
    check_named_args args;
    let arg_names =
      if List.for_all Option.is_some args then
        List.map Option.get args |> List.sort String.compare
      else []
    in
    let funcs =
      List.filter_map
        (fun ((fname', params), func) ->
          (* Falls into roughly five cases:
             (1) Name mismatch or more args than params
             (2) Arity match
                (a) Named args (b) Unnamed args
             (3) Arity mismatch (maybe due to default param)
                (a) Named args (b) Unnamed args *)
          if not (check_func_name fname fname') then None
          else if check_arity_more args params then None
          else if check_arity args params then
            if check_args_named arg_names then
              find_match_named func arg_names params
            else Some (func, [])
          else if check_args_named arg_names then
            find_match_named_default func arg_names params
          else find_match_unnamed_default func args params)
        (bindings fenv)
    in
    if List.length funcs > 2 then (
      Format.printf
        "(find_overloaded_opt) Cannot resolve overloaded function given %a\n"
        FId.pp_name fname;
      assert false);
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs)

  let find_overloaded (fname, args) fenv =
    match find_overloaded_opt (fname, args) fenv with
    | Some value -> value
    | None ->
        Format.asprintf "Key not found: %a\n" FId.pp_name fname |> failwith

  (* Non-overloaded lookup, allowing defaults *)

  let find_non_overloaded_opt (fname, args) fenv =
    check_named_args args;
    let funcs =
      List.filter (fun ((fname', _), _) -> fname = fname') (bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)

  let find_non_overloaded (fname, args) fenv =
    match find_non_overloaded_opt (fname, args) fenv with
    | Some value -> value
    | None ->
        Format.asprintf "Key not found: %a\n" FId.pp_name fname |> failwith
end

module MakeCIdEnv = MakeFIdEnv

(* Pair functor *)

module MakePair (A : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (B : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = A.t * B.t

  let pp fmt (a, b) = Format.fprintf fmt "(%a, %a)" A.pp a B.pp b
end

(* Triple functor *)

module MakeTriple (A : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (B : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (C : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = A.t * B.t * C.t

  let pp fmt (a, b, c) = Format.fprintf fmt "(%a, %a, %a)" A.pp a B.pp b C.pp c
end
