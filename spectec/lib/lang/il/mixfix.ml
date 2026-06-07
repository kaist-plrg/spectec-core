open Common.Source

type atom = Xl.Atom.t phrase
type 'a mixeme = Arg of 'a | Atom of atom
type 'a t = 'a mixeme list
type mixop = unit t

exception Arity_mismatch of string

(* Comparison *)

let compare_atom (atom_a : atom) (atom_b : atom) =
  Xl.Atom.compare atom_a.it atom_b.it

let compare_mixeme (compare_arg : 'a -> 'b -> int) (a : 'a mixeme)
    (b : 'b mixeme) =
  match (a, b) with
  | Arg arg_a, Arg arg_b -> compare_arg arg_a arg_b
  | Atom atom_a, Atom atom_b -> compare_atom atom_a atom_b
  | Arg _, Atom _ -> -1
  | Atom _, Arg _ -> 1

let compare ~(compare_arg : 'a -> 'b -> int) (mf_a : 'a t) (mf_b : 'b t) : int =
  let rec compare_mixemes mixemes_a mixemes_b =
    match (mixemes_a, mixemes_b) with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | mixeme_a :: rest_a, mixeme_b :: rest_b ->
        let c = compare_mixeme compare_arg mixeme_a mixeme_b in
        if c <> 0 then c else compare_mixemes rest_a rest_b
  in
  compare_mixemes mf_a mf_b

let eq ~(eq_arg : 'a -> 'b -> bool) (mf_a : 'a t) (mf_b : 'b t) : bool =
  compare ~compare_arg:(fun a b -> if eq_arg a b then 0 else -1) mf_a mf_b = 0

let compare_mixop (type a b) (mf_a : a t) (mf_b : b t) : int =
  compare ~compare_arg:(fun (_ : a) (_ : b) -> 0) mf_a mf_b

let eq_mixop (mf_a : 'a t) (mf_b : 'b t) : bool = compare_mixop mf_a mf_b = 0

(* Projections *)

let args (mf : 'a t) : 'a list =
  List.filter_map (function Arg a -> Some a | Atom _ -> None) mf

let atoms (mf : 'a t) : atom list =
  List.filter_map (function Atom a -> Some a | Arg _ -> None) mf

let arity (mf : 'a t) : int =
  List.fold_left (fun n p -> match p with Arg _ -> n + 1 | Atom _ -> n) 0 mf

let to_mixop (mixfix : 'a t) : mixop =
  List.map (function Arg _ -> Arg () | Atom atom -> Atom atom) mixfix

let map (f : 'a -> 'b) : 'a t -> 'b t =
  List.map (function Arg a -> Arg (f a) | Atom atom -> Atom atom)

let map_atoms (f : atom -> atom) : 'a t -> 'a t =
  List.map (function Atom atom -> Atom (f atom) | Arg a -> Arg a)

(* Walks *)

let iter_args (f : 'a -> unit) (mf : 'a t) : unit = List.iter f (args mf)

let fold_args (f : 'acc -> 'a -> 'acc) (acc : 'acc) (mf : 'a t) : 'acc =
  List.fold_left
    (fun acc mixeme -> match mixeme with Arg a -> f acc a | Atom _ -> acc)
    acc mf

let iter_atoms (f : atom -> unit) (mf : 'a t) : unit = List.iter f (atoms mf)

(* Construction / deconstruction *)

let rec fill (mixop : mixop) (args : 'a list) : 'a t =
  match (mixop, args) with
  | [], [] -> []
  | Atom atom :: mixop_rest, args -> Atom atom :: fill mixop_rest args
  | Arg () :: mixop_rest, a :: args_rest -> Arg a :: fill mixop_rest args_rest
  | [], _ :: _ -> raise (Arity_mismatch "Mixfix.fill: too many arguments")
  | Arg () :: _, [] -> raise (Arity_mismatch "Mixfix.fill: too few arguments")

let split (mixfix : 'a t) : mixop * 'a list =
  List.fold_right
    (fun mixeme (mixop, args) ->
      match mixeme with
      | Arg a -> (Arg () :: mixop, a :: args)
      | Atom atom -> (Atom atom :: mixop, args))
    mixfix ([], [])

(* Rendering *)

let is_open_bracket : Xl.Atom.t -> bool = function
  | LParen | LBrack | LBrace | LAngle -> true
  | _ -> false

let is_close_bracket : Xl.Atom.t -> bool = function
  | RParen | RBrack | RBrace | RAngle -> true
  | _ -> false

let opens_bracket = function Atom a -> is_open_bracket a.it | Arg _ -> false
let closes_bracket = function Atom a -> is_close_bracket a.it | Arg _ -> false

let rec render_with ~(pad_brackets : bool) (of_mixeme : 'a mixeme -> string)
    (mixfix : 'a t) : string =
  match mixfix with
  | [] -> ""
  | [ mixeme ] -> of_mixeme mixeme
  | left :: (right :: _ as rest) ->
      let l = of_mixeme left in
      let r = render_with ~pad_brackets of_mixeme rest in
      if l = "" then r
      else if r = "" then l
      else
        let sep =
          if (not pad_brackets) && (opens_bracket left || closes_bracket right)
          then ""
          else " "
        in
        l ^ sep ^ r

let render ?(pad_brackets = false) ~(string_of_atom : atom -> string)
    ~(string_of_arg : 'a -> string) (mixfix : 'a t) : string =
  render_with ~pad_brackets
    (function Arg a -> string_of_arg a | Atom atom -> string_of_atom atom)
    mixfix

let assemble ?(pad_brackets = false) ~(string_of_atom : atom -> string)
    (mixop : mixop) (args : string list) : string =
  let filled = fill mixop args in
  render ~pad_brackets ~string_of_atom ~string_of_arg:Fun.id filled

let string_of_mixeme = function
  | Arg _ -> "%"
  | Atom atom -> Xl.Atom.string_of_atom atom.it

let to_string (mixfix : 'a t) : string =
  "`" ^ render_with ~pad_brackets:false string_of_mixeme mixfix ^ "`"
