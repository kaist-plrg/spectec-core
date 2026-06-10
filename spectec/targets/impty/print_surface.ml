(* Inverts the IL [value] constructors built in {!Parse}. Expression precedence
   mirrors [parse_expr] and friends (loosest to tightest: [&&], [<=], [+], unary
   [!], call, primary) so the output stays minimally parenthesised yet re-parses
   to the same tree. The public contract lives in the .mli. *)

open Lang.Il

(** Raised when a value has no surface-syntax representation. *)
exception Unsupported of string

let flatten = Value.flatten_case_v
let paren cond s = if cond then "(" ^ s ^ ")" else s

let unsupported what id atoms n =
  raise
    (Unsupported
       (Printf.sprintf "%s: %s [%s]/%d" what id (String.concat ";" atoms) n))

let rec string_of_expr ~prec (v : Value.t) : string =
  let id, atoms, vs = flatten v in
  match (id, atoms, vs) with
  (* Generators ({!Manual_gen}) build literal and id leaves under synid [expr];
     {!Parse} builds them under [literal]/[id]. Match either. *)
  | _, [ "_NUM" ], [ n ] -> Bigint.to_string (Xl.Num.to_int (Value.get_num n))
  | _, [ "_BOOL" ], [ b ] -> if Value.get_bool b then "true" else "false"
  | _, [ "_ID" ], [ s ] -> Value.get_text s
  | "expr", [ "&&" ], [ l; r ] ->
      paren (prec > 0)
        (string_of_expr ~prec:0 l ^ " && " ^ string_of_expr ~prec:1 r)
  | "expr", [ "<=" ], [ l; r ] ->
      paren (prec > 1)
        (string_of_expr ~prec:1 l ^ " <= " ^ string_of_expr ~prec:2 r)
  | "expr", [ "+" ], [ l; r ] ->
      paren (prec > 2)
        (string_of_expr ~prec:2 l ^ " + " ^ string_of_expr ~prec:3 r)
  | "expr", [ "!" ], [ e ] -> paren (prec > 3) ("!" ^ string_of_expr ~prec:3 e)
  | "expr", [ "("; ")" ], [ f; a ] ->
      paren (prec > 4)
        (string_of_expr ~prec:4 f ^ "(" ^ string_of_expr ~prec:0 a ^ ")")
  | "expr", [ "FUN"; "("; ")"; "->"; "{"; "}" ], [ t_arg; idv; t_ret; body ] ->
      Printf.sprintf "fun (%s %s) -> %s { %s }" (string_of_type t_arg)
        (text_of_id idv) (string_of_type t_ret)
        (string_of_expr ~prec:0 body)
  | _ -> unsupported "expr" id atoms (List.length vs)

and string_of_type (v : Value.t) : string =
  let id, atoms, vs = flatten v in
  match (id, atoms, vs) with
  | "type", [ "INT" ], [] -> "int"
  | "type", [ "BOOL" ], [] -> "bool"
  | "type", [ "->" ], [ a; b ] ->
      string_of_type_atom a ^ " -> " ^ string_of_type b
  | _ -> unsupported "type" id atoms (List.length vs)

(* Function arrow is right-associative; parenthesize an arrow on the left. *)
and string_of_type_atom (v : Value.t) : string =
  match flatten v with
  | "type", [ "->" ], _ -> "(" ^ string_of_type v ^ ")"
  | _ -> string_of_type v

and string_of_command (v : Value.t) : string =
  let id, atoms, vs = flatten v in
  match (id, atoms, vs) with
  | "command", [ "SKIP" ], [] -> "skip"
  | "command", [ "=" ], [ idv; e ] ->
      Printf.sprintf "%s = %s" (text_of_id idv) (string_of_expr ~prec:0 e)
  | "command", [ "=" ], [ t; idv; e ] ->
      Printf.sprintf "%s %s = %s" (string_of_type t) (text_of_id idv)
        (string_of_expr ~prec:0 e)
  | "command", [ ";" ], [ c1; c2 ] ->
      string_of_command c1 ^ ";\n" ^ string_of_command c2
  | "command", [ "IF"; "THEN"; "ELSE"; "END" ], [ c; s1; s2 ] ->
      Printf.sprintf "if %s then %s else %s end" (string_of_expr ~prec:0 c)
        (string_of_command s1) (string_of_command s2)
  | "command", [ "WHILE"; "DO"; "END" ], [ c; body ] ->
      Printf.sprintf "while %s do %s end" (string_of_expr ~prec:0 c)
        (string_of_command body)
  | _ -> unsupported "command" id atoms (List.length vs)

and text_of_id (v : Value.t) : string =
  match flatten v with
  | _, [ "_ID" ], [ s ] -> Value.get_text s
  | id, atoms, vs -> unsupported "id" id atoms (List.length vs)

(* [prog = command], so a program is just its top-level command. *)
let string_of_prog (v : Value.t) : string = string_of_command v
