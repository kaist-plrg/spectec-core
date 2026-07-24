module Mixfix = Il.Mixfix
module Terminal = Grammar.Terminal

exception Error of string

type terminal_printer = Terminal.t -> string

let run (print_terminal : terminal_printer) (grammar : Grammar.t)
    (start : string) (value : Il.value) : string =
  let find_syntax name =
    match
      List.find_opt
        (fun (syntax : Grammar.syntax) -> syntax.name.it = name)
        grammar
    with
    | Some syntax -> syntax
    | None -> failwith ("Print: grammar references unknown syntax " ^ name)
  in

  (* A plain inclusion like [prog = command] has no case of its own. This
     returns the name of the syntax it includes, which [print_phrase] then
     tries instead. *)
  let find_alias_target (syntax : Grammar.syntax) : string option =
    List.find_map
      (fun (p : Grammar.production) ->
        match (p.construction, p.notation) with
        | Grammar.Alias, [ Mixfix.Arg (Grammar.Nonterminal target) ] ->
            Some target.it
        | _ -> None)
      syntax.productions
  in

  (* [min_rank] is the lowest precedence an operator can have and still be
     printed without parentheses. It is passed down to each operand, and any
     operator with a lower precedence is wrapped in parentheses. *)
  let rec print_phrase (syntax : Grammar.syntax) min_rank (value : Il.value) :
      string =
    let mixop, values =
      match value.it with
      | Il.CaseV valuecase -> Mixfix.split valuecase
      | _ -> failwith "Print: expected a case value"
    in
    let matches (p : Grammar.production) =
      p.construction = Grammar.Case && Mixfix.eq_mixop p.notation mixop
    in
    match List.find_opt matches syntax.productions with
    | Some prod -> print_production syntax min_rank prod values
    | None -> (
        match find_alias_target syntax with
        | Some target -> print_phrase (find_syntax target) min_rank value
        | None ->
            raise
              (Error ("no production matches a " ^ syntax.name.it ^ " value")))
  and print_production syntax min_rank (prod : Grammar.production)
      (values : Il.value list) : string =
    let operator_rank =
      match Grammar.operator_atom prod with
      | Some atom -> Grammar.rank syntax atom
      | None -> 0
    in
    (* [operand_rank] is the [min_rank] passed to each operand. Only an infix
       treats its operands differently: the left operand keeps [operator_rank]
       and the right gets [operator_rank] + 1, which makes equal-precedence
       operators associate to the left. *)
    let operand_rank ~is_left =
      match prod.recursion with
      | Grammar.Both -> if is_left then operator_rank else operator_rank + 1
      | Grammar.Right | Grammar.Left -> operator_rank
      | Grammar.Neither -> 0
    in
    let rec render_notation ~left_operand_printed values = function
      | [] -> []
      | Mixfix.Atom { it = Xl.Atom.Tag _; _ } :: rest ->
          render_notation ~left_operand_printed values rest
      | Mixfix.Atom atom :: rest ->
          print_terminal (Terminal.Atom atom.it)
          :: render_notation ~left_operand_printed values rest
      | Mixfix.Arg arg :: rest -> (
          match values with
          | [] -> failwith "Print: production argument count mismatch"
          | value :: values -> (
              match arg with
              | Grammar.Primitive prim ->
                  print_terminal (Terminal.Primitive (prim, value))
                  :: render_notation ~left_operand_printed values rest
              | Grammar.Nonterminal sub when sub.it = syntax.name.it ->
                  print_phrase syntax
                    (operand_rank ~is_left:(not left_operand_printed))
                    value
                  :: render_notation ~left_operand_printed:true values rest
              | Grammar.Nonterminal sub ->
                  print_phrase (find_syntax sub.it) 0 value
                  :: render_notation ~left_operand_printed values rest))
    in
    let needs_parens =
      match prod.recursion with
      | Grammar.Both | Grammar.Right | Grammar.Left -> min_rank > operator_rank
      | Grammar.Neither -> false
    in
    let rendered =
      render_notation ~left_operand_printed:false values prod.notation
      |> String.concat " "
    in
    if needs_parens then "(" ^ rendered ^ ")" else rendered
  in

  print_phrase (find_syntax start) 0 value
