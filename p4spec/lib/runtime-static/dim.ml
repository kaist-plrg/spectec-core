(* Dimension : list of iterators from inside-out *)

type t = Il.Ast.iter list

let compare = compare

let equiv dim_a dim_b =
  List.length dim_a = List.length dim_b && List.for_all2 ( = ) dim_a dim_b

let sub dim_a dim_b =
  List.length dim_a <= List.length dim_b
  && List.for_all2 ( = ) dim_a
       (List.filteri (fun idx _ -> idx < List.length dim_a) dim_b)

let to_string t = t |> List.map Il.Print.string_of_iter |> String.concat ""
