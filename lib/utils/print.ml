(* Utils *)

let print_indent (indent : int) = String.make (indent * 2) ' '

let print_inline (s : string) =
  let rec replace_newline acc = function
    | [] -> acc
    | '\n' :: tl -> replace_newline (' ' :: acc) tl
    | hd :: tl -> replace_newline (hd :: acc) tl
  in
  let rec reduce_spaces acc = function
    | [] -> acc
    | ' ' :: ' ' :: tl -> reduce_spaces acc (' ' :: tl)
    | hd :: tl -> reduce_spaces (hd :: acc) tl
  in
  s |> String.to_seq |> List.of_seq |> replace_newline [] |> reduce_spaces []
  |> List.to_seq |> String.of_seq
