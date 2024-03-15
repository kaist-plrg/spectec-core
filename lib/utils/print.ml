(* Utils *)

let print_indent (indent: int) = String.make (indent * 2) ' '

let print_inline (s: string) =
  let len = String.length s in
  let rec replace acc i =
    if i < len then
      let c = s.[i] in
      if c = '\n' then replace (acc ^ " ") (i + 1)
      else replace (acc ^ String.make 1 c) (i + 1)
    else acc
  in
  replace "" 0
