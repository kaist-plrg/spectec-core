type t = string list

let print (t : t) = String.concat "." t
let concat = print
let compare = compare
