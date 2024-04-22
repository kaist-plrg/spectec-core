type t = int

let print (t : t) = string_of_int t
let compare = compare
let inc = ( + ) 1
let max (ts : t list) = List.fold_left max 0 ts
