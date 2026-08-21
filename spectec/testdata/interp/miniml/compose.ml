let compose = fun f -> fun g -> fun x -> f (g x) in
let plus = fun x -> fun y -> + (x, y) in
compose (plus 2) (plus 4) 36
