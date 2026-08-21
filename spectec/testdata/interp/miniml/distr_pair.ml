let distr_pair = fun f -> fun p -> (f (fst p), f (snd p)) in
+ (distr_pair (fun x -> x) (40, 2))
