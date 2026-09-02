// Fibonacci

// y = \f. (\x. f (\y. x x y)) (\x. f (\y. x x y))
let y = fun f ->
  let d = fun x -> f (fun y -> x x y) in
  d d in
y
  (fun f -> fun n ->
    ifz (n,
         (fun _ -> 0,
          fun _ -> let n = - (n, 1) in
                   ifz (n,
                        (fun _ -> 1,
                         fun _ -> + (f n, f (- (n, 1)))
                     ))
      ))
  )
  10
