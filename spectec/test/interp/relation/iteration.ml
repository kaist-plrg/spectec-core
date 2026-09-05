open Core
module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ

let ( $ ) = Common.Source.( $ )
let no_region = Common.Source.no_region
let nat n = V.nat (Bigint.of_int n)
let nats ns = V.list (Typ.nat $ no_region) (List.map ns ~f:nat)
let spec = H.compile_file "iteration.spectec"

let run () =
  H.check spec ~name:"iterated relation binds each output" ~relation:"Add_each"
    ~args:(fun () -> [ nats [ 1; 2; 3 ]; nats [ 10; 20; 30 ] ])
    (H.returns (fun () -> [ nats [ 11; 22; 33 ] ]));
  H.check spec ~name:"iterated relation accepts empty inputs"
    ~relation:"Add_each"
    ~args:(fun () -> [ nats []; nats [] ])
    (H.returns (fun () -> [ nats [] ]));
  H.check spec ~name:"iterated relation rejects different input lengths"
    ~relation:"Add_each"
    ~args:(fun () -> [ nats [ 1; 2 ]; nats [ 10 ] ])
    (H.fails_with "cannot transpose a matrix of value batches")
