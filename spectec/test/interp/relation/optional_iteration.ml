open Core
module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ

let ( $ ) = Common.Source.( $ )
let no_region = Common.Source.no_region
let nat n = V.nat (Bigint.of_int n)
let nat_opt n = V.opt (Typ.nat $ no_region) (Option.map n ~f:nat)
let spec = H.compile_file "optional_iteration.spectec"

let run () =
  H.check spec ~name:"optional relation binds present output"
    ~relation:"Add_opt"
    ~args:(fun () -> [ nat_opt (Some 1); nat_opt (Some 2) ])
    (H.returns (fun () -> [ nat_opt (Some 3) ]));
  H.check spec ~name:"optional relation binds absent output" ~relation:"Add_opt"
    ~args:(fun () -> [ nat_opt None; nat_opt None ])
    (H.returns (fun () -> [ nat_opt None ]));
  H.check spec ~name:"optional relation rejects mixed optionality"
    ~relation:"Add_opt"
    ~args:(fun () -> [ nat_opt (Some 1); nat_opt None ])
    (H.fails_with "mismatch in optionality of iterated variables");
  H.check spec ~name:"optional relation propagates present failure"
    ~relation:"Positive_opt"
    ~args:(fun () -> [ nat_opt (Some 0) ])
    H.fails;
  H.check spec ~name:"optional condition accepts present true"
    ~relation:"Require_positive_opt"
    ~args:(fun () -> [ nat_opt (Some 1) ])
    (H.returns (fun () -> [ nat_opt (Some 1) ]));
  H.check spec ~name:"optional condition rejects present false"
    ~relation:"Require_positive_opt"
    ~args:(fun () -> [ nat_opt (Some 0) ])
    H.fails;
  H.check spec ~name:"optional condition accepts absent input"
    ~relation:"Require_positive_opt"
    ~args:(fun () -> [ nat_opt None ])
    (H.returns (fun () -> [ nat_opt None ]));
  H.check spec ~name:"optional assertion accepts present success"
    ~relation:"Assert_positive_opt"
    ~args:(fun () -> [ nat_opt (Some 1) ])
    (H.returns (fun () -> [ nat_opt (Some 1) ]));
  H.check spec ~name:"optional assertion rejects present failure"
    ~relation:"Assert_positive_opt"
    ~args:(fun () -> [ nat_opt (Some 0) ])
    H.fails;
  H.check spec ~name:"optional assertion accepts absence"
    ~relation:"Assert_positive_opt"
    ~args:(fun () -> [ nat_opt None ])
    (H.returns (fun () -> [ nat_opt None ]));
  H.check spec ~name:"optional negated assertion accepts present failure"
    ~relation:"Assert_not_positive_opt"
    ~args:(fun () -> [ nat_opt (Some 0) ])
    (H.returns (fun () -> [ nat_opt (Some 0) ]));
  H.check spec ~name:"optional negated assertion rejects present success"
    ~relation:"Assert_not_positive_opt"
    ~args:(fun () -> [ nat_opt (Some 1) ])
    H.fails;
  H.check spec ~name:"optional negated assertion accepts absence"
    ~relation:"Assert_not_positive_opt"
    ~args:(fun () -> [ nat_opt None ])
    (H.returns (fun () -> [ nat_opt None ]))
