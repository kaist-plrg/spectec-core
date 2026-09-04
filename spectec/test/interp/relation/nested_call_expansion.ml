open Core
module H = Harness
module V = Lang.Il.Value
module Typ = Lang.Il.Typ

let ( $ ) = Common.Source.( $ )
let no_region = Common.Source.no_region
let nat_type = Typ.nat $ no_region
let nat_opt_type = Typ.opt nat_type $ no_region
let nat n = V.nat (Bigint.of_int n)
let nat_opt n = V.opt nat_type (Option.map n ~f:nat)
let nat_opts ns = V.list nat_opt_type (List.map ns ~f:nat_opt)
let spec = H.compile_file "nested_call_expansion.spectec"

let run () =
  H.check spec ~name:"lifted call preserves optional and list dimensions"
    ~relation:"Map_nested"
    ~args:(fun () -> [ nat_opts [ Some 1; None; Some 2 ] ])
    (H.returns (fun () -> [ nat_opts [ Some 4; None; Some 6 ] ]));
  H.check spec
    ~name:"premise-local call remains inside optional and list iterations"
    ~relation:"Bind_nested"
    ~args:(fun () -> [ nat_opts [ Some 1; None; Some 2 ] ])
    (H.returns (fun () -> [ nat_opts [ Some 4; None; Some 6 ] ]))
