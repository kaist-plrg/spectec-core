open Utils
open Flatmap

(* Global store *)
(* Instances should be distinguishable by their paths,
   or fully-qualified names, so a store can be a flat map *)

module GSto = FlatMap (Path) (Object)

type store = GSto.t
