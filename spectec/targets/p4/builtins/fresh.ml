module Il = Lang.Il
open Il
open Common.Source
open Builtins
open Error

(* Global tid provider for P4 *)
module GlobalTidProvider : sig
  val with_provider : (unit -> string) -> (unit -> 'a) -> 'a
  val fresh : unit -> string
end = struct
  let provider : (unit -> string) ref = ref (fun () -> "FRESH__0")

  let with_provider p f =
    let previous = !provider in
    provider := p;
    Fun.protect f ~finally:(fun () -> provider := previous)

  let fresh () = !provider ()
end

(* dec $fresh_tid() : tid *)
let fresh_tid ~at : Value.t result =
  at |> ignore;
  let tid = GlobalTidProvider.fresh () in
  let typ = VarT { synid = "tid" $ no_region; targs = [] } in
  Ok (Il.Value.Make.text typ tid)

let builtins = [ ("fresh_typeId", Define.T0.a0 fresh_tid) ]
