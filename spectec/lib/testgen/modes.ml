(* Modes for the fuzzer *)

type logmode = Silent | Verbose
type bootmode = Cold of string list * string | Warm of string
type mutationmode = Random | Derive | Hybrid
type covermode = Strict | Relaxed

type t = {
  logmode : logmode;
  bootmode : bootmode;
  mutationmode : mutationmode;
  covermode : covermode;
}
