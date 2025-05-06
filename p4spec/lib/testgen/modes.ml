(* Modes for the fuzzer *)

type logmode = Silent | Verbose
type bootmode = Cold of string | Warm of string
type targetmode = Roundrobin | Target of string
type mutationmode = Random | Derive | Hybrid
type covermode = Strict | Relaxed

type t = {
  logmode : logmode;
  bootmode : bootmode;
  targetmode : targetmode;
  mutationmode : mutationmode;
  covermode : covermode;
}
