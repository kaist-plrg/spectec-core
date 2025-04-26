open Domain.Lib
open Sl.Ast
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple

(* Generate derivations from closest-misses *)

let gen_typing (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string) (pids_boot : PIdSet.t) :
    unit =
  let ctx, _ =
    Interp_sl.Interp.run_typing ~derive:true spec includes_p4 filename_p4
  in
  let cover = !(ctx.cover) in
  let graph = Option.get ctx.graph in
  let misses = SCov.collect_miss cover in
  Derive.derive_misses dirname_derive filename_p4 graph pids_boot misses

let gen_typing_cold (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string)
    (filenames_p4 : string list) : unit =
  let pids_boot = Boot.boot_cold spec includes_p4 filenames_p4 in
  gen_typing spec dirname_derive includes_p4 filename_p4 pids_boot

let gen_typing_warm (spec : spec) (dirname_derive : string)
    (includes_p4 : string list) (filename_p4 : string) (filename_cov : string) :
    unit =
  let pids_boot = Boot.boot_warm filename_cov in
  gen_typing spec dirname_derive includes_p4 filename_p4 pids_boot

let cover_typing (spec : spec) (includes_p4 : string list)
    (filenames_p4 : string list) : unit =
  let cover_multi =
    Interp_sl.Interp.cover_typing spec includes_p4 filenames_p4
  in
  MCov.log cover_multi
