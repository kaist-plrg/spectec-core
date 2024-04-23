open Syntax
open Syntax.Ast
open Runtime.Domain
open Utils

let add_known = Scope.add_double

let init_benv tdenv =
  let genv = Env.empty in
  let lenv = Env.empty in
  let tsto = Heap.empty in
  let vsto = Heap.empty in
  let genv, tsto, vsto =
    let typ = TRef in
    let value = VRef [ "packet" ] in
    add_known "packet" typ value genv tsto vsto
  in
  let lenv, tsto, vsto =
    let typ = Env.find "headers" tdenv in
    let value = Interpreter.default_value typ in
    add_known "hdr" typ value lenv tsto vsto
  in
  let lenv, tsto, vsto =
    let typ = Env.find "metadata" tdenv in
    let value = Interpreter.default_value typ in
    add_known "meta" typ value lenv tsto vsto
  in
  let lenv, tsto, vsto =
    let typ = Env.find "standard_metadata_t" tdenv in
    let value = Interpreter.default_value typ in
    add_known "standard_metadata" typ value lenv tsto vsto
  in
  (genv, lenv, tsto, vsto)

(* Architecture *)

let apply_args (args : string list) =
  List.map
    (fun arg ->
      Argument.Expression
        {
          tags = Info.M "";
          value =
            Expression.Name
              {
                tags = Info.M "";
                name = BareName { tags = Info.M ""; str = arg };
              };
        })
    args

let drive_instantiation (tdenv : tdenv) (ccenv : ccenv) (ienv : ienv) =
  let packet_in_cclos = Env.find "packet_in" ccenv in
  let ienv =
    Instance.Instantiate.instantiate_cclos tdenv Env.empty Heap.empty Heap.empty
      ccenv ienv [ "packet" ] packet_in_cclos [] []
  in
  ienv

let drive_parser_impl (_tdenv : tdenv) (benv : benv) (ienv : ienv) =
  let parser_impl = Env.find "main.p" ienv in
  let parser_impl_args =
    apply_args [ "packet"; "hdr"; "meta"; "standard_metadata" ]
  in
  Interpreter.eval_method_call benv parser_impl "apply" parser_impl_args

let drive_ingress (_tdenv : tdenv) (benv : benv) (ienv : ienv) =
  let ingress = Env.find "main.ig" ienv in
  let ingress_args = apply_args [ "hdr"; "meta"; "standard_metadata" ] in
  Interpreter.eval_method_call benv ingress "apply" ingress_args

let drive (tdenv : tdenv) (ccenv : ccenv) (ienv : ienv) =
  (* Instantiations that should be done by the architecture *)
  let ienv = drive_instantiation tdenv ccenv ienv in
  Interpreter.register_store ienv;
  (* Build an environment to call parser apply *)
  let benv = init_benv tdenv in
  (* Obtain parser object from store and call apply *)
  print_endline "Calling main.p.apply()";
  let benv = drive_parser_impl tdenv benv ienv in
  (* Obtain control object from store and call apply *)
  print_endline "Calling main.ig.apply()";
  let _ = drive_ingress tdenv benv ienv in
  ()
