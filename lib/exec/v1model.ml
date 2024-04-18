open Syntax
open Ast
open Runtime
open Store
open Runtime.Envs
open Instance
open Envs

(* Glboal Store *)

type store = GSto.t

(* Environments *)

type env = CEnv.t * LEnv.t * TSto.t * VSto.t
type tdenv = TDEnv.t

let init_env tdenv =
  let cenv = CEnv.empty in
  let lenv = LEnv.empty in
  let tsto = TSto.empty in
  let vsto = VSto.empty in
  let cenv, tsto, vsto =
    let typ = Typ.Ref in
    let value = Value.Ref [ "packet" ] in
    load_const cenv tsto vsto "packet" typ value
  in
  let lenv, tsto, vsto =
    let typ = TDEnv.find "headers" tdenv in
    let value = Interpreter.default typ in
    add_var lenv tsto vsto "hdr" typ value
  in
  let lenv, tsto, vsto =
    let typ = TDEnv.find "metadata" tdenv in
    let value = Interpreter.default typ in
    add_var lenv tsto vsto "meta" typ value
  in
  let lenv, tsto, vsto =
    let typ = TDEnv.find "standard_metadata_t" tdenv in
    let value = Interpreter.default typ in
    add_var lenv tsto vsto "standard_metadata" typ value
  in
  (cenv, lenv, tsto, vsto)

(* Architecture *)

let apply_args (args : string list) =
  List.map
    (fun arg ->
      Argument.Expression
        {
          tags = Info.M "";
          value =
            Expression.Name { tags = Info.M ""; name = BareName { tags = Info.M ""; str = arg } };
        })
    args

let drive_instantiation (tdenv : tdenv) (ccenv : ccenv) (store : store) =
  let packet_in_cclos = CCEnv.find "packet_in" ccenv in
  let store =
    Instance.Instantiate.instantiate_cclos tdenv CEnv.empty TSto.empty
      VSto.empty ccenv store [ "packet" ] packet_in_cclos [] []
  in
  store

let drive_parser_impl (_tdenv : tdenv) (env : env) (store : store) =
  let parser_impl = GSto.find [ "main"; "p" ] store in
  let parser_impl_args = apply_args [ "packet"; "hdr"; "meta"; "standard_metadata" ] in
  Interpreter.eval_method_call env parser_impl "apply" parser_impl_args

let drive_ingress (_tdenv : tdenv) (env : env) (store : store) =
  let ingress = GSto.find [ "main"; "ig" ] store in
  let ingress_args = apply_args [ "hdr"; "meta"; "standard_metadata" ] in
  Interpreter.eval_method_call env ingress "apply" ingress_args

let drive (tdenv : tdenv) (ccenv : ccenv) (store : store) =
  (* Instantiations that should be done by the architecture *)
  let store = drive_instantiation tdenv ccenv store in
  Interpreter.register_store store;
  (* Build an environment to call parser apply *)
  let env = init_env tdenv in
  (* Obtain parser object from store and call apply *)
  print_endline "Calling ParserImpl p.apply()";
  let env = drive_parser_impl tdenv env store in
  (* Obtain control object from store and call apply *)
  print_endline "Calling ingress ig.apply()";
  let _ = drive_ingress tdenv env store in
  ()
