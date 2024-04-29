open Syntax
open Syntax.Ast
open Domain
open Domain.Scope
open Domain.Ccenv
open Domain.Ienv

type bscope = Env.t * Env.t * Sto.t

let init_bscope (tdenv : TDEnv.t) =
  let genv = Env.empty in
  let lenv = Env.empty in
  let sto = Sto.empty in
  let genv, sto =
    let typ = Type.TRef in
    let value = Value.VRef [ "packet" ] in
    add_var "packet" typ value genv sto
  in
  let lenv, sto =
    let typ = TDEnv.find "headers" tdenv in
    let value = Interpreter.default_value typ in
    add_var "hdr" typ value lenv sto
  in
  let lenv, sto =
    let typ = TDEnv.find "metadata" tdenv in
    let value = Interpreter.default_value typ in
    add_var "meta" typ value lenv sto
  in
  let lenv, sto =
    let typ = TDEnv.find "standard_metadata_t" tdenv in
    let value = Interpreter.default_value typ in
    add_var "standard_metadata" typ value lenv sto 
  in
  (genv, lenv, sto)

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

let drive_instantiation (tdenv : TDEnv.t) (ccenv : CcEnv.t) (ienv : IEnv.t) =
  let packet_in_cclos = Env.find "packet_in" ccenv in
  let ienv =
    Instance.Instantiate.instantiate_cclos tdenv Env.empty Sto.empty
      ccenv ienv [ "packet" ] packet_in_cclos [] []
  in
  ienv

let drive_parser_impl (tdenv : TDEnv.t) (bscope : bscope) =
  let parser_impl = "main.p" in
  let parser_impl_args =
    apply_args [ "packet"; "hdr"; "meta"; "standard_metadata" ]
  in
  Interpreter.eval_method_call tdenv bscope parser_impl "apply" parser_impl_args
    []

let drive_ingress (tdenv : TDEnv.t) (bscope : bscope) =
  let ingress = "main.ig" in
  let ingress_args = apply_args [ "hdr"; "meta"; "standard_metadata" ] in
  Interpreter.eval_method_call tdenv bscope ingress "apply" ingress_args []

let drive (tdenv : TDEnv.t) (ccenv : CcEnv.t) (ienv : IEnv.t) =
  (* Instantiations that should be done by the architecture *)
  let ienv = drive_instantiation tdenv ccenv ienv in
  Interpreter.register_ienv ienv;
  (* Build an environment to call parser apply *)
  let bscope = init_bscope tdenv in
  (* Obtain parser object from store and call apply *)
  print_endline "Calling main.p.apply()";
  let bscope = drive_parser_impl tdenv bscope in
  Interpreter.print_bscope bscope;
  (* Obtain control object from store and call apply *)
  print_endline "Calling main.ig.apply()";
  let bscope = drive_ingress tdenv bscope in
  Interpreter.print_bscope bscope;
  ()
