open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Cclos

let instantiate_toplevel_decl (_genv: VTEnv.t) (_ccenv: CCEnv.t) (_path: Path.t) (decl: decl) =
  match decl with
  | InstD _ -> failwith "InstD not implemented";
  | _ -> failwith "Not implemented"

let instantiate_program (program: program) =
  List.fold_left
    (fun (genv, ccenv) decl ->
      instantiate_toplevel_decl genv ccenv [] decl)
    (VTEnv.empty, CCEnv.empty) program
