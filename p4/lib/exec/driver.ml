open Domain.Dom
module Ctk = Il.Ctk
module Types = Runtime_type.Types
module Envs_static = Runtime_static.Envs
open Il.Ast
module Table = Runtime_dynamic.Table
module Obj = Runtime_dynamic.Object
module Envs_dynamic = Runtime_dynamic.Envs
module VEnv = Envs_dynamic.VEnv
module TDEnv = Envs_dynamic.TDEnv
module FEnv = Envs_dynamic.FEnv
module CEnv = Envs_dynamic.CEnv
module Sto = Envs_dynamic.Sto
open Sigs
open Util.Source
open Util.Error

let error = error_driver

(* (TODO) Inserts VoidT, shouldn't matter in dynamics but not a good practice either *)
let no_info_expr = (no_info, Il.Ast.{ typ = Types.VoidT; ctk = Ctk.DYN })

(* Types for the driver *)

type port = int
type packet = string
type result = port * packet

let compare_packet packet_out packet_expect : bool =
  let to_list s = List.init (String.length s) (String.get s) in
  let packet_out = to_list packet_out in
  let packet_expect = to_list packet_expect in
  List.length packet_out = List.length packet_expect
  && List.fold_left2
       (fun same o e -> same && (e = '*' || o = e))
       true packet_out packet_expect

let compare_result (port_out, packet_out) (port_expect, packet_expect) : bool =
  let pass =
    port_out = port_expect && compare_packet packet_out packet_expect
  in
  if pass then
    F.printf "[PASS] Expected: %d %s / Got: %d %s\n" port_expect packet_expect
      port_out packet_out
  else
    F.printf "[FAIL] Expected: %d %s / Got: %d %s\n" port_expect packet_expect
      port_out packet_out;
  pass

(* Helpers *)

let make_expr_base (path : OId.t) =
  let base, members =
    match path with base :: members -> (base, members) | _ -> assert false
  in
  let var_base = Lang.Ast.Current (base $ no_info) $ no_info in
  let expr_base = Il.Ast.VarE { var = var_base } $$ no_info_expr in
  List.fold_left
    (fun expr_base member ->
      let member = member $ no_info in
      Il.Ast.ExprAccE { expr_base; member } $$ no_info_expr)
    expr_base members

let make_arg (arg : Id.t) =
  let var_arg = Lang.Ast.Current (arg $ no_info) $ no_info in
  let expr_arg = Il.Ast.VarE { var = var_arg } $$ no_info_expr in
  Lang.Ast.ExprA expr_arg $ no_info

let make_call (path : OId.t) (func : Id.t) (args : Id.t list) =
  let expr_base = make_expr_base path in
  let args = List.map make_arg args in
  (expr_base, func $ no_info, args)

module type ARCH = sig
  val init : Ctx.t -> Sto.t -> Ctx.t * Sto.t
  val eval_extern_func_call : Ctx.t -> FId.t -> Ctx.t * SSig.t
  val eval_extern_method_call : Ctx.t -> OId.t -> FId.t -> Ctx.t * SSig.t
  val drive_pipe : Ctx.t -> port -> packet -> result option
end

module type INTERP = sig
  val sto : Sto.t ref
  val init : Sto.t -> unit
  val update : OId.t -> Obj.t -> unit

  val eval_method_call :
    Ctx.cursor ->
    Ctx.t ->
    expr ->
    member ->
    targ list ->
    arg list ->
    Ctx.t * SSig.t
end

module type DRIVER = sig
  val run :
    CEnv.t -> TDEnv.t -> FEnv.t -> VEnv.t -> Sto.t -> Stf.Ast.stmt list -> bool
end

module Make
    (MakeArch : functor (Interp : INTERP) -> ARCH)
    (MakeInterp : functor (Arch : ARCH) -> INTERP) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp)
  and Interp : INTERP = MakeInterp (Arch)

  (* Table operations *)

  let find_table (id_table : Stf.Ast.name) : OId.t * Obj.t =
    let oid_table =
      match String.split_on_char '.' id_table with
      | [] -> assert false
      | [ id_unqualified ] ->
          !Interp.sto |> Sto.bindings |> List.map fst
          |> List.find_opt (fun oid ->
                 oid |> List.rev |> List.hd = id_unqualified)
      | oid_table -> Some oid_table
    in
    check (Option.is_some oid_table)
      (F.asprintf "(find_table) table %a not found" Stf.Print.print_name
         id_table);
    let oid_table = Option.get oid_table in
    let obj_table = Sto.find oid_table !Interp.sto in
    (oid_table, obj_table)

  let find_table_key (id_key : Stf.Ast.name) (table : Table.t) :
      int * Il.Ast.table_key =
    let table_key_indexed =
      table.keys
      |> List.mapi (fun idx key -> (idx, key))
      |> List.filter_map (fun (idx, table_key) ->
             let expr_key, _match_kind, annos = table_key.it in
             let match_anno_name =
               List.exists
                 (fun anno ->
                   match anno.it with
                   | Lang.Ast.TextN ({ it = "name"; _ }, [ name ]) ->
                       let name =
                         String.sub name.it 1 (String.length name.it - 2)
                       in
                       name = id_key
                   | _ -> false)
                 annos
             in
             let match_expr_key =
               let id_key = Stf.Print.convert_dollar_to_brackets id_key in
               F.asprintf "%a" (Il.Pp.pp_expr ~level:0) expr_key = id_key
             in
             if match_anno_name || match_expr_key then Some (idx, table_key)
             else None)
    in
    match table_key_indexed with
    | [ (idx, table_key) ] -> (idx, table_key)
    | _ ->
        F.asprintf "(TODO: find_table_key) %a" Stf.Print.print_name id_key
        |> error

  let find_table_action (id_action : Stf.Ast.name) (table : Table.t) :
      Il.Ast.table_action =
    let id_action =
      match String.split_on_char '.' id_action with
      | [] -> assert false
      | [ id_unqualified ] -> id_unqualified
      (* (TODO) Should check that the prefix of the qualified name is valid *)
      | oid_action -> oid_action |> List.rev |> List.hd
    in
    let table_action =
      List.filter_map
        (fun table_action ->
          let var_action, _, _, _, _ = table_action.it in
          match var_action.it with
          | Lang.Ast.Top id | Lang.Ast.Current id ->
              if id.it = id_action then Some table_action else None)
        table.actions
    in
    match table_action with
    | [ table_action ] -> table_action
    | _ ->
        F.asprintf "(find_table_action) no matching action found for %a"
          Stf.Print.print_name id_action
        |> error_driver

  let make_exact_key (num_key : Stf.Ast.number) =
    let num_key = num_key |> int_of_string |> Bigint.of_int in
    let num_key = (num_key, None) $ no_info in
    Il.Ast.(
      NumE { num = num_key } $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))

  let make_ternary_key (num_key : Stf.Ast.number) =
    let hex = String.starts_with ~prefix:"0x" num_key in
    check hex "(TODO: make_ternary_key) non-hexadecimal number";
    let num_key = String.sub num_key 2 (String.length num_key - 2) in
    let base =
      String.init (String.length num_key) (fun i ->
          if num_key.[i] = '*' then '0' else num_key.[i])
    in
    let base = "0x" ^ base |> int_of_string |> Bigint.of_int in
    let num_base = (base, None) $ no_info in
    let expr_base =
      Il.Ast.(
        NumE { num = num_base } $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))
    in
    let mask =
      String.init (String.length num_key) (fun i ->
          if num_key.[i] = '*' then '0' else 'F')
    in
    let mask = "0x" ^ mask |> int_of_string |> Bigint.of_int in
    let num_mask = (mask, None) $ no_info in
    let expr_mask =
      Il.Ast.(
        NumE { num = num_mask } $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))
    in
    Il.Ast.(
      MaskE { expr_base; expr_mask }
      $$ (no_info, { typ = Types.IntT; ctk = Ctk.DYN }))

  let add_table_entry (id_table : Stf.Ast.name) (keys : Stf.Ast.mtch list)
      (action : Stf.Ast.action) (priority : int option) : unit =
    let oid_table, obj_table = find_table id_table in
    let id_table, venv_table, table = Obj.get_table obj_table in
    let keysets =
      List.map
        (fun (id_key, mtchkind) ->
          let idx, table_key = find_table_key id_key table in
          let expr_key, match_kind, _annos = table_key.it in
          let typ_key = expr_key.note.typ in
          match mtchkind with
          | Stf.Ast.Num num_key ->
              let expr_key =
                match match_kind.it with
                | "exact" -> make_exact_key num_key
                | "ternary" -> make_ternary_key num_key
                | _ ->
                    F.asprintf "(TODO: add_table_entry) %a"
                      Stf.Print.print_mtchkind mtchkind
                    |> error
              in
              let typ_key_set = Types.SetT typ_key in
              let expr_key =
                Il.Ast.(
                  CastE { typ = typ_key_set $ no_info; expr = expr_key }
                  $$ (no_info, { typ = typ_key_set; ctk = Ctk.DYN }))
              in
              let keyset = Lang.Ast.ExprK expr_key $ no_info in
              (idx, keyset)
          | _ ->
              F.asprintf "(TODO: add_table_entry) %a" Stf.Print.print_mtchkind
                mtchkind
              |> error)
        keys
      |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
      |> List.map snd
    in
    let action =
      let id_action, args_action_supplied = action in
      let table_action = find_table_action id_action table in
      let var_action, args_action, annos, params_data, params_control =
        table_action.it
      in
      let module PMap = Map.Make (String) in
      let pmap_control =
        List.map it params_control
        |> List.mapi (fun idx (id, _, typ, _, _) -> (idx, id.it, typ.it))
        |> List.fold_left
             (fun pmap (idx, id, typ) -> PMap.add id (idx, typ) pmap)
             PMap.empty
      in
      let args_action_supplied =
        args_action_supplied
        |> List.map (fun arg_action_supplied ->
               let id_arg, num_arg = arg_action_supplied in
               let idx, typ = PMap.find id_arg pmap_control in
               let num_arg = num_arg |> int_of_string |> Bigint.of_int in
               let num_arg = (num_arg, None) $ no_info in
               let expr_arg = Il.Ast.NumE { num = num_arg } $$ no_info_expr in
               let expr_arg =
                 Il.Ast.(
                   CastE { typ = typ $ no_info; expr = expr_arg }
                   $$ (no_info, { typ; ctk = Ctk.DYN }))
               in
               let arg = Lang.Ast.ExprA expr_arg $ no_info in
               (idx, arg))
        |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
        |> List.map snd
      in
      let args_action = args_action @ args_action_supplied in
      (var_action, args_action, annos, params_data @ params_control, [])
      $ no_info
    in
    (* (TODO) Should validate priority value *)
    let priority =
      Option.map
        (fun priority ->
          let num = (Bigint.of_int priority, None) $ no_info in
          let expr = Il.Ast.NumE { num } in
          Value.IntV (Bigint.of_int priority) $$ (no_info, expr))
        priority
    in
    let table = Table.add_entry keysets action priority table in
    let obj_table = Obj.TableO (id_table, venv_table, table) in
    Interp.update oid_table obj_table

  let add_table_default (id_table : Stf.Ast.name) (action : Stf.Ast.action) :
      unit =
    let oid_table, obj_table = find_table id_table in
    let id_table, venv_table, table = Obj.get_table obj_table in
    let table_action_default_const, _ = table.action_default in
    if table_action_default_const then
      F.asprintf "(add_table_default) default action is declared as constant"
      |> error;
    let id_action, args_action = action in
    let table_action = find_table_action id_action table in
    let var_action, _, annos, params_data, params_control = table_action.it in
    let module PMap = Map.Make (String) in
    let pmap_control =
      List.map it (params_data @ params_control)
      |> List.mapi (fun idx (id, _, typ, _, _) -> (idx, id.it, typ.it))
      |> List.fold_left
           (fun pmap (idx, id, typ) -> PMap.add id (idx, typ) pmap)
           PMap.empty
    in
    let args_action =
      args_action
      |> List.map (fun arg_action ->
             let id_arg, num_arg = arg_action in
             let idx, typ = PMap.find id_arg pmap_control in
             let num_arg = num_arg |> int_of_string |> Bigint.of_int in
             let num_arg = (num_arg, None) $ no_info in
             let expr_arg = Il.Ast.NumE { num = num_arg } $$ no_info_expr in
             let expr_arg =
               Il.Ast.(
                 CastE { typ = typ $ no_info; expr = expr_arg }
                 $$ (no_info, { typ; ctk = Ctk.DYN }))
             in
             let arg = Lang.Ast.ExprA expr_arg $ no_info in
             (idx, arg))
      |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
      |> List.map snd
    in
    let action =
      (var_action, args_action, annos, params_data @ params_control, [])
      $ no_info
    in
    let table = Table.add_default action table in
    let obj_table = Obj.TableO (id_table, venv_table, table) in
    Interp.update oid_table obj_table

  (* STF interpreter *)

  let run_stf_stmt (ctx : Ctx.t) (pass : bool) (queue_packet : result list)
      (queue_expect : result list) (stmt_stf : Stf.Ast.stmt) :
      Ctx.t * bool * result list * result list =
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in) -> (
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let result_out = Arch.drive_pipe ctx port_in packet_in in
        match result_out with
        | None -> (ctx, pass, queue_packet, queue_expect)
        | Some (port_out, packet_out) -> (
            match queue_expect with
            | [] ->
                let queue_packet = queue_packet @ [ (port_out, packet_out) ] in
                (ctx, pass, queue_packet, queue_expect)
            | (port_expect, packet_expect) :: queue_expect ->
                let pass =
                  compare_result (port_out, packet_out)
                    (port_expect, packet_expect)
                  && pass
                in
                (ctx, pass, queue_packet, queue_expect)))
    | Stf.Ast.Expect (port_expect, Some packet_expect) -> (
        let port_expect = int_of_string port_expect in
        let packet_expect = String.uppercase_ascii packet_expect in
        match queue_packet with
        | [] ->
            ( ctx,
              pass,
              queue_packet,
              queue_expect @ [ (port_expect, packet_expect) ] )
        | (port_out, packet_out) :: queue_packet ->
            let pass =
              compare_result (port_out, packet_out) (port_expect, packet_expect)
              && pass
            in
            (ctx, pass, queue_packet, queue_expect))
    (* Table operations *)
    | Stf.Ast.Add (id_table, priority, keys, action, _) ->
        add_table_entry id_table keys action priority;
        (ctx, pass, queue_packet, queue_expect)
    | Stf.Ast.SetDefault (id_table, action) ->
        add_table_default id_table action;
        (ctx, pass, queue_packet, queue_expect)
    (* Timing *)
    | Stf.Ast.Wait -> (ctx, pass, queue_packet, queue_expect)
    | _ ->
        F.asprintf "(run_stf_stmt) unknown stf stmt: %a" Stf.Print.print_stmt
          stmt_stf
        |> error

  let run_stf_stmts (ctx : Ctx.t) (stmts_stf : Stf.Ast.stmt list) : bool =
    let _, pass, queue_packet, queue_expect =
      List.fold_left
        (fun (ctx, pass, queue_packet, queue_expect) stmt_stf ->
          run_stf_stmt ctx pass queue_packet queue_expect stmt_stf)
        (ctx, true, [], []) stmts_stf
    in
    let pass = pass && queue_packet = [] && queue_expect = [] in
    if queue_packet <> [] then (
      F.printf "[FAIL] Remaining packets to be matched:\n";
      List.iteri
        (fun idx (port, packet) -> F.printf "(%d) %d %s\n" idx port packet)
        queue_packet);
    if queue_expect <> [] then (
      F.printf "[FAIL] Expected packets to be output:\n";
      List.iteri
        (fun idx (port, packet) -> F.printf "(%d) %d %s\n" idx port packet)
        queue_expect);
    pass

  let run (cenv : CEnv.t) (tdenv : TDEnv.t) (fenv : FEnv.t) (venv : VEnv.t)
      (sto : Sto.t) (stmts_stf : Stf.Ast.stmt list) : bool =
    let ctx = { Ctx.empty with global = { cenv; tdenv; fenv; venv } } in
    let ctx, sto = Arch.init ctx sto in
    Interp.init sto;
    run_stf_stmts ctx stmts_stf
end
