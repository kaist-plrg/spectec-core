open Core
module Value = Lang.Il.Value
module Diagnostic = Spectec.Diagnostic

type compiled_spec = {
  filename : string;
  spec_il : Lang.Il.spec;
  spec_sl : Lang.Sl.spec;
  spec_pl : Lang.Pl.spec;
}

type mode = Il | Sl | Pl

type expectation =
  | Returns of (unit -> Value.t list)
  | Fails
  | Fails_with of string

module Pure_target : Interp.Target.S = struct
  let builtins = []
  let is_impure_func _ = false
  let is_impure_rel _ = false
  let state_version = ref 0

  let with_state f =
    let next_vid = ref 0 in
    let fresh_vid () =
      let vid = !next_vid in
      Int.incr next_vid;
      vid
    in
    Value.GlobalVidProvider.with_provider fresh_vid f
end

let render_diagnostics diagnostics =
  Diagnostic.Render.render_bag ~ansi:Diagnostic.Ansi.plain diagnostics

let fail_diagnostics filename diagnostics =
  failwithf "relation test spec %s emitted diagnostics:\n%s" filename
    (render_diagnostics diagnostics)
    ()

let compile_source ~filename contents =
  let source : Spectec.spec_source = { filename; contents } in
  let result, diagnostics =
    Spectec.with_diagnostics (fun () ->
        let ( let* ) = Stdlib.Result.bind in
        let* spec_el = Spectec.parse_spec_source source in
        let* spec_il = Spectec.elaborate spec_el in
        let henv =
          Spectec.henv_with_il_spec (Spectec.henv_of_el_spec spec_el) spec_il
        in
        let spec_sl = Spectec.structure spec_il in
        let spec_pl = spec_sl |> Spectec.annotate ~henv |> Spectec.shorten in
        Ok { filename; spec_il; spec_sl; spec_pl })
  in
  match result with
  | Ok spec when Diagnostic.Bag.is_empty diagnostics -> spec
  | Ok _ | Error _ -> fail_diagnostics filename diagnostics

let compile_file filename =
  compile_source ~filename (In_channel.read_all filename)

let returns expected = Returns expected
let fails = Fails
let fails_with message = Fails_with message
let string_of_mode = function Il -> "IL" | Sl -> "SL" | Pl -> "PL"

let string_of_values values =
  values
  |> List.map ~f:Value.to_string
  |> String.concat ~sep:", " |> sprintf "[%s]"

let render_error error =
  error |> Interp.error_to_diagnostic |> Diagnostic.Bag.singleton
  |> render_diagnostics

let eval target_state spec mode relation args =
  match mode with
  | Il ->
      Interp.eval_il target_state spec.spec_il relation args spec.filename
      |> Result.map ~f:snd
  | Sl ->
      Interp.eval_sl target_state spec.spec_sl relation args spec.filename
      |> Result.map ~f:snd
  | Pl ->
      Interp.eval_pl target_state spec.spec_pl relation args spec.filename
      |> Result.map ~f:snd

let fail_check ~name ~mode message =
  failwithf "%s (%s): %s" name (string_of_mode mode) message ()

let check_result ~name ~mode expectation result =
  match (expectation, result) with
  | Returns expected, Ok actual ->
      let expected = expected () in
      if not (List.equal Value.eq expected actual) then
        fail_check ~name ~mode
          (sprintf "expected %s but returned %s"
             (string_of_values expected)
             (string_of_values actual))
  | Returns _, Error error ->
      fail_check ~name ~mode ("evaluation failed:\n" ^ render_error error)
  | Fails, Error _ -> ()
  | Fails_with expected, Error error ->
      let actual = render_error error in
      if not (String.is_substring actual ~substring:expected) then
        fail_check ~name ~mode
          (sprintf "expected an error containing %S but got:\n%s" expected
             actual)
  | Fails, Ok actual ->
      fail_check ~name ~mode
        (sprintf "expected an error but returned %s" (string_of_values actual))
  | Fails_with expected, Ok actual ->
      fail_check ~name ~mode
        (sprintf "expected an error containing %S but returned %s" expected
           (string_of_values actual))

let check spec ?(modes = [ Il; Sl; Pl ]) ~name ~relation ~args expectation =
  List.iter modes ~f:(fun mode ->
      Interp.with_target_state
        (module Pure_target)
        (fun target_state ->
          let args = args () in
          eval target_state spec mode relation args
          |> check_result ~name ~mode expectation))
