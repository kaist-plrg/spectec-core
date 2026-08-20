type error = ParseError of Parse.error | ElaborateError of Elaborate.error
type spec_source = Parse.spec_source = { filename : string; contents : string }

let parse_files filenames =
  Parse.parse_files filenames |> Result.map_error (fun e -> ParseError e)

let parse_source source =
  Parse.parse_source source |> Result.map_error (fun e -> ParseError e)

let parse_sources sources =
  Parse.parse_sources sources |> Result.map_error (fun e -> ParseError e)

let elaborate spec_el =
  Elaborate.elab_spec spec_el |> Result.map_error (fun e -> ElaborateError e)

let structure spec = Structure.struct_spec spec
let henv_of_el_spec spec = Hints.Henv.of_el_spec spec
let henv_with_il_spec henv spec_il = Hints.Henv.load_il_spec henv spec_il

let annotate ~henv spec_sl =
  spec_sl |> Annotate.Expand.expand_spec |> Annotate.Linearize.linearize_spec
  |> Annotate.annotate_spec henv

let shorten spec = Annotate.Shorthand.shorten_spec spec

let error_to_diagnostics = function
  | ParseError e -> Diag.Bag.singleton (Parse.error_to_diagnostic e)
  | ElaborateError e -> Elaborate.error_to_diagnostics e
