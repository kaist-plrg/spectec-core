module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    HeaderD { id = "H" $ no_info; tparams = []; fields = []; annos = [] }
    $ no_info;
    ConstD
      {
        id = "h" $ no_info;
        typ = NameT (L.Current ("H" $ no_info) $ no_info) $ no_info;
        value = SeqE { exprs = [ InvalidE $ no_info ] } $ no_info;
        annos = [];
      }
    $ no_info;
  ]
