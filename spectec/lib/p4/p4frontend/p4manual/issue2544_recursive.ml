module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    StructD
      {
        id = "S" $ no_info;
        tparams = [];
        fields =
          [
            ( "x" $ no_info,
              NameT (L.Current ("S" $ no_info) $ no_info) $ no_info,
              [] );
          ];
        annos = [];
      }
    $ no_info;
  ]
