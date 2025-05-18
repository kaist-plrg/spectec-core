open P4el.Ast
open P4util.Source

let program =
  [
    ExternObjectD
      {
        id = "Z" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM { id = "m" $ no_info; cparams = []; annos = [] }
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
  ]
