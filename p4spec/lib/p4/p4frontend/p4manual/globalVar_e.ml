open P4el.Ast
open P4util.Source

let program =
  [
    VarD
      {
        id = "x" $ no_info;
        typ =
          FBitT (NumE { num = (Bigint.of_int_exn 1, None) $ no_info } $ no_info)
          $ no_info;
        init = None;
        annos = [];
      }
    $ no_info;
  ]
