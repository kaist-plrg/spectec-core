open P4el.Ast
open P4util.Source

let program =
  [
    StructD
      {
        id = "s1" $ no_info;
        tparams = [];
        fields = [ ("field1" $ no_info, AnyT $ no_info, []) ];
        annos = [];
      }
    $ no_info;
  ]
