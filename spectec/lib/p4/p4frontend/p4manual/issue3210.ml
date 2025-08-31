module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    ParserD
      {
        id = "p" $ no_info;
        tparams = [];
        params = [];
        cparams = [];
        locals = [];
        states =
          [
            ( "start" $ no_info,
              ( [
                  TransS
                    {
                      expr_label =
                        VarE { var = L.Current ("accept" $ no_info) $ no_info }
                        $ no_info;
                    }
                  $ no_info;
                ],
                [] )
              $ no_info,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    FuncD
      {
        id = "func" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params = [];
        body =
          ( [
              DeclS
                {
                  decl =
                    InstD
                      {
                        id = "t" $ no_info;
                        var_inst = L.Current ("p" $ no_info) $ no_info;
                        targs = [];
                        args = [];
                        init = [];
                        annos = [];
                      }
                    $ no_info;
                }
              $ no_info;
              CallFuncS
                {
                  var_func = L.Current ("t" $ no_info) $ no_info;
                  targs = [];
                  args = [];
                }
              $ no_info;
            ],
            [] )
          $ no_info;
      }
    $ no_info;
  ]
