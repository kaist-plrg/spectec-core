module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    ExternObjectD
      {
        id = "g1" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM { id = "g1" $ no_info; cparams = []; annos = [] }
            $ no_info;
            ExternM
              {
                id = "f" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params = [];
                annos = [];
              }
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternObjectD
      {
        id = "g" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM { id = "g" $ no_info; cparams = []; annos = [] }
            $ no_info;
            ExternAbstractM
              {
                id = "f" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params = [];
                annos = [];
              }
            $ no_info;
            ExternM
              {
                id = "h1" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params = [];
                annos = [];
              }
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternObjectD
      {
        id = "gg" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM { id = "gg" $ no_info; cparams = []; annos = [] }
            $ no_info;
            ExternM
              {
                id = "f" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params = [];
                annos = [];
              }
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    PackageTypeD
      {
        id = "myp" $ no_info;
        tparams = [];
        cparams =
          [
            ( "tg" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("g1" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "tgg" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("gg" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    FuncD
      {
        id = "h" $ no_info;
        typ_ret = BoolT $ no_info;
        tparams = [];
        params = [];
        body =
          ( [
              DeclS
                {
                  decl =
                    InstD
                      {
                        id = "ty" $ no_info;
                        var_inst = L.Current ("myp" $ no_info) $ no_info;
                        targs = [];
                        args =
                          [
                            L.ExprA
                              (InstE
                                 {
                                   var_inst =
                                     L.Current ("g1" $ no_info) $ no_info;
                                   targs = [];
                                   args = [];
                                 }
                              $ no_info)
                            $ no_info;
                            L.ExprA
                              (InstE
                                 {
                                   var_inst =
                                     L.Current ("gg" $ no_info) $ no_info;
                                   targs = [];
                                   args = [];
                                 }
                              $ no_info)
                            $ no_info;
                          ];
                        init = [];
                        annos = [];
                      }
                    $ no_info;
                }
              $ no_info;
              DeclS
                {
                  decl =
                    InstD
                      {
                        id = "t" $ no_info;
                        var_inst = L.Current ("g" $ no_info) $ no_info;
                        targs = [];
                        args = [];
                        init =
                          [
                            InstD
                              {
                                id = "ty" $ no_info;
                                var_inst = L.Current ("myp" $ no_info) $ no_info;
                                targs = [];
                                args =
                                  [
                                    L.ExprA
                                      (InstE
                                         {
                                           var_inst =
                                             L.Current ("g1" $ no_info)
                                             $ no_info;
                                           targs = [];
                                           args = [];
                                         }
                                      $ no_info)
                                    $ no_info;
                                    L.ExprA
                                      (InstE
                                         {
                                           var_inst =
                                             L.Current ("gg" $ no_info)
                                             $ no_info;
                                           targs = [];
                                           args = [];
                                         }
                                      $ no_info)
                                    $ no_info;
                                  ];
                                init = [];
                                annos = [];
                              }
                            $ no_info;
                            InstD
                              {
                                id = "f1" $ no_info;
                                var_inst = L.Current ("gg" $ no_info) $ no_info;
                                targs = [];
                                args = [];
                                init = [];
                                annos = [];
                              }
                            $ no_info;
                            FuncD
                              {
                                id = "f" $ no_info;
                                typ_ret = VoidT $ no_info;
                                tparams = [];
                                params = [];
                                body =
                                  ( [
                                      CallMethodS
                                        {
                                          expr_base =
                                            VarE
                                              {
                                                var =
                                                  L.Current ("f1" $ no_info)
                                                  $ no_info;
                                              }
                                            $ no_info;
                                          member = "f" $ no_info;
                                          targs = [];
                                          args = [];
                                        }
                                      $ no_info;
                                      CallMethodS
                                        {
                                          expr_base =
                                            VarE
                                              {
                                                var =
                                                  L.Current ("this" $ no_info)
                                                  $ no_info;
                                              }
                                            $ no_info;
                                          member = "h1" $ no_info;
                                          targs = [];
                                          args = [];
                                        }
                                      $ no_info;
                                      CallFuncS
                                        {
                                          var_func =
                                            L.Current ("h" $ no_info) $ no_info;
                                          targs = [];
                                          args = [];
                                        }
                                      $ no_info;
                                    ],
                                    [] )
                                  $ no_info;
                              }
                            $ no_info;
                          ];
                        annos = [];
                      }
                    $ no_info;
                }
              $ no_info;
              CallMethodS
                {
                  expr_base =
                    VarE { var = L.Current ("t" $ no_info) $ no_info } $ no_info;
                  member = "f" $ no_info;
                  targs = [];
                  args = [];
                }
              $ no_info;
              RetS { expr_ret = Some (BoolE { boolean = true } $ no_info) }
              $ no_info;
            ],
            [] )
          $ no_info;
      }
    $ no_info;
  ]
