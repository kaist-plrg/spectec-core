module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    ControlD
      {
        id = "c" $ no_info;
        tparams = [];
        params = [];
        cparams = [];
        locals =
          [
            VarD
              {
                id = "a" $ no_info;
                typ =
                  FBitT
                    (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                    $ no_info)
                  $ no_info;
                init = None;
                annos = [];
              }
            $ no_info;
            VarD
              {
                id = "b" $ no_info;
                typ =
                  FBitT
                    (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                    $ no_info)
                  $ no_info;
                init = None;
                annos = [];
              }
            $ no_info;
          ];
        body =
          ( [
              AssignS
                {
                  expr_l =
                    SeqE
                      {
                        exprs =
                          [
                            VarE { var = L.Current ("a" $ no_info) $ no_info }
                            $ no_info;
                            VarE { var = L.Current ("b" $ no_info) $ no_info }
                            $ no_info;
                          ];
                      }
                    $ no_info;
                  expr_r =
                    SeqE
                      {
                        exprs =
                          [
                            NumE
                              { num = (Bigint.of_int_exn 10, None) $ no_info }
                            $ no_info;
                            NumE
                              { num = (Bigint.of_int_exn 20, None) $ no_info }
                            $ no_info;
                          ];
                      }
                    $ no_info;
                }
              $ no_info;
            ],
            [] )
          $ no_info;
        annos = [];
      }
    $ no_info;
    ControlTypeD
      { id = "proto" $ no_info; tparams = []; params = []; annos = [] }
    $ no_info;
    PackageTypeD
      {
        id = "top" $ no_info;
        tparams = [];
        cparams =
          [
            ( "_p" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("proto" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    InstD
      {
        id = "main" $ no_info;
        var_inst = L.Current ("top" $ no_info) $ no_info;
        targs = [];
        args =
          [
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("c" $ no_info) $ no_info;
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
  ]
