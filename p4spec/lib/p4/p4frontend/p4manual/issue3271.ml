module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    PackageTypeD
      {
        id = "myp" $ no_info;
        tparams = [];
        cparams =
          [
            ( "a" $ no_info,
              L.No $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 1, None) $ no_info } $ no_info)
              $ no_info,
              None,
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
                        id = "a" $ no_info;
                        var_inst = L.Current ("myp" $ no_info) $ no_info;
                        targs = [];
                        args =
                          [
                            L.ExprA
                              (NumE
                                 {
                                   num =
                                     ( Bigint.of_int_exn 1,
                                       Some (Bigint.of_int_exn 1, false) )
                                     $ no_info;
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
            ],
            [] )
          $ no_info;
      }
    $ no_info;
    ControlD
      {
        id = "c" $ no_info;
        tparams = [];
        params = [];
        cparams = [];
        locals = [];
        body =
          ( [
              CallFuncS
                {
                  var_func = L.Current ("func" $ no_info) $ no_info;
                  targs = [];
                  args = [];
                }
              $ no_info;
            ],
            [] )
          $ no_info;
        annos = [];
      }
    $ no_info;
    ControlTypeD { id = "C" $ no_info; tparams = []; params = []; annos = [] }
    $ no_info;
    PackageTypeD
      {
        id = "top" $ no_info;
        tparams = [];
        cparams =
          [
            ( "_c" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("C" $ no_info) $ no_info) $ no_info,
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
