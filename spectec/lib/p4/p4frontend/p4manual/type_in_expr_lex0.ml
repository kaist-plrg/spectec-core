module L = P4lang.Ast
open P4el.Ast
open P4util.Source

let program =
  [
    ErrD
      {
        members =
          [
            "NoError" $ no_info;
            "PacketTooShort" $ no_info;
            "NoMatch" $ no_info;
            "StackOutOfBounds" $ no_info;
            "HeaderTooShort" $ no_info;
            "ParserTimeout" $ no_info;
            "ParserInvalidArgument" $ no_info;
          ];
      }
    $ no_info;
    ExternObjectD
      {
        id = "packet_in" $ no_info;
        tparams = [];
        mthds =
          [
            ExternM
              {
                id = "extract" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [ "T" $ no_info ];
                params =
                  [
                    ( "hdr" $ no_info,
                      L.Out $ no_info,
                      NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
                      None,
                      [] )
                    $ no_info;
                  ];
                annos = [];
              }
            $ no_info;
            ExternM
              {
                id = "extract" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [ "T" $ no_info ];
                params =
                  [
                    ( "variableSizeHeader" $ no_info,
                      L.Out $ no_info,
                      NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "variableFieldSizeInBits" $ no_info,
                      L.In $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                  ];
                annos = [];
              }
            $ no_info;
            ExternM
              {
                id = "lookahead" $ no_info;
                typ_ret = NameT (L.Current ("T" $ no_info) $ no_info) $ no_info;
                tparams = [ "T" $ no_info ];
                params = [];
                annos = [];
              }
            $ no_info;
            ExternM
              {
                id = "advance" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params =
                  [
                    ( "sizeInBits" $ no_info,
                      L.In $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                  ];
                annos = [];
              }
            $ no_info;
            ExternM
              {
                id = "length" $ no_info;
                typ_ret =
                  FBitT
                    (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                    $ no_info)
                  $ no_info;
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
        id = "packet_out" $ no_info;
        tparams = [];
        mthds =
          [
            ExternM
              {
                id = "emit" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [ "T" $ no_info ];
                params =
                  [
                    ( "hdr" $ no_info,
                      L.In $ no_info,
                      NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
                      None,
                      [] )
                    $ no_info;
                  ];
                annos = [];
              }
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "verify" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ("check" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
            ("toSignal" $ no_info, L.In $ no_info, ErrT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ActionD
      {
        id = "NoAction" $ no_info;
        params = [];
        body = ([], []) $ no_info;
        annos =
          [ L.TextN ("noWarn" $ no_info, [ "\"unused\"" $ no_info ]) $ no_info ];
      }
    $ no_info;
    MatchKindD
      { members = [ "exact" $ no_info; "ternary" $ no_info; "lpm" $ no_info ] }
    $ no_info;
    ExternFuncD
      {
        id = "static_assert" $ no_info;
        typ_ret = BoolT $ no_info;
        tparams = [];
        params =
          [
            ("check" $ no_info, L.No $ no_info, BoolT $ no_info, None, [])
            $ no_info;
            ("message" $ no_info, L.No $ no_info, StrT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "static_assert" $ no_info;
        typ_ret = BoolT $ no_info;
        tparams = [];
        params =
          [
            ("check" $ no_info, L.No $ no_info, BoolT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ControlTypeD
      {
        id = "ctrl" $ no_info;
        tparams = [ "H" $ no_info ];
        params =
          [
            ("val" $ no_info, L.No $ no_info, BoolT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    PackageTypeD
      {
        id = "pkg" $ no_info;
        tparams = [ "H" $ no_info ];
        cparams =
          [
            ( "val" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("ctrl" $ no_info) $ no_info,
                  [ NameT (L.Current ("H" $ no_info) $ no_info) $ no_info ] )
              $ no_info,
              Some
                (InstE
                   {
                     var_inst = L.Current ("ctrl" $ no_info) $ no_info;
                     targs = [];
                     args =
                       [
                         L.ExprA
                           (BinE
                              {
                                binop = L.GtOp $ no_info;
                                expr_l =
                                  VarE
                                    {
                                      var = L.Current ("H" $ no_info) $ no_info;
                                    }
                                  $ no_info;
                                expr_r =
                                  NumE
                                    {
                                      num =
                                        (Bigint.of_int_exn 3, None) $ no_info;
                                    }
                                  $ no_info;
                              }
                           $ no_info)
                         $ no_info;
                       ];
                   }
                $ no_info),
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
  ]
