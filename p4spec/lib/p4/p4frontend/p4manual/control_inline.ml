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
    MatchKindD
      {
        members =
          [ "range" $ no_info; "optional" $ no_info; "selector" $ no_info ];
      }
    $ no_info;
    ConstD
      {
        id = "__v1model_version" $ no_info;
        typ =
          FBitT (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
          $ no_info;
        value =
          NumE { num = (Bigint.of_int_exn 20180101, None) $ no_info } $ no_info;
        annos = [];
      }
    $ no_info;
    StructD
      {
        id = "standard_metadata_t" $ no_info;
        tparams = [];
        fields =
          [
            ( "ingress_port" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 9, None) $ no_info } $ no_info)
              $ no_info,
              [] );
            ( "egress_spec" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 9, None) $ no_info } $ no_info)
              $ no_info,
              [] );
            ( "egress_port" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 9, None) $ no_info } $ no_info)
              $ no_info,
              [] );
            ( "instance_type" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              [] );
            ( "packet_length" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              [] );
            ( "enq_timestamp" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"queueing_metadata.enq_timestamp\"" $ no_info ] )
                $ no_info;
              ] );
            ( "enq_qdepth" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 19, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"queueing_metadata.enq_qdepth\"" $ no_info ] )
                $ no_info;
              ] );
            ( "deq_timedelta" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"queueing_metadata.deq_timedelta\"" $ no_info ] )
                $ no_info;
              ] );
            ( "deq_qdepth" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 19, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"queueing_metadata.deq_qdepth\"" $ no_info ] )
                $ no_info;
              ] );
            ( "ingress_global_timestamp" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 48, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [
                      "\"intrinsic_metadata.ingress_global_timestamp\""
                      $ no_info;
                    ] )
                $ no_info;
              ] );
            ( "egress_global_timestamp" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 48, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [
                      "\"intrinsic_metadata.egress_global_timestamp\"" $ no_info;
                    ] )
                $ no_info;
              ] );
            ( "mcast_grp" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 16, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"intrinsic_metadata.mcast_grp\"" $ no_info ] )
                $ no_info;
              ] );
            ( "egress_rid" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 16, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"intrinsic_metadata.egress_rid\"" $ no_info ] )
                $ no_info;
              ] );
            ( "checksum_error" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 1, None) $ no_info } $ no_info)
              $ no_info,
              [] );
            ("parser_error" $ no_info, ErrT $ no_info, []);
            ( "priority" $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 3, None) $ no_info } $ no_info)
              $ no_info,
              [
                L.TextN
                  ( "alias" $ no_info,
                    [ "\"intrinsic_metadata.priority\"" $ no_info ] )
                $ no_info;
              ] );
          ];
        annos =
          [
            L.EmptyN ("metadata" $ no_info) $ no_info;
            L.TextN ("name" $ no_info, [ "\"standard_metadata\"" $ no_info ])
            $ no_info;
          ];
      }
    $ no_info;
    EnumD
      {
        id = "CounterType" $ no_info;
        members =
          [
            "packets" $ no_info;
            "bytes" $ no_info;
            "packets_and_bytes" $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    EnumD
      {
        id = "MeterType" $ no_info;
        members = [ "packets" $ no_info; "bytes" $ no_info ];
        annos = [];
      }
    $ no_info;
    ExternObjectD
      {
        id = "counter" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM
              {
                id = "counter" $ no_info;
                cparams =
                  [
                    ( "size" $ no_info,
                      L.No $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "type" $ no_info,
                      L.No $ no_info,
                      NameT (L.Current ("CounterType" $ no_info) $ no_info)
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
                id = "count" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params =
                  [
                    ( "index" $ no_info,
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
          ];
        annos = [];
      }
    $ no_info;
    ExternObjectD
      {
        id = "direct_counter" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM
              {
                id = "direct_counter" $ no_info;
                cparams =
                  [
                    ( "type" $ no_info,
                      L.No $ no_info,
                      NameT (L.Current ("CounterType" $ no_info) $ no_info)
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
                id = "count" $ no_info;
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
        id = "meter" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM
              {
                id = "meter" $ no_info;
                cparams =
                  [
                    ( "size" $ no_info,
                      L.No $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "type" $ no_info,
                      L.No $ no_info,
                      NameT (L.Current ("MeterType" $ no_info) $ no_info)
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
                id = "execute_meter" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [ "T" $ no_info ];
                params =
                  [
                    ( "index" $ no_info,
                      L.In $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "result" $ no_info,
                      L.Out $ no_info,
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
    ExternObjectD
      {
        id = "direct_meter" $ no_info;
        tparams = [ "T" $ no_info ];
        mthds =
          [
            ExternConsM
              {
                id = "direct_meter" $ no_info;
                cparams =
                  [
                    ( "type" $ no_info,
                      L.No $ no_info,
                      NameT (L.Current ("MeterType" $ no_info) $ no_info)
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
                id = "read" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params =
                  [
                    ( "result" $ no_info,
                      L.Out $ no_info,
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
    ExternObjectD
      {
        id = "register" $ no_info;
        tparams = [ "T" $ no_info ];
        mthds =
          [
            ExternConsM
              {
                id = "register" $ no_info;
                cparams =
                  [
                    ( "size" $ no_info,
                      L.No $ no_info,
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
                id = "read" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params =
                  [
                    ( "result" $ no_info,
                      L.Out $ no_info,
                      NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "index" $ no_info,
                      L.In $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                  ];
                annos = [ L.EmptyN ("noSideEffects" $ no_info) $ no_info ];
              }
            $ no_info;
            ExternM
              {
                id = "write" $ no_info;
                typ_ret = VoidT $ no_info;
                tparams = [];
                params =
                  [
                    ( "index" $ no_info,
                      L.In $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "value" $ no_info,
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
    ExternObjectD
      {
        id = "action_profile" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM
              {
                id = "action_profile" $ no_info;
                cparams =
                  [
                    ( "size" $ no_info,
                      L.No $ no_info,
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
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "random" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info ];
        params =
          [
            ( "result" $ no_info,
              L.Out $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "lo" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "hi" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "digest" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info ];
        params =
          [
            ( "receiver" $ no_info,
              L.In $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    EnumD
      {
        id = "HashAlgorithm" $ no_info;
        members =
          [
            "crc32" $ no_info;
            "crc32_custom" $ no_info;
            "crc16" $ no_info;
            "crc16_custom" $ no_info;
            "random" $ no_info;
            "identity" $ no_info;
            "csum16" $ no_info;
            "xor16" $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "mark_to_drop" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params = [];
        annos =
          [
            L.TextN
              ( "deprecated" $ no_info,
                [
                  "\"Please use mark_to_drop(standard_metadata) instead.\""
                  $ no_info;
                ] )
            $ no_info;
          ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "mark_to_drop" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ( "standard_metadata" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("standard_metadata_t" $ no_info) $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("pure" $ no_info) $ no_info ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "hash" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "O" $ no_info; "T" $ no_info; "D" $ no_info; "M" $ no_info ];
        params =
          [
            ( "result" $ no_info,
              L.Out $ no_info,
              NameT (L.Current ("O" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "algo" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("HashAlgorithm" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "base" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("D" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "max" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("pure" $ no_info) $ no_info ];
      }
    $ no_info;
    ExternObjectD
      {
        id = "action_selector" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM
              {
                id = "action_selector" $ no_info;
                cparams =
                  [
                    ( "algorithm" $ no_info,
                      L.No $ no_info,
                      NameT (L.Current ("HashAlgorithm" $ no_info) $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "size" $ no_info,
                      L.No $ no_info,
                      FBitT
                        (NumE { num = (Bigint.of_int_exn 32, None) $ no_info }
                        $ no_info)
                      $ no_info,
                      None,
                      [] )
                    $ no_info;
                    ( "outputWidth" $ no_info,
                      L.No $ no_info,
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
          ];
        annos = [];
      }
    $ no_info;
    EnumD
      {
        id = "CloneType" $ no_info;
        members = [ "I2E" $ no_info; "E2E" $ no_info ];
        annos = [];
      }
    $ no_info;
    ExternObjectD
      {
        id = "Checksum16" $ no_info;
        tparams = [];
        mthds =
          [
            ExternConsM
              { id = "Checksum16" $ no_info; cparams = []; annos = [] }
            $ no_info;
            ExternM
              {
                id = "get" $ no_info;
                typ_ret =
                  FBitT
                    (NumE { num = (Bigint.of_int_exn 16, None) $ no_info }
                    $ no_info)
                  $ no_info;
                tparams = [ "D" $ no_info ];
                params =
                  [
                    ( "data" $ no_info,
                      L.In $ no_info,
                      NameT (L.Current ("D" $ no_info) $ no_info) $ no_info,
                      None,
                      [] )
                    $ no_info;
                  ];
                annos = [];
              }
            $ no_info;
          ];
        annos =
          [
            L.TextN
              ( "deprecated" $ no_info,
                [
                  "\"Please use verify_checksum/update_checksum instead.\""
                  $ no_info;
                ] )
            $ no_info;
          ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "verify_checksum" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info; "O" $ no_info ];
        params =
          [
            ("condition" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "checksum" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("O" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "algo" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("HashAlgorithm" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "update_checksum" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info; "O" $ no_info ];
        params =
          [
            ("condition" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "checksum" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("O" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "algo" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("HashAlgorithm" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("pure" $ no_info) $ no_info ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "verify_checksum_with_payload" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info; "O" $ no_info ];
        params =
          [
            ("condition" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "checksum" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("O" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "algo" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("HashAlgorithm" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "update_checksum_with_payload" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info; "O" $ no_info ];
        params =
          [
            ("condition" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "checksum" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("O" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "algo" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("HashAlgorithm" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("noSideEffects" $ no_info) $ no_info ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "clone" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ( "type" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("CloneType" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "session" $ no_info,
              L.In $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "resubmit" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info ];
        params =
          [
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos =
          [
            L.TextN
              ( "deprecated" $ no_info,
                [
                  "\"Please use 'resubmit_preserving_field_list' instead\""
                  $ no_info;
                ] )
            $ no_info;
          ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "resubmit_preserving_field_list" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ( "index" $ no_info,
              L.No $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 8, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "recirculate" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info ];
        params =
          [
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos =
          [
            L.TextN
              ( "deprecated" $ no_info,
                [
                  "\"Please use 'recirculate_preserving_field_list' instead\""
                  $ no_info;
                ] )
            $ no_info;
          ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "recirculate_preserving_field_list" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ( "index" $ no_info,
              L.No $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 8, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "clone3" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info ];
        params =
          [
            ( "type" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("CloneType" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "session" $ no_info,
              L.In $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos =
          [
            L.TextN
              ( "deprecated" $ no_info,
                [
                  "\"Please use 'clone_preserving_field_list' instead\""
                  $ no_info;
                ] )
            $ no_info;
          ];
      }
    $ no_info;
    ExternFuncD
      {
        id = "clone_preserving_field_list" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ( "type" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("CloneType" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "session" $ no_info,
              L.In $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "index" $ no_info,
              L.No $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 8, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "truncate" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ( "length" $ no_info,
              L.In $ no_info,
              FBitT
                (NumE { num = (Bigint.of_int_exn 32, None) $ no_info } $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "assert" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ("check" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "assume" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ("check" $ no_info, L.In $ no_info, BoolT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "log_msg" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [];
        params =
          [
            ("msg" $ no_info, L.No $ no_info, StrT $ no_info, None, [])
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ExternFuncD
      {
        id = "log_msg" $ no_info;
        typ_ret = VoidT $ no_info;
        tparams = [ "T" $ no_info ];
        params =
          [
            ("msg" $ no_info, L.No $ no_info, StrT $ no_info, None, [])
            $ no_info;
            ( "data" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("T" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ParserTypeD
      {
        id = "Parser" $ no_info;
        tparams = [ "H" $ no_info; "M" $ no_info ];
        params =
          [
            ( "b" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("packet_in" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "parsedHdr" $ no_info,
              L.Out $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "standard_metadata" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("standard_metadata_t" $ no_info) $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ControlTypeD
      {
        id = "VerifyChecksum" $ no_info;
        tparams = [ "H" $ no_info; "M" $ no_info ];
        params =
          [
            ( "hdr" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ControlTypeD
      {
        id = "Ingress" $ no_info;
        tparams = [ "H" $ no_info; "M" $ no_info ];
        params =
          [
            ( "hdr" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "standard_metadata" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("standard_metadata_t" $ no_info) $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("pipeline" $ no_info) $ no_info ];
      }
    $ no_info;
    ControlTypeD
      {
        id = "Egress" $ no_info;
        tparams = [ "H" $ no_info; "M" $ no_info ];
        params =
          [
            ( "hdr" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "standard_metadata" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("standard_metadata_t" $ no_info) $ no_info)
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("pipeline" $ no_info) $ no_info ];
      }
    $ no_info;
    ControlTypeD
      {
        id = "ComputeChecksum" $ no_info;
        tparams = [ "H" $ no_info; "M" $ no_info ];
        params =
          [
            ( "hdr" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    ControlTypeD
      {
        id = "Deparser" $ no_info;
        tparams = [ "H" $ no_info ];
        params =
          [
            ( "b" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("packet_out" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "hdr" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [ L.EmptyN ("deparser" $ no_info) $ no_info ];
      }
    $ no_info;
    PackageTypeD
      {
        id = "V1Switch" $ no_info;
        tparams = [ "H" $ no_info; "M" $ no_info ];
        cparams =
          [
            ( "p" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("Parser" $ no_info) $ no_info,
                  [
                    NameT (L.Current ("H" $ no_info) $ no_info) $ no_info;
                    NameT (L.Current ("M" $ no_info) $ no_info) $ no_info;
                  ] )
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "vr" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("VerifyChecksum" $ no_info) $ no_info,
                  [
                    NameT (L.Current ("H" $ no_info) $ no_info) $ no_info;
                    NameT (L.Current ("M" $ no_info) $ no_info) $ no_info;
                  ] )
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "ig" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("Ingress" $ no_info) $ no_info,
                  [
                    NameT (L.Current ("H" $ no_info) $ no_info) $ no_info;
                    NameT (L.Current ("M" $ no_info) $ no_info) $ no_info;
                  ] )
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "eg" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("Egress" $ no_info) $ no_info,
                  [
                    NameT (L.Current ("H" $ no_info) $ no_info) $ no_info;
                    NameT (L.Current ("M" $ no_info) $ no_info) $ no_info;
                  ] )
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "ck" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("ComputeChecksum" $ no_info) $ no_info,
                  [
                    NameT (L.Current ("H" $ no_info) $ no_info) $ no_info;
                    NameT (L.Current ("M" $ no_info) $ no_info) $ no_info;
                  ] )
              $ no_info,
              None,
              [] )
            $ no_info;
            ( "dep" $ no_info,
              L.No $ no_info,
              SpecT
                ( L.Current ("Deparser" $ no_info) $ no_info,
                  [ NameT (L.Current ("H" $ no_info) $ no_info) $ no_info ] )
              $ no_info,
              None,
              [] )
            $ no_info;
          ];
        annos = [];
      }
    $ no_info;
    TypeDefD
      {
        id = "std_m" $ no_info;
        typdef =
          L.Left
            (NameT (L.Current ("standard_metadata_t" $ no_info) $ no_info)
            $ no_info);
        annos = [];
      }
    $ no_info;
    StructD { id = "H" $ no_info; tparams = []; fields = []; annos = [] }
    $ no_info;
    StructD { id = "M" $ no_info; tparams = []; fields = []; annos = [] }
    $ no_info;
    ParserD
      {
        id = "ParserI" $ no_info;
        tparams = [];
        params =
          [
            ( "pk" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("packet_in" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "hdr" $ no_info,
              L.Out $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "smeta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("std_m" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
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
    ControlD
      {
        id = "VC" $ no_info;
        tparams = [];
        params =
          [
            ( "hdr" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        cparams = [];
        locals = [];
        body = ([], []) $ no_info;
        annos = [];
      }
    $ no_info;
    ControlD
      {
        id = "Main" $ no_info;
        tparams = [];
        params =
          [
            ( "hdr" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "smeta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("std_m" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        cparams = [];
        locals = [];
        body =
          ( [
              DeclS
                {
                  decl =
                    InstD
                      {
                        id = "vc" $ no_info;
                        var_inst = L.Current ("VC" $ no_info) $ no_info;
                        targs = [];
                        args = [];
                        init = [];
                        annos = [];
                      }
                    $ no_info;
                }
              $ no_info;
              CallMethodS
                {
                  expr_base =
                    VarE { var = L.Current ("vc" $ no_info) $ no_info }
                    $ no_info;
                  member = "apply" $ no_info;
                  targs = [];
                  args =
                    [
                      L.ExprA
                        (VarE { var = L.Current ("hdr" $ no_info) $ no_info }
                        $ no_info)
                      $ no_info;
                      L.ExprA
                        (VarE { var = L.Current ("meta" $ no_info) $ no_info }
                        $ no_info)
                      $ no_info;
                    ];
                }
              $ no_info;
            ],
            [] )
          $ no_info;
        annos = [];
      }
    $ no_info;
    ControlD
      {
        id = "CC" $ no_info;
        tparams = [];
        params =
          [
            ( "hdr" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "meta" $ no_info,
              L.InOut $ no_info,
              NameT (L.Current ("M" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        cparams = [];
        locals = [];
        body = ([], []) $ no_info;
        annos = [];
      }
    $ no_info;
    ControlD
      {
        id = "DeparserI" $ no_info;
        tparams = [];
        params =
          [
            ( "b" $ no_info,
              L.No $ no_info,
              NameT (L.Current ("packet_out" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
            ( "hdr" $ no_info,
              L.In $ no_info,
              NameT (L.Current ("H" $ no_info) $ no_info) $ no_info,
              None,
              [] )
            $ no_info;
          ];
        cparams = [];
        locals = [];
        body = ([], []) $ no_info;
        annos = [];
      }
    $ no_info;
    InstD
      {
        id = "main" $ no_info;
        var_inst = L.Current ("V1Switch" $ no_info) $ no_info;
        targs = [];
        args =
          [
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("ParserI" $ no_info) $ no_info;
                   targs = [];
                   args = [];
                 }
              $ no_info)
            $ no_info;
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("VC" $ no_info) $ no_info;
                   targs = [];
                   args = [];
                 }
              $ no_info)
            $ no_info;
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("Main" $ no_info) $ no_info;
                   targs = [];
                   args = [];
                 }
              $ no_info)
            $ no_info;
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("Main" $ no_info) $ no_info;
                   targs = [];
                   args = [];
                 }
              $ no_info)
            $ no_info;
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("CC" $ no_info) $ no_info;
                   targs = [];
                   args = [];
                 }
              $ no_info)
            $ no_info;
            L.ExprA
              (InstE
                 {
                   var_inst = L.Current ("DeparserI" $ no_info) $ no_info;
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
