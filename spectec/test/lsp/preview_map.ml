open Common.Source
module Il = Lang.Il
module Sl = Lang.Sl
module Pl = Lang.Pl
module Map = Spectec_lsp.Preview_map

let region line =
  let left = { file = "mapping.spectec"; line; column = 0 } in
  { left; right = { left with column = 1 } }

let id = "test" $ region 1
let exp = Il.BoolE true $$ (region 1, Il.BoolT)
let annotated_exp = Pl.Annot.bare (Pl.BoolE true $$ (region 1, Il.BoolT))
let sl line node = node $ region line
let pl line node = Pl.Annot.bare (node $ region line)
let leaf line = sl line (Sl.ResultI [])
let pleaf line = pl line (Pl.ResultI [])
let phantom = Some (1, [])

let check name expected mappings =
  let actual = List.map (fun (_, region) -> region.left.line) mappings in
  if actual <> expected then
    failwith
      (Printf.sprintf "%s: expected [%s], got [%s]" name
         (String.concat "; " (List.map string_of_int expected))
         (String.concat "; " (List.map string_of_int actual)))

let sl_def children tail = Sl.DecD (id, [], [], children, tail) $ region 1

let pl_def children tail =
  Pl.Annot.bare (Pl.DecD (id, [], [], children, tail) $ region 1)

let () =
  let def = sl_def [ leaf 1; leaf 2 ] None in
  let text = Sl.Print.string_of_def def in
  let mappings = Map.sl ~text def in
  assert (mappings = [ (3, region 1); (5, region 2) ]);
  assert (Map.sl ~text:(text ^ "\n1. unexpected") def = []);
  let shifted =
    String.split_on_char '\n' text
    |> List.mapi (fun i line -> if i = 3 then " " ^ line else line)
    |> String.concat "\n"
  in
  assert (Map.sl ~text:shifted def = []);
  let changed =
    String.split_on_char '\n' text
    |> List.mapi (fun i line -> if i = 3 then "1. Changed heading" else line)
    |> String.concat "\n"
  in
  assert (Map.sl ~text:changed def = []);
  assert (Sl.Print.string_of_def def = text);
  let unknown = { (leaf 3) with at = no_region } in
  let def = sl_def [ leaf 1; unknown; leaf 2 ] None in
  check "SL unknown region" [ 1; 2 ]
    (Map.sl ~text:(Sl.Print.string_of_def def) def);
  let nested =
    sl 10
      (Sl.DebugI
         ( exp,
           sl 11
             (Sl.LetI
                ( exp,
                  exp,
                  [],
                  [
                    sl 12
                      (Sl.RelI
                         {
                           call = { relid = id; notexp = [] };
                           iterexps = [];
                           block =
                             [
                               sl 13
                                 (Sl.IfI
                                    ( exp,
                                      [],
                                      [
                                        sl 14
                                          (Sl.CaseI
                                             ( exp,
                                               [
                                                 (Sl.BoolG true, [ leaf 15 ]);
                                                 (Sl.BoolG false, []);
                                               ],
                                               None ));
                                      ],
                                      phantom ));
                             ];
                         });
                  ] )) ))
  in
  let def =
    sl_def
      [
        nested;
        sl 16 (Sl.OtherwiseI (leaf 17));
        sl 19
          (Sl.RelAssertI
             {
               call = { relid = id; notexp = [] };
               expect = true;
               iterexps = [];
               block = [ leaf 20 ];
               phantom;
             });
        sl 21 (Sl.ReturnI exp);
      ]
      (Some [ leaf 18 ])
  in
  check "SL nested continuations"
    [ 10; 11; 12; 13; 14; 15; 15; 13; 16; 17; 19; 20; 19; 21; 18; 18 ]
    (Map.sl ~text:(Sl.Print.string_of_def def) def);
  let e = annotated_exp in
  let nested =
    pl 10
      (Pl.TryI
         [
           [
             pl 11
               (Pl.CheckLetI
                  ( e,
                    e,
                    [
                      pl 12
                        (Pl.OptionGetI
                           (e, e, [ pl 13 (Pl.OtherwiseI (pleaf 14)) ]));
                    ] ));
           ];
           [
             pl 15
               (Pl.IfI
                  ( e,
                    [],
                    [
                      pl 16
                        (Pl.CaseI
                           ( e,
                             [
                               (Pl.BoolG true, [ pleaf 17 ]);
                               (Pl.BoolG false, []);
                             ],
                             phantom ));
                    ],
                    phantom ));
           ];
         ])
  in
  let def =
    pl_def
      [
        nested;
        pl 18
          (Pl.RelAssertI
             {
               call = { relid = id; notexp = [] };
               expect = true;
               iterexps = [];
               block = [ pleaf 19 ];
               phantom;
             });
        pl 20 (Pl.RelI { call = { relid = id; notexp = [] }; iterexps = [] });
        pl 21 (Pl.LetI (e, e, []));
        pl 22 (Pl.ResultI [ e ]);
        pl 23 (Pl.ReturnI e);
        pl 24 (Pl.DebugI e);
        pl 25 (Pl.DestructI ([], e));
      ]
      (Some [ pleaf 26 ])
  in
  let text = Pl.Print.string_of_def def in
  check "PL nested blocks"
    [
      10;
      11;
      12;
      13;
      14;
      15;
      16;
      17;
      17;
      16;
      15;
      18;
      19;
      18;
      20;
      21;
      22;
      23;
      24;
      25;
      26;
      26;
    ]
    (Map.pl ~text def);
  assert (Map.pl ~text:(text ^ "\n1. unexpected") def = []);
  assert (Pl.Print.string_of_def def = text)
