// Intended pid 860
// Source vid 8
// Depth 2

// Mutation GenFromTyp

/*
From (FBitT (NumE (INT +32)))
To (TupleT [ (MatchKindT) ])
*/

extern E {
  E(tuple<match_kind> x = 0);
  void f(in bit<16> z = 2);
}

// Covered pids { 860 }
