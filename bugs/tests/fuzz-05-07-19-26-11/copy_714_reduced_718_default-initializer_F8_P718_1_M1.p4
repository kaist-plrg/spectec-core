// Intended pid 718
// Source vid 37
// Depth 4

// Mutation MutateList

/*
From (StructD "S" [] [ ("b", (FBitT (NumE (INT +32)))) ])
To (StructD "S" [] [])
*/

error {
  ParserInvalidArgument
}
extern packet_in {

}
match_kind {
  exact
}
enum E {
  A,
  B,
  C
};
enum bit<32> Z {
A = 2
};
struct S {

}
header H {

}
struct S1 {

}
control C(out bit<32> x)() {

  apply {
    S s1 = { ... };
  }
}

// Covered pids { 714 }
