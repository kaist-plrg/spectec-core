// Intended pid 639
// Source vid 107
// Depth 3

// Mutation GenFromTyp

/*
From (CastE (NameT (CURRENT "H")) (InvalidE))
To (SeqDefaultE [])
*/

error {
  ParserInvalidArgument
}
extern packet_in {
  void extract<T>(out T variableSizeHeader, in bit<32> variableFieldSizeInBits);
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
header I {

}
header_union HU {

}
header H1 {

}
struct S1 {

}
control C(out bit<32> x)() {
  H[2] stack;
  apply {
    stack = ((H[2]) ({ { ... }, ... }));
  }
}

// Covered pids { 639, 646 }
