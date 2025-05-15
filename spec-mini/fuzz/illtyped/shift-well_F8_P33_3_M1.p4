// Intended pid 33
// Source vid 41
// Depth 4

// Mutation MutateList

/*
From [ (HeaderD "Hdr" [ ("src", (FBitT (NumE (INT +32)))) ]),
  (FuncD "f" (FBitT (NumE (INT +32))) [ ("x" (IN) (FBitT (NumE (INT +32))) None),
  ("hdr" (IN) (NameT "Hdr") None) ] [ (RetS Some((BinE (SHL) (NameE "x") (ExprAccE (NameE "hdr") "src")))) ]) ]
To [ (HeaderD "Hdr" [ ("src", (FBitT (NumE (INT +32)))) ]),
  (HeaderD "Hdr" [ ("src", (FBitT (NumE (INT +32)))) ]),
  (FuncD "f" (FBitT (NumE (INT +32))) [ ("x" (IN) (FBitT (NumE (INT +32))) None),
  ("hdr" (IN) (NameT "Hdr") None) ] [ (RetS Some((BinE (SHL) (NameE "x") (ExprAccE (NameE "hdr") "src")))) ]) ]
*/

header Hdr {
  bit<32> src;

}
header Hdr {
  bit<32> src;

}
bit<32> f(in bit<32> x, in Hdr hdr) {
  return ((x) << (hdr.src));
}

// Covered pids { 33 }
