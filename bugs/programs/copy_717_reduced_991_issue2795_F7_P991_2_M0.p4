// Intended pid 991
// Source vid 80
// Depth 4

// Mutation MixopGroup

/*
From (ExprA (SeqE [ (NumE (INT +0)),
  (NumE (INT +1)) ]))
To (ExprA (SeqDefaultE [ (NumE (INT +0)),
  (NumE (INT +1)) ]))
*/

extern packet_out {
  void emit<T>(in T hdr);
}
header H {
  bit<32> a;
  bit<32> b;

}
control c(packet_out p)() {

  apply {
    p.emit(((H) ({ 0, 1 })));
    p.emit<H>({ 0, 1, ... });
  }
}

// Covered pids { 717 }
