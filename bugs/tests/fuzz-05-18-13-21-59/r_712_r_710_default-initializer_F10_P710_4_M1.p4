header H {
      }
       control C(out bit<32> x)() {
        H[6s0] stack;
        apply {
         H h0 = ...;
         stack = {
    h0, ... };
       }
      }
