 header H1 {
       }
        struct S1 {
         H1 h;
         bool b;
       }
        control C(out bit<32> x)() {
         apply {
          S1 sb = {
     b = false, h = {
    3, true, ... }
    , ... };
        }
       }
