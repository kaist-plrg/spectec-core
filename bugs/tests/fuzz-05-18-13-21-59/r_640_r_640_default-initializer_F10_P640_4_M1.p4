 header H {
    }
     control C(out bit<32> x)() {
      H[2] stack;
      apply {
       stack = ((H[2]) ({
  {
 ... }
 , ... }
  ));
     }
    }
