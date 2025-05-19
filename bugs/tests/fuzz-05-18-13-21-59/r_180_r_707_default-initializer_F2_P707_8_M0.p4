 enum bit<8> myEnum {
       One = 1, Two = 2, Three = 3 };
        enum myEnum1 {
         e1,   e2,   e3 };
        header H1 {}
        struct S1 {}
        control C0() {
         apply {
          tuple<int<8>, bool, tuple<string>, myEnum1, myEnum, H1, S1> xx ;
        }
       }
