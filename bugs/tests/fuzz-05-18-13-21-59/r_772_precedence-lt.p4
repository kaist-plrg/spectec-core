extern T f<T>(T x);
        extern Object {
       }
        struct data {
           bit<8> f;
       }
        control C(inout data d, inout bit<16> foo, Object o) {
           apply {
              if (4 + d.f < 10) ;
          }
       }
