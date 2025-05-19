control c(out bit<16> r)() {
         apply {
          tuple<_, bit<16>> x = {
     10, 12 };
          if (((x) == ({
     10, 12 }
     ))) r = x[1];
        }
       }
