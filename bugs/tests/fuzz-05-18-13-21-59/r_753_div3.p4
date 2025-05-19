control c(inout bit<8> a) {
         bit<8> b = 3;
         apply {
            a = a / b;
        }
     }
