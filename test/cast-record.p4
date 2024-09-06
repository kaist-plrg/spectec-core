struct S {
   bit<8> x;
   int<8> y;
}

void func(in S s) {
    return;
}

control c() {
   apply {
       S s = { x = 8w1, y = 8s1 };
       func({ x = 8w1, y = 8s1 });
   }
}
