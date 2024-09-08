struct S {
   bit<8> x;
   int<8> y;
}

void func(in S s) {
    return;
}

void impl(in bit<8> x, in int<8> y) {
    return;
}

control c() {
   apply {
       S s = { x = 1, y = 1 };
       func({ x = 1, y = 1 });
       impl(1, 1);
   }
}
