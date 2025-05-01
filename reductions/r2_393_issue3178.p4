enum bit a { b = 1 } const bit<10> d = (bit<10>)(bit)a.b;
const bit<10> c = 9 | d;
