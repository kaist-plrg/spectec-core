typedef bit<64> a;
action b(inout a c) { c [64 - 1:48] = c [64 - 1:48]; }
