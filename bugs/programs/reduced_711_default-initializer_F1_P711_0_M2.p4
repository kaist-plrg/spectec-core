error {
    ParserInvalidArgument }
  header_union HU {
  }
  struct S1 {
  }
  control C(out bit<32> x)() {
    HU[0] hustack;
    apply {
     hustack = { ... };
   }
  }
