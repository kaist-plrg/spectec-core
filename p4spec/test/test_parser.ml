[@@@warning "-32-34"]

open Parsing

let show_ast = ref true

type test_case = string * string * bool

let test_cases = [
  (* Basic type declarations - building on working examples *)
  ("Simple struct", "struct S { bit<8> x; bit<32> y; }", true);
  ("Struct with bool field", "struct S { bool valid; bit<16> data; }", true);
  ("Struct with multiple fields", "struct Packet { bit<32> srcAddr; bit<32> dstAddr; bit<16> length; }", true);
  ("Empty struct", "struct Empty { }", true);
  
  ("Simple header", "header h { bit<32> f; }", true);
  ("Header with multiple fields", "header Ethernet { bit<48> dstAddr; bit<48> srcAddr; bit<16> etherType; }", true);
  ("Header with bool field", "header IPv4 { bit<4> version; bit<4> ihl; bit<8> tos; bit<16> totalLength; }", true);
  
  (* Control blocks - building on working example *)
  ("Simple control", "control c() { apply {} }", true);
  ("Control with parameters", "control c(inout h hdr) { apply {} }", true);
  ("Control with multiple parameters", "control c(in h hdr1, inout h hdr2, out h hdr3) { apply {} }", true);
  ("Control with empty body", "control c() { }", true);
  
  ("Control with action", "control c(inout h hdr) { action a() { hdr.setInvalid(); } apply {} }", true);
  ("Control with multiple actions", "control c(inout h hdr) { action a1() { hdr.setInvalid(); } action a2() { } apply {} }", true);
  
  (* Type definitions *)
  ("Simple typedef", "typedef tuple<bit<32>, bool> pair;", true);
  
  (* Package declarations *)
  ("Simple package", "package top(proto _p);", true);
  ("Package with multiple parameters", "package top(proto _p1, proto _p2);", true);
  ("Package with type parameters", "package top<H>(proto _p);", true);
  
  (* Main package instantiations *)
  ("Simple main", "package top(proto _p); top(c()) main;", true);
  ("Main with multiple arguments", "top(c1(), c2()) main;", true);
  ("Main with type arguments", "top<MyHeader>(c()) main;", true);
  
  (* Parser declarations *)
  ("Simple parser", "parser p() { return ingress; }", true);
  ("Parser with parameters", "parser p(inout h hdr) { return ingress; }", true);
  ("Parser with state", "parser p() { state start { return ingress; } }", true);
  ("Parser with multiple states", "parser p() { state start { transition parse_ethernet; } state parse_ethernet { return ingress; } }", true);
  
  (* Table declarations *)
  ("Simple table", "table t { key = { } actions = { } }", true);
  ("Table with key", "table t { key = { h.f: exact; } actions = { } }", true);
  ("Table with actions", "table t { key = { } actions = { a; } }", true);
  ("Table with key and actions", "table t { key = { h.f: exact; } actions = { a; } }", true);
  ("Table with default action", "table t { key = { } actions = { a; } default_action = a(); }", true);
  
  (* Action declarations *)
  ("Simple action", "action a() { }", true);
  ("Action with parameters", "action a(inout h hdr) { }", true);
  ("Action with body", "action a() { hdr.setInvalid(); }", true);
  ("Action with multiple statements", "action a() { hdr.setInvalid(); hdr.f = 0; }", true);
  
  (* Variable declarations *)
  ("Simple variable", "bit<8> x;", true);
  ("Variable with initialization", "bit<8> x = 0;", true);
  ("Multiple variables", "bit<8> x; bit<16> y;", true);
  
  (* Expression statements *)
  ("Assignment", "x = 1;", true);
  ("Field assignment", "hdr.f = 0;", true);
  ("Method call", "hdr.setInvalid();", true);
  
  (* Complex nested structures *)
  ("Struct in struct", "struct Outer { struct Inner { bit<8> x; } inner; }", true);
  ("Header in struct", "struct Packet { header Ethernet eth; header IPv4 ip; }", true);
  ("Control in control", "control outer() { control inner() { apply {} } apply {} }", true);
  
  (* Edge cases and boundary conditions *)
  ("Very long field name", "struct S { bit<8> very_long_field_name_that_tests_boundaries; }", true);
  ("Numeric literals", "bit<8> x = 255;", true);
  ("Zero width", "bit<0> x;", true);
  ("Large width", "bit<1024> x;", true);
  
  (* Comments and whitespace handling *)
  ("With comments", "/* comment */ struct S { bit<8> x; }", true);
  ("Multiple comments", "/* start */ struct S { /* field */ bit<8> x; /* end */ }", true);
  ("Extra whitespace", "  struct  S  {  bit<8>  x;  }  ", true);
]

(* Single test runner function *)
let run_test (test_name : string) (input : string) (should_pass : bool) =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parser.p4program Lexer.lexer lexbuf in
    Printf.printf "✓ %s:  Success!\n    Parsed: %s\n" test_name input;
    if !show_ast then
      Format.asprintf "    AST: %a\n" Pp.pp_value result |> print_endline;
    if should_pass then (
      Printf.printf "Expected: PASS, Got: PASS ✓\n\n";
      true
    ) else (
      Printf.printf "Expected: FAIL, Got: PASS ✗\n\n";
      false
    )
  with
  | Parser.Error ->
      Printf.printf "✗ %s: \nParse error :\n0 | %s\n- | %s^\n" 
        test_name input (List.init (Lexing.lexeme_start lexbuf) (fun _ -> " ") |> String.concat "");
      if should_pass then (
        Printf.printf "Expected: PASS, Got: FAIL\n\n";
        false
      ) else (
        Printf.printf "Expected: FAIL, Got: FAIL\n\n";
        true
      )
  | Lexer.Error msg ->
      Printf.printf "✗ %s: Lexer error: %s for input: %s\n" test_name msg input;
      if should_pass then (
        Printf.printf "Expected: PASS, Got: FAIL\n\n";
        false
      ) else (
        Printf.printf "Expected: FAIL, Got: FAIL\n\n";
        true
      )

(* Run all tests *)
let run_all_tests () =
  Printf.printf "Testing parser\n";
  Printf.printf "=============================================================\n";
  Printf.printf "AST Display: %s\n\n" (if !show_ast then "ON" else "OFF");
  
  let total = List.length test_cases in
  let results = List.map (fun (name, input, should_pass) -> 
    run_test name input should_pass
  ) test_cases in
  let passed = List.fold_left (fun acc passed -> if passed then acc + 1 else acc) 0 results in
  
  Printf.printf "=== TEST SUMMARY ===\n";
  Printf.printf "Passed: %d/%d tests\n" passed total;
  if passed = total then
    Printf.printf "🎉 All tests passed!\n"
  else
    Printf.printf "❌ %d test(s) failed\n" (total - passed);
  
  passed = total

let () =
  show_ast := true;
  
  let all_passed = run_all_tests () in
  
  if all_passed then
    Printf.printf "\nCore functionality working! P4-to-Il.Ast parser handles real-world P4 programs.\n"
  else
    Printf.printf "\nSome tests failed. Please check the parser implementation.\n"
