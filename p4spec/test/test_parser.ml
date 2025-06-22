(* Suppress warnings for unused values and types in test files *)
[@@@warning "-32-34"]

open Parsing

let show_ast = ref true

type test_case = string * string * bool

let test_cases = [
  (* Basic literals and expressions *)
  ("Simple number", "42;", true);
  ("Boolean true", "true;", true);
  ("Boolean false", "false;", true);
  ("Hex number", "0xFF;", true);
  ("Binary number", "0b1010;", true);
  ("Octal number", "0o77;", true);
  ("Width-specified number", "8w42;", true);
  ("Signed width number", "8s42;", true);
  ("Parenthesized expression", "(123);", true);
  ("Empty program", "", true);
  
  (* Real P4 programs - simple cases *)
  ("Version struct", "struct Version { bit<8> major; bit<8> minor; }", true);
  ("Version const", "const .Version version = { 8w0, 8w1 };", true);
  ("Simple header", "header h { bit<32> f; }", true);
  ("Simple control", "control c() { apply {} }", true);
  ("Tuple typedef", "typedef tuple<bit<32>, bool> pair;", true);
  ("Tuple struct", "struct S { bit<32> f; bool s; }", true);
  ("Tuple assignment", "pair x = { 10, false };", true);
  
  (* Real P4 programs - more complex *)
  ("Control with action", "control c(inout h hdr) { action a() { hdr.setInvalid(); } apply {} }", true);
  ("Package declaration", "package top(proto _p);", true);
  ("Control with tuple", "control c() { pair x = { 10, false }; tuple<bit<32>, bool> y; apply { y = x; } }", true);
  ("Main package", "top(c()) main;", true);
  
  (* Expected failures - syntax errors *)
  ("Invalid syntax", "42 43;", false);
  ("Incomplete expression", "42 +", false);
  ("Wrong cast", "const bool b = (bool)3;", false);
  ("Invalid width", "const bool c = (bool)2s0;", false);
  ("Invalid width format", "const bool d = (bool)10w0;", false);
  
  (* Real error cases from p4c/testdata *)
  ("Underscore error", "const bit<_> x = 0;", false);
  ("Width error", "const bit<0> x = 0;", false);
  ("Template error", "const bit<template> x = 0;", false);
  ("Type params error", "const bit<type> x = 0;", false);
  ("String error", "const string s = 42;", false);
  ("Struct error", "struct S { bit<32> f; } struct S { bool b; }", false);
]

(* Single test runner function *)
let run_test (test_name : string) (input : string) (should_pass : bool) =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parser.p4program Lexer.lexer lexbuf in
    Printf.printf "✓ %s: Success! Parsed: %s\n" test_name input;
    if !show_ast then
      Printf.printf "  AST: %s\n" (Il.Print.string_of_value result);
    if should_pass then (
      Printf.printf "   Expected: PASS, Got: PASS ✓\n\n";
      true
    ) else (
      Printf.printf "   Expected: FAIL, Got: PASS ✗\n\n";
      false
    )
  with
  | Parser.Error ->
      Printf.printf "✗ %s: Parse error at position %d for input: %s\n" 
        test_name (Lexing.lexeme_start lexbuf) input;
      if should_pass then (
        Printf.printf "   Expected: PASS, Got: FAIL ✗\n\n";
        false
      ) else (
        Printf.printf "   Expected: FAIL, Got: FAIL ✓\n\n";
        true
      )
  | Lexer.Error msg ->
      Printf.printf "✗ %s: Lexer error: %s for input: %s\n" test_name msg input;
      if should_pass then (
        Printf.printf "   Expected: PASS, Got: FAIL ✗\n\n";
        false
      ) else (
        Printf.printf "   Expected: FAIL, Got: FAIL ✓\n\n";
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
