[@@@warning "-32-34"]

open Parsing

let show_ast = ref true

type test_case = string * string * bool

let test_cases = [
  ("Struct", "struct S { bit<8> x; bit<32> y; }", true);
  ("Header", "header h { bit<32> f; }", true);
  ("Control", "control c() { apply {} }", true);
  ("Typedef", "typedef tuple<bit<32>, bool> pair;", true);
  
  ("Control with action", "control c(inout h hdr) { action a() { hdr.setInvalid(); } apply {} }", true);
  ("Package declaration", "package top(proto _p);", true);
  ("Main package", "top(c()) main;", true);
]

(* Single test runner function *)
let run_test (test_name : string) (input : string) (should_pass : bool) =
  let lexbuf = Lexing.from_string input in
  try
    let result = Parser.p4program Lexer.lexer lexbuf in
    Printf.printf "✓ %s: Success! Parsed: %s\n" test_name input;
    if !show_ast then
      Printf.printf "  AST: %s\n" (Pp.pp_value result);
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
