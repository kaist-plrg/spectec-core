open Error

(* Shorthand for extracting type arguments and values *)

let zero at = function [] -> () | _ -> error at "arity mismatch"
let one at = function [ a ] -> a | _ -> error at "arity mismatch"
let two at = function [ a; b ] -> (a, b) | _ -> error at "arity mismatch"

let three at = function
  | [ a; b; c ] -> (a, b, c)
  | _ -> error at "arity mismatch"
