# Typed Imp: recursion

An optional exercise for early finishers. It extends Typed Imp with two
constructs and asks you to give their static and dynamic semantics. No solution
is provided; the rules you wrote in the main exercise are the model.

The skeleton in `recursion.spectec` is a superset of the main language, so the
function rules are stubbed here as well. Recursion is defined on top of
closures, so begin by importing your completed function rules with `make
import` -- it copies them from `../impty.spectec`, once the main exercise is
finished.

## New constructs

A conditional expression, evaluating to one of two branches according to a
boolean guard:

    e ::= ... | e `?` e `:` e

A recursive function declaration, binding a name that is visible inside the
function body (so the body may call itself):

    c ::= ... | `rec` f `(` t x `)` `->` t `{` e `}`

A non-recursive `fun` evaluates to a closure `CLO`. A `rec` declaration instead
evaluates to a self-referential closure `RECCLO`, which records the function's
own name alongside its parameter, body, and captured environment; applying it
rebinds that name to the closure before evaluating the body.

## Task

Complete `recursion.spectec` by filling the stubs, each tagged in the file:

  - `(functions)`  -- the four function rules. `make import` copies them over
                      from the main exercise; or paste them in by hand.
  - `(recursion)`  -- the conditional and recursion rules: typing and
                      evaluation for `e ? e : e`, and for the `rec` declaration
                      and its application.

## Running

Work from this directory. `./spectecx` is the same binary as the main exercise.

    make import       # bring your finished function rules in from ../impty.spectec
    make test         # whole suite: base + functions + recursion programs
    make test-base    # base + functions (a quick sanity check, before recursion)

    # debug one program at a time -- debug adds the full derivation tree + trace
    ./spectecx impty typecheck -p tests/recursion/rec_sum.imp
    ./spectecx impty eval      -p tests/recursion/rec_sum.imp
    ./spectecx impty debug     -p tests/recursion/rec_sum.imp

The base programs pass immediately; the function programs pass once you import
your rules (`make import`); the recursion programs pass once you complete the
new rules.
