# Typed Imp: recursion

An optional exercise for early finishers. It extends Typed Imp with two
constructs and asks you to give their static and dynamic semantics. No solution
is provided; the rules you wrote in the main exercise are the model.

The skeleton in `recursion.spectec` is a superset of the main language, so the
function rules are stubbed here as well. Recursion is defined on top of
closures, so begin by carrying over your completed function rules.

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

  - `(Part 2)`     -- the four function rules. Paste in your answers from the
                      main exercise (`../impty.spectec`).
  - `(recursion)`  -- the conditional and recursion rules: typing and
                      evaluation for `e ? e : e`, and for the `rec` declaration
                      and its application.

## Running

Work from this directory. `./spectecx` is the same binary as the main exercise.

    make test-rec     # whole suite: base + functions + recursion programs
    make test-base    # base programs only (a quick sanity check)

    # debug one program at a time
    ./spectecx impty typecheck -p tests/recursion/rec_sum.imp
    ./spectecx impty eval      -p tests/recursion/rec_sum.imp

The base programs pass immediately; the function programs pass once you carry
your rules over; the recursion programs pass once you complete the new rules.
