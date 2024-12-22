# P4-SpecTec

P4 spec written in SpecTec DSL.

## Structure

The spec is divided into three parts: syntax, runtime, and typing.

The P4 type system is simple in its core, but has many language features and complex validity checks.
To name a few, P4 has:
* Copy-in/copy-out calling convention
* Compile-time evaluation of expressions
* A mixture of nominal and structural types
* Generic types
* Implicit type conversions (coercions)
* Type inference
* Limitations reflecting the target architecture (hardware)

### Syntax

[1-syntax](1-syntax.watsup) defines the syntax of P4 language.
This roughly corresponds to EL in p4cherry.

### Runtime

[2a-runtime-domain](2a-runtime-domain.watsup) defines the runtime domain of P4 language: identifiers, sets, and maps.

[2b-runtime-value](2b-runtime-value.watsup) and [2c1-runtime-type](2c1-runtime-type.watsup) defines the runtime values and types of P4 language, respectively.

For types, [2c3-runtime-subst](2c3-runtime-subst.watsup) defines type substitution, and [2c4-runtime-alpha](2c4-runtime-alpha.watsup) defines alpha-equivalence of types.
[2c5-runtime-wellformed](2c5-runtime-wellformed.watsup) defines the well-formedness checks of types.

### Typing

[3a-typing-domain](3a-typing-domain.watsup) defines the typing domain of P4 language: typing contexts, control flow, and compile-time known-ness.

[3b-typing-relation](3b-typing-relation.watsup) defines the typing relations of P4 language.
Relations are defined beforehand because they are mutually recursive.

[3c-typing-static-eval](3c-typing-static-eval.watsup) defines the static evaluation of expressions.
Yet, this is incorrect at this stage because while p4cherry implements static evaluation on IL (an elaborated language), SpecTec does not have IL yet.
It currently defines static evaluation on EL, which is not correct.

[3d1-typing-type](3d1-typing-type.watsup) defines the typing rules of syntactic types.
[3d2-typing-subtyping](3d2-typing-subtyping.watsup) defines the subtyping rules, implicit or explicit.

[3e-typing-expr](3e-typing-expr.watsup) defines the typing rules of expressions.
[3f-typing-stmt](3f-typing-stmt.watsup) defines the typing rules of statements.
[3g-typing-decl](3g-typing-decl.watsup) defines the typing rules of declarations.
[3h-typing-call](3h-typing-call.watsup) defines the typing rules of calls, i.e., the calling convention.

## Omissions (for now)

There are several features that are not yet specified in the spec.
Mainly because they intoduce complexity to the spec, and are not essential to the core of P4 language.

* Type inference and don't care types (e.g. `_`)
* Function and method overloading
* Compile-time evaluation
    * Compile-time evaluation should happen on the IL level, but the current spec does not define IL yet.

## TODOs

An incomplete list of features that are implemented, but not yet specified in DSL.

### Syntax

* Add decl case for `typedef` and `type`

### Runtime

* Capture-avoiding type substitution
* Alpha-equivalence of types and function definitions
    * Displaying the equivalence symbol in the output, requiring SpecTec extension.
* Well-formnedness of types
    * Parameter nesting rules for function types 

### Typing

* Subtyping rules
* Expression typing rules
    * Select expression typing
* Declaration typing rules
    * Instantiation typing, when object initializer is present
    * Table typing, which is quite complex (mainly due to detailed validity checks)
* Call typing rules
    * Copy-in/copy-out calling convention
