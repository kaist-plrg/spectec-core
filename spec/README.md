# P4-SpecTec

P4 spec written in SpecTec DSL.

## Compiling the Spec to LaTeX

From the root of the repository, run: `make spec`.
This will generate `spec.pdf` in this directory.

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
* Validity checks for limitations of the target architecture (hardware)

### Auxiliary Files

There are several auxiliary files that are used in the spec.
They define library functions for reusability and readability through display hints.

For example, [0-aux](0-aux.watsup) holds definitions for set and map operations.

### External Syntax

[1a-syntax-el](1a-syntax-el.watsup) defines the surface syntax of P4 language.

### Runtime

[2a-runtime-domain](2a-runtime-domain.watsup) defines the runtime domain of P4 language, particularly various identifiers.

[2b2-runtime-value](2b2-runtime-value.watsup) defines the runtime values of P4 language.

[2c1-runtime-type](2c1-runtime-type.watsup) defines the types of P4 language: types of identifiers, functions, and constructors.
P4 has generics, so the type/function/constructor definition corresponds to the concept of type constructors, or type schemes.
Note that the type specified in SpecTec is more refined than the one in p4cherry, yet they are essentially the same.

For types, [2c3-runtime-type-subst](2c3-runtime-type-subst.watsup) defines type substitution, and [2c5-runtime-type-alpha](2c5-runtime-type-alpha.watsup) defines alpha-equivalence of types.
[2c6-runtime-type-wellformed](2c6-runtime-type-wellformed.watsup) defines the well-formedness checks of types.

### Internal Syntax

[3-syntax-il](3-syntax-il.watsup) defines the internal syntax of P4 language.
The internal syntax is an elaborated version of the surface syntax, annotated with type information that comes post type-checking.

### Typing

[4a1-typing-domain](4a1-typing-domain.watsup) defines the typing domain of P4 language: control flow and compile-time known-ness.
[4a2-typing-context](4a2-typing-context.watsup) defines the typing context of P4 language.
[4a3-typing-tblctx](4a3-typing-tblctx.watsup) defines the typing context of tables, which is a special case of the typing context.

[4b-typing-relation](4b-typing-relation.watsup) defines the typing relations of P4 language.
Relations are declared beforehand because they are mutually recursive.

[4c-typing-static-eval](4c-typing-static-eval.watsup) defines the static evaluation of IL expressions.

[4d1-typing-type](4d1-typing-type.watsup) defines the typing rules of syntactic types.
[4d2-typing-subtyping](4d2-typing-subtyping.watsup) defines the subtyping rules, implicit or explicit.

[4e-typing-expr](4e-typing-expr.watsup) defines the typing rules of expressions.
[4f-typing-stmt](4f-typing-stmt.watsup) defines the typing rules of statements.
[4g-typing-decl](4g-typing-decl.watsup) defines the typing rules of declarations.
[4h-typing-call](4h-typing-call.watsup) defines the typing rules of various invocations, and the calling convention.

## Omissions (for now)

There are several features that are not yet specified in the spec.
Mainly because they intoduce complexity to the spec, and are not essential to the core of P4 language.

* Type inference and don't care types (e.g. `_`)
* Function and method overloading
* Call via argument names and default arguments

## TODOs

An incomplete list of features that are implemented, but not yet specified in DSL.
They are marked with `(TODO)` in the spec.
`(TODO)` marks come along with a note and a reference to the corresponding implementation in p4cherry.

### Syntax

* Add decl case for `typedef` and `type`

### Runtime

* Alpha-equivalence of types and function definitions ([2c5-runtime-type-alpha](2c5-runtime-type-alpha.watsup))
    * Displaying the equivalence symbol in the output, requiring SpecTec extension.
