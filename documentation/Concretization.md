# Concretization (Experimental)

Languages 1~3 in [Intro.md] operate on abstract syntax. However, to actually typecheck or evaluate a program with a SpecTec spec, one must be able to convert a program string into abstract syntax and (optionally) print the abstract syntax as a program string. Although abstract syntax has a rough correspondance with with concrete syntax, the conversion between concrete and abstract syntax is oftentimes non-trivial, as there are different desired properties for parsing grammars and abstract representations. SpecTec-Core provides a handful of experimental features to aid this conversion proccess.

## Concrete Atoms
*Atoms* and *symbolic atoms* aid the spec editor 
