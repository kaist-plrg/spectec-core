.. _runtime-functions:

Function
--------

A P4 program may contain functions and methods for programmable/non-programmable objects.
They are both called functions in this document, if the context is clear.

Most functions carry a visibility set, which is a set of type, variable, and function names that are visible to the function.
Functions are like closures in the sense that they capture the environment in which they are defined.
However, the visibility is captured instead of the environment itself, allowing mutation of the environment external to the function.

${:FMETHOD} is a method of a programmable parser or control block. (which should be "apply")
${:FEXTERNMETHOD} is a method of an external, fixed-function block.
${:FMETHOD} is a state of a finite state machine described by a programmable parser block.
Note that ${:FSTATE} are mutually recursive, and it does not have a visibility set, for it inherits the visibility set of the parser block.
${:FTABLE} is a method of a match-action table. (which should be "apply")

$${syntax: func}

Function environment is a map from function names to functions.

$${syntax: fenv}

Function visibility is a set of function names.

$${syntax: fvis}
