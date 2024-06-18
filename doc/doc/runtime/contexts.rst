.. _runtime-contexts:

Contexts
--------

Context is a collection of environment and visibility information, to be used in the instantiation phase and the interpreter.
Visibility restricts the environment to a specific set of mappings.

Context
~~~~~~~

A context in P4 is divided into three logical layers: global, object, and local.

$${syntax: ctx}

Global and object layers have environment and visibility for typedefs, variables, and functions.

$${syntax+: genv gvis oenv ovis}

Local layer has a typedef environment and a stack of environments, for each block scope.
The topmost environment in the stack is the most recent environment.
Local layer does not have a function environment, as function declarations are not allowed inside local blocks.

$${syntax: lenv}

Instantiation Context
~~~~~~~~~~~~~~~~~~~~~

Instantiation context is a subset of the context, used in the instantiation phase.
It omits the local layer, as local blocks are not instantiated.

.. todo:: p4cherry currently does not consider direct application

$${syntax: ictx}
