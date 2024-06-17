.. _runtime-context:

Context
-------

Context is a collection of environment and visibility information.
Visibility restricts the environment to a specific set of mappings.

Instantiation Context
~~~~~~~~~~~~~~~~~~~~~

$${syntax: ictx}

Context
~~~~~~~

A context is divided into three logical layers: global, object, and local.

$${syntax: ctx}

Global and object layers have environment and visibility for typedefs, variables, and functions.

$${syntax+: genv gvis oenv ovis}

Local layer has a typedef environment and a stack of environments, for each block scope.

$${syntax: lenv}
