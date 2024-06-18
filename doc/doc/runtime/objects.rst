.. _runtime-objects:

Objects
-------

Objects are explicitly allocated at compile time via the instantiation phase.
${:O_TABLE}, ${:O_VALUESET}, and ${:O_EXTERN} are stateful, i.e., they retain information across invocations.
${:O_PARSER}, ${:O_CONTROL}, and ${:O_PACKAGE} are classified as objects, since they wrap around the stateful objects.
Yet, a table need not be instantiated explicitly, where a declaration is considered as an instantiation.

$${syntax: obj}

Store is a map from path to objects.
Paths are fully-qualified names of objects, the local name of an object prepended with the fully qualified name of its enclosing namespace.

$${syntax: sto}
