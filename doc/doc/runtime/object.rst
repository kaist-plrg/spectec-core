.. _runtime-object:

Object
------

Objects are instantiated via a static instantiation phase.
Tables, value sets, and extern objects are stateful.
Others are classified as objects, since they wrap around the stateful objects.

$${syntax: obj}

Store is a map from path to objects.
Paths are fully-qualified names of objects.

$${syntax: sto}
