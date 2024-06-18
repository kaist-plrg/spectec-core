.. _exec-calls:

Calls
-----

Function calls and method calls may both appear as expressions or statements in P4.

$${rule+: Interp_call}

Finding the Callee Function and Determining the Callee Context
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first thing to do is to identify the callee function or method.

Function Calls
^^^^^^^^^^^^^^

A function identifier may be prefixed with a dot, in which the function should be found in the toplevel (global) context.
A function may be found in the object context, in case of an action call.
Rule Interp_inter_call is used for calls that go beyond the caller's object boundary.
In such case, the callee's context first inherits the caller's global context. Then, it will be restricted by the global visibility of the callee function.
On the other hand, rule Interp_intra_call is used for calls that stay within the caller's object boundary.

$${rule: Interp_fcall/top}
$${rule: Interp_fcall/bare-inter}
$${rule: Interp_fcall/bare-intra}

Method Calls
^^^^^^^^^^^^

All method calls except "apply" on a table are inter-object calls.
For inter-object calls, the callee's context inherits the caller's global context and takes the callee object's global visibility and object context.

.. todo:: Specify rules for header/stack built-in calls

$${rule: Interp_mcall/object-extern}
$${rule: Interp_mcall/object-parser}
$${rule: Interp_mcall/object-control}
$${rule: Interp_mcall/object-table}

Restricting the Callee Context Visibility and Passing Control to the Callee
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, calls are classified as either inter-object or intra-object calls.

Calling Conventions
^^^^^^^^^^^^^^^^^^^

P4 adopts a copy-in/copy-out calling convention.
Note that p4cherry defines two variants of copy-in.
If a call is a "apply" method call on a parser or control object, the arguments are copied into the callee's object layer. (This is because the "apply" method of a parser and control scopes over the entire object.)
Otherwise, the arguments are copied into the callee's local layer.

${:copyin_loc'} copies in a single parameter-value pair.

$${definition: copyin_loc'}

${:copyin_loc} copies in a list of parameter-value pairs.

$${definition: copyin_loc}

${:copyin_obj'} and ${:copyin_obj} are defined similarly.

$${definition: copyin_obj'}
$${definition: copyin_obj}

Relation ${:Copyouts} defines the copy-out operation.

$${rule+: Copyout/*}
$${rule+: Copyouts/*}

.. todo:: Define $copyin_loc, $copyin_obj, and $copyout

Intra-object Calls
^^^^^^^^^^^^^^^^^^

For intra-object calls, after evaluating the callee's body and copy-out, the caller context inherits the callee's object context to take the mutations of object-local variables into account.

$${rule: Interp_intra_call/action}

Inter-object Calls
^^^^^^^^^^^^^^^^^^

Since all globals are immutable in P4, the caller does not need to inherit the callee's global context after callee evaluation.

$${rule: Interp_inter_call/extern}
$${rule: Interp_inter_call/extern_method}
$${rule: Interp_inter_call/method}
$${rule: Interp_inter_call/action}
