.. _exec-statements:

Statements
----------

Statements are evaluated under a signal and a context.
Signal represents the current non-local control flow.
Statements evaluate to a signal and an updated context.

Empty
~~~~~

An empty statement does nothing.

$${rule: Interp_stmt/i_empty}

Assignment
~~~~~~~~~~

An assignment evaluates the right-hand side expression and assigns it to the lvalue on the left-hand side.

.. todo:: Define $write, which writes a value to a lvalue in the context

$${rule+: Interp_stmt/i_assign-*}

Conditional
~~~~~~~~~~~

$${rule+: Interp_stmt/i_if-*}

Block
~~~~~

$${rule: Interp_stmt/i_block}

Call
~~~~

Calls are evaluated by the rule Interp_call.
A return from the callee function is handled by the caller and produces a continue signal.
Exit signals are propagated.

$${rule+: Interp_stmt/i_call-*}

State Transition
~~~~~~~~~~~~~~~~

State transitions in a parser state machine is handled by i_trans and i_select.

.. todo:: Specify the semantics of select statement

$${rule+: Interp_stmt/i_trans-*}

Declaration Statement
~~~~~~~~~~~~~~~~~~~~~

A declaration statement introduces a new variable in the context.

$${rule+: Interp_stmt/i_decl-*}

Switch
~~~~~~

.. todo:: Specify the semantics of switch statement

Exit
~~~~

Exit statement immediately terminates the execution of all the blocks currently executing.
Note that exit is not allowed within parser or functions.

$${rule+: Interp_stmt/i_exit-*}

Return
~~~~~~

$${rule+: Interp_stmt/i_return-*}

Interpreting a Block
~~~~~~~~~~~~~~~~~~~~

A block is a sequence of statements.

$${rule+: Interp_stmts/*}
