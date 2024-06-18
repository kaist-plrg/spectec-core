.. _exec_expressions:

Expressions
-----------

Expressions evaluate to a value, and may have side-effects on the evaluating context due to function calls.

Boolean
~~~~~~~

$${rule: Interp_expr/e_bool}

String
~~~~~~

$${rule: Interp_expr/e_str}

Number
~~~~~~

.. todo:: Specify number expression evaluation

Variable
~~~~~~~~

If a variable is prefixed with a dot, it should be looked up in the top-level (global) context.

$${rule+: Interp_expr/e_var-*}

List
~~~~

$${rule: Interp_expr/e_list}

Record
~~~~~~

$${rule: Interp_expr/e_record}

Unary, Binary, and Ternary
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. todo:: Define $unop and $binop

$${rule:
  {Interp_expr/e_un}
  {Interp_expr/e_bin}}
$${rule: {Interp_expr/e_tern-*}}

Cast
~~~~

.. todo:: Define $cast

$${rule: Interp_expr/e_cast}

Mask and Range
~~~~~~~~~~~~~~

.. todo:: Specify mask and range expression evaluation

Accesses
~~~~~~~~

Array Accesses
^^^^^^^^^^^^^^

$${rule: Interp_expr/e_arracc}

Bitstring Accesses
^^^^^^^^^^^^^^^^^^

$${rule: Interp_expr/e_bitacc}

Type Accesses
^^^^^^^^^^^^^

$${rule: Interp_expr/e_typeacc-*}

Error Accesses
^^^^^^^^^^^^^^

$${rule: Interp_expr/e_erracc}

Expression Accesses
^^^^^^^^^^^^^^^^^^^

.. todo:: Define $find_field

.. todo:: Specify header stack access

$${rule: Interp_expr/e_expracc-*}

Call
~~~~

Calls are handled by the rule Interp_call.

$${rule: Interp_expr/e_call}

Interpreting a Sequence of Expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rule Interp_expr is used to interpret a sequence of expressions.

$${rule+: Interp_exprs/*}
