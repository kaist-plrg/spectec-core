.. _syntax-statements:

Statements
----------

$${syntax: stmt}

A block is a sequence of statements.

$${syntax: block}

Switch statements may have fallthroughs or default cases.

$${syntax+: case swcase}

Labels are used as names for parser states.

$${syntax: label}

Select statements resemble switch statements but are used for describing parser state machine.

$${syntax+: mtch selcase}

Statments carry signals, which represent non-local control flow such as return and exit.

$${syntax: sig}
