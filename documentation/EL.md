# SpecTec EL

SpecTec EL(External Language), also known as SpecTec DSL, is a language designed to describe the syntax and semantics of other programming languages. It is modeled after *mathematical inference rules* which are used in programming language textbooks and papers.

In the SpecTec-style BNF grammars shown in this document, we use `x?`, `x*`, `x+` to describe optional, sequences and non-empty sequences of symbols, respectively. We also use `"s"` to describe concrete syntax strings.
Furthermore, a bit less standard, we write `x*sep` or `x+sep` to describe respective sequences of `x`'s that are separated by `sep`.
Note that `x+`, `"s"`, `x*sep` and `x+sep` are not part of the actual SpecTec grammar.

## Spec
A SpecTec spec is a list of definitions.
```spectec
syntax spec = definition*
```

## Definitions
```spectec
syntax definition =
  | syntaxDeclaration
  | syntaxDefinition
  | variableDeclaration
  | relationDeclaration
  | ruleDefinition
  | functionDeclaration
  | functionDefinition
```

### Syntax Declaration
```spectec
syntax syntaxDeclaration =
  SYNTAX syntaxId+","
```

### Syntax Definitions
```spectec
syntax syntaxDefinition =
  SYNTAX varId "=" defType
```

### Variable Declarations
```spectec
syntax variableDeclaration =
  VAR varId ":" plainType
```

### Relation Declaration
```spectec
syntax relationDeclaration =
  RELATION relationId ":" notationType
```

### Rule Definition
```spectec
syntax ruleDefinition =
  RULE relationId "/" ruleId ":" expression ("--" premise)*
```
SpecTec rules define a single path of a relation. Thus, the *input* portion of `expression` evaluates to the *output* portion only when all of the premises evaluate to `true`.

### Function Declaration
```spectec
syntax functionDeclaration =
  DEC "$" functionId typeParams? params? ":" plainType
```

### Function Definition
```spectec
syntax funtionDefinition =
  DEF "$" functionId typeParams? params? "=" expression ("--" premise)*
```

## Types

### Plain Types
```spectec
syntax plainType =
  | primitiveType
  | "(" plainType+"," ")" ;; tupleType
  | plainType "*" ;; iteratedType
```

Plain Types are recursively defined as either primitive types, or plain types nested within tuples, parentheses, or iterators.

#### Primitive Types
```spectec
syntax primitiveType =
  | BOOL | NAT | INT | TEXT
```

There are four primitive types in SpecTec. `bool` denotes booleans, `nat` denotes 0 or positive integers, `int` denotes all signed integers, and `text` denotes general strings.

## Type Definitions
```spectec
syntax typeDef =
  | plainTypeDef
  | structTypeDef
  | variantTypeDef
```


### Struct Type Definitions
```spectec
syntax structTypeDef =
  "{" fieldType+"," "}"
```
SpecTec has struct types, similar to record types in OCaml.

#### Struct Field Types
```spectec
syntax structFieldType =
  fieldId plainType
```

**Example: a **
```spectec
syntax struct =
  { FIELD1 type1,
    FIELD2 type2 }
```

Fields can later be referenced by their names in `fieldAccessExpression` (i.e. `struct.FIELD1`) and `fieldUpdateExpression` (i.e. `struct' = struct[.FIELD1 = type1'`)


### Variant Type Definitions
```spectec
syntax variantTypeDef =
  "|"? caseType+"|"
```

### Plain Type Definitions (TODO)

## Premises
```spectec
syntax premise =
  | ifPremise
  | rulePremise
  | elsePremise
  | iteratedPremise
```

### If Premises
```spectec
syntax ifPremise =
  IF expression
```

### Rule Premises
```spectec
syntax rulePremise =
  relationId ":" expression
```

### Else Premises
```spectec
syntax elsePremise =
  OTHERWISE
```
Informally, `otherwise` means "if any of the premises in the previous rule do not hold". It is used to make it explicit that a function or relation is *total* (i.e. defined for all of its input space).

### Iterated Premises
```spectec
syntax iteratedPremise =
  "(" premise ")*"
```

## Expressions

### Literal Expressions (TODO)
```spectec
syntax literalExpression =
  | TRUE | FALSE
  | int
  | '"' text '"'
```

### Variable Expressions (TODO)

### Arithmetic Expressions
```spectec
syntax arithExpr =
  | "$(" expr ")"
```
Certain operations are allowed only inside arithmetic expressions, namely numeric binary expressions and inequality expressions.

### Operations on Boolean Types
#### Unary Expressions
```spectec
syntax logicalUnaryExpression =
  | "~" expr  ;; Not
```

#### Binary Expressions
```spectec
syntax logicalBinaryExpression =
  | expr "/\" expr  ;; And
  | expr "\/" expr  ;; Or
  | expr "=>" expr  ;; Implies
  | expr "<=>" expr ;; Equivalence
```

### Operations on Numeric Types
#### Unary Expressions
```spectec
syntax numericUnaryExpr =
  | "-" expr
  | "+" expr
```

#### Binary Expressions
```spectec
syntax numericBinaryExpr =
  | expr "+" expr
  | expr "-" expr
  | expr "*" expr
  | expr "/" expr
  | expr "\" expr ;; Modulo
```

#### Comparison Expressions
```spectec
syntax comparisonExpr =
  | expr "=" expr   ;; Equal
  | expr "=/=" expr ;; Not equal
  | expr "<" expr
  | expr ">" expr
  | expr "<=" expr
  | expr ">=" expr
```
Only the first two variants may appear outside arithmetic expressions.

### Operations on Struct Types
#### Field Access Expressions
```spectec
syntax fieldAccessExpression =
  expr "." id
```
Access the field named `id` in `expr` of *struct type*.

#### Field Update Expressions
```spectec
syntax fieldUpdateExpression =
  expr "[" path "=" expr "]"
```
For an `expr_1` with *struct type* `type_s`, creates a new value of `type_s` that has the same fields as `expr_1` except for the field `path`, which is updated to `expr_2`.

### Operation on Iterated Types
#### Epsilon
```spectec
syntax epsilonExpr = EPS
```
Epsilon means empty. For list iterators (`*`), epsilon is elaborated as the empty list. For optional iterators (`?`), epsilon is elaborated as `None` type.

#### Cons Expression
```spectec
syntax consExpr =
  expr "::" expr
```
LHS of type `x` and RHS of type `x*` are concatenated into a list of type `x*`.

#### List Constructor
```spectec
syntax listConstructorExpr =
  "[" expr*"," "]"
```
For expressions of type `x`, constructs a list of type `x*` with the given values. Note that `expr*` can be empty, which makes `[]` semantically equivalent to `eps`.

#### Concatenation
```spectec
syntax listConcatExpr =
  expr "++" expr
```
Concatenates two lists of the same type `x*`.

#### Length
```spectec
syntax listLengthExpr =
  "|" expr "|"
```
Returns the length of an expression of *iterated type*.

#### Index Access
```spectec
syntax listIndexAccessExpr =
  expr "[" expr "]"
```
Accessing an expression of *iterated type* with an index of `nat` type.

#### Slicing (TODO)
```spectec
syntax listSliceExpr =
  expr "[" expr ":" expr "]"
```
Slicing an expression of *iterated type* with range values of `nat` type.

#### Membership
```spectec
syntax listMembershipExpr =
  | expr "<-" expr
  | expr "->" expr
```
For a membership expression `expr_l "<-" expr_r`, it is checked whether `expr_l` of type `x` is an element of `expr_r` of type `x*`. It can also be written in the reverse order.

### Other Expressions
```spectec
syntax isTypeExpr =
  expr ":" plainType
```
Checks whether `expr` is of type `plainType`.

### Notation Expressions (TODO)

### Hint Expressions (TODO)

