# SpecTec EL Language Reference

SpecTec EL(External Language), also known as SpecTec DSL, is a language designed to describe the syntax and semantics of other programming languages. It is modeled after *mathematical inference rules* which are used in programming language textbooks and literature.

## Notation
In the SpecTec-style grammar notation below, we use `x?`, `x*`, `x+` to denote optional, sequences and non-empty sequences of symbols.
We also use `'s'` to denote concrete strings, and `x*'s'`, `x+'s'` to denote lists with separators.

## Types
SpecTec operates on *abstract syntax*, which are represented as SpecTec types. To disambiguate with types of the specified language, we use the term *syntax* or *meta-types* to refer to SpecTec types.

### Primitive Types
```spectec
syntax primitiveType =
  | 'bool' | 'nat' | 'int' | 'text'
```

There are four primitive types in SpecTec. `bool` denotes booleans, `nat` denotes 0 or positive integers, `int` denotes all signed integers, and `text` denotes general strings.

### Plain Types
```spectec
syntax plainType =
  | primitiveType
  | '(' plainType+',' ')'   (; tuple type ;)
  | plainType iterator      (; iterated type ;)

syntax iterator = 
  | '*'                     (; optional ;)
  | '?'                     (; list ;)
```

Plain types recursively build upon primitive types, expressing their Cartesian product (tuples) or iteration.

### Notation Types
Notation is at the heart of SpecTec's expressive type system. Notations allow for flexible definition of types using a mixture of *identifier atoms* and *symbolic atoms*.
```spectec
syntax lowId =
  lowercase (letter | digit)*
syntax upId =
  uppercase (letter | digit)*

syntax typeName = lowId

syntax idAtom = upId

syntax relationalAtom =
  | ':' | '<:' | ':>' | '~~' | '~>' | '~>*' | '|-' | '-|'

syntax infixAtom =
  | '`.' | '..' | '...' | ';' | '\' | '->' | '->_' | '=>_' | '==>'

syntax symbolicAtom = relationalAtom | infixAtom

syntax notationType =
  | typeName | plainType
  | idAtom                                    (; identifier atoms ;)
  | relationalAtom notationType               (; relational notation ;)
  | notationType relationalAtom notationType
  | notationType infixAtom notationType       (; infix notation ;)
  | '`' infixAtom                             (; escaped infix atom ;)
  | '`(' notationType ')'                     (; bracket notation ;)
  | '`[' notationType ']'
  | '`{' notationType '}'
```
*Identifier atoms* (uppercase-led strings) can be freely mixed with types to construct notations. *Symbolic atoms* behave the same, except symbols have a pre-assigned precedence that affect parsing. Rougly speaking, parsing of notation goes from *relational notations* (outermost) -> *infix notations* -> *sequences* -> *brackets* -> *ids* / *plain types* (innermost). The list of symbolic atoms and their precedence may be changed depending on the specified language.

Internally, all atoms are merged into a single *mixfix constructor*, which is used to disambiguate between [variants of the same type](#variant-types).

### Variant Types
```spectec
syntax variantTypeCase =
  | notationType
  | typeName                (; extend existing type ;)

syntax variantType =
  '|'? variantTypeCase+'|'
```
*Variant types* supply one or more type cases as alternatives, separated by `|`. Every alternative must have a different arrangement of atoms, to be syntactically distinguishable. Variant definitions can also extend one or more existing variant types with their type name. However, extension of [plain types](#plain-types) are not allowed.

### Record Types
```spectec
syntax recordField =
  fieldId plainType

syntax recordType =
  '{' recordField+',' '}'
```
*Record types* are types with named fields. Each field is given a distinct uppercase-led name, which is later used to access or update the field using dot notation.

### Type Definitions
```spectec
syntax defType =
  | plainType           (; type alias ;)
  | variantType
  | recordType

syntax syntaxDefinition =
  'syntax' typeName '=' defType
```
Type definitions either define an alias to a [plain type](#plain-types), define [multiple variants](#variant-types), or create a [record type](#record-types), with a lowercase-led type name.

## Expressions

### Literal Expressions
```spectec
syntax literalExpr =
  | 'true' | 'false'
  | int
  | '"' text '"'
```

### Variable Expressions
```spectec
syntax variableExpr =
  varId
```
*Variables* should either be [declared](#variable-declarations) or it should be possible to infer their types.

### Operations on Boolean Types
#### Logical Unary Expressions
```spectec
syntax logUnOp =
  '~'  (; not ;)
syntax logicalUnaryExpression =
  | logUnOp expr
```
The logical negation can be used to negate a boolean expression.

#### Logical Binary Expressions
```spectec
syntax logBinOp =
  | '/\'  (; and ;)
  | '\/'  (; or ;)
  | '=>'  (; implies ;)
  | '<=>' (; equivalent ;)
syntax logicalBinaryExpr =
  expr logBinOp expr
```
And, or, implies and equivalent logical operators can be used to compose two boolean expressions.

### Operations on Numeric Types
#### Comparison Expressions
```spectec
syntax cmpOp =
  | '='
  | '=/=' (; not equals ;)
  | '<' | '>' | '<=' | '>='
syntax comparisonExpr =
  | expr cmpOp expr
```

### Operations on Tuple Types
```spectec
syntax tupleExpr =
  '(' expr*',' ')'
```
The construction of tuple expressions is the same as [tuple types](#tuple-types). Tuples can be deconstructed with the same syntax via pattern matching.

### Operations on List Types
#### Epsilon
```spectec
syntax epsilonExpr = 'eps'
```
Epsilon means empty. For list iterators (`*`), epsilon is elaborated as the empty list. For optional iterators (`?`), epsilon is elaborated as `None` value.

#### Cons Expression
```spectec
syntax consExpr =
  expr '::' expr
```
LHS of type `x` (head) and RHS of type `x*` (tail) are concatenated into a list of type `x*`.

#### List Construction
```spectec
syntax listConstructionExpr =
  '[' expr*',' ']'
```
For expressions of type `x`, constructs a list of type `x*` with the given values. Note that `expr*` can be empty, which makes `[]` semantically equivalent to `eps`.

#### Concatenation
```spectec
syntax listConcatExpr =
  expr '++' expr
```
Concatenates two lists of the same underlying type.

#### Length
```spectec
syntax listLengthExpr =
  '|' expr '|'
```
Returns the length of an expression of *iterated type*.

#### Index Access
```spectec
syntax listIndexAccessExpr =
  expr '[' arith ']'
```
Accessing an expression of *iterated type* with an index of `nat` type.

#### Update Expression
```spectec
syntax path =
  | path? '[' arith ']'             (; list index ;)
  | path? '[' arith ':' arith ']'   (; list slice ;)
  | path? '.' idAtom                (; record field ;)

syntax updateExpr =
  expr '[' path '=' expr ']' = expr
```
Creates a new list with element at index updated to the expression inside the bracket.

#### Slicing
```spectec
syntax listSliceExpr =
  expr '[' arith ':' arith ']'
```
Slicing an expression of *iterated type* with range values of `nat` type.

#### Membership
```spectec
syntax listMembershipExpr =
  | expr '<-' expr
  | expr '->' expr
```
For a membership expression `expr_l '<-' expr_r`, it is checked whether `expr_l` of type `x` is an element of `expr_r` of type `x*`. It can also be written in the reverse direction.

### Operations on Record Types
#### Record Construction
```spectec
syntax fieldExpr = idAtom expr
syntax recordExpression =
  '{' fieldExpr*',' '}'
```
Record expressions can be constructed and deconstructed using syntax similar to [record types](#record-types).

#### Field Access Expressions
```spectec
syntax fieldAccessExpression =
  expr '.' idAtom
```
Record fields can be accessed with their id.

#### Field Update Expressions
The third variant of [update expressions](#update-expressions) may be used to update record fields.

### Arithmetic Expressions
```spectec
syntax numExpr =
  | '$(' arith ')'

syntax numUnOp = '+' | '-'
syntax numBinOp =
  | '+' | '-' | '*' | '/'
  | '\'   (; modulo ;)

syntax unOp = logUnOp | numUnOp
syntax binOp = cmpOp | logBinOp | numBinOp

syntax arith =
  | unOp arith
  | arith binOp arith         
  | variableExpr
  | literalExpr
  | expr '[' arith ']'        (; list indexing ;)
  | '(' arith ')'
  | '(' arith iterator ')'
  | '|' expr '|'              (; list length ;)
  | '$' funcId                (; function call ;)
  | '$' '(' expr ')'          (; escape back to expr ;)
```
Because `*` and `+` are parsed as iterators by default, expressions must be enclosed by `$( ... )` to be used as arithmetic operators. A subset of normal expressions plus *numeric operators* are allowed inside arithmetic expressions, and they can be escaped back to normal expressions with another enclosure.

### Other Expressions
```spectec
syntax isTypeExpr =
  expr ':' plainType
```
Checks whether `expr` is of type `plainType`.

### Notation Expressions (TODO)

### Hint Expressions (TODO)

## Premises
```spectec
syntax premise =
  | ifPremise
  | relationPremise
  | elsePremise
  | iteratedPremise
```


### If Premises
```spectec
syntax ifPremise =
  'if' expr
```
*If premises* are the most basic form of premises

### Rule Premises
```spectec
syntax relationPremise =
  relationId ':' expr
```

### Else Premises
```spectec
syntax elsePremise =
  'otherwise'
```
Informally, `otherwise` means 'if any of the premises in the previous rule do not hold'. It is used to make it explicit that a function or relation is *total* (i.e. defined for all of its input space).

### Iterated Premises
```spectec
syntax iteratedPremise =
  '(' premise ')*'
```


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
  'syntax' syntaxId+','
```
Declaring one or more syntax before they are defined. Mostly used as forward-declarations for mutually recursive syntax.

### Syntax Definitions
```spectec
syntax syntaxDefinition =
  'syntax' varId '=' defType
```

### Variable Declarations
```spectec
syntax variableDeclaration =
  'var' varId ':' plainType
```

### Relation Declaration
```spectec
syntax relationDeclaration =
  'relation' relationId ':' notationType
```

### Rule Definition
```spectec
syntax ruleDefinition =
  'rule' relationId '/' ruleId ':' expression ('--' premise)*
```
SpecTec rules define a single path of a relation. Thus, the *input* portion of `expression` evaluates to the *output* portion only when all of the premises evaluate to `true`.

### Function Declaration
```spectec
syntax functionDeclaration =
  'dec' '$' functionId typeParams? params? ':' plainType
```

### Function Definition
```spectec
syntax funtionDefinition =
  'def' '$' functionId typeParams? params? '=' expression ('--' premise)*
```
