# Introduction

A programming language specification is the authoritative document that defines the language's syntax and semantics. It is responsible for ensuring that different compilers and interpreters behave consistently, which is crucial for creating code that is portable and predictable.

SpecTec is designed to closely resemble pen-and-paper notation commonly used in literature and textbooks on programming language semantics. At the same time, SpecTec-Core aims to be explicit enough to be able to generate interpreters or prose specification automatically.

The core concepts of SpecTec consist of the following:
* *Syntaxes*, which represent abstract syntax or internal constructs via meta-types;
* *Relations* and inference rules, which represent core semantics such as typechecking or evaluation;
* *Functions*, which enable auxiliary meta-level definitions.

## Getting Started

### Project Structure
SpecTec does not have namespacing or modules. This provides the advantage that mutual recursion may be split across any number of files. The obvious downside is that naming things can get verbose; however, being a specification language, clarity is valued over brevity.

The recommended  to keep all `.spectec` specification files of a language under the same directory, in alphabetical order. This allows for easier importing via command-line globbing, i.e. `spec/*.spectec`. Below is an example directory structure.

```bash
spectec-core/
├─ spec/
│  ├─ 1-syntax.spectec
│  ├─ 2.1-typing-type.spectec
│  ├─ 2.2-typing-context.spectec
│  ├─ 2.3-typing.spectec
│  ├─ 3.1-evaluation-value.spectec
│  └─ 3.2-evaluation.spectec
└─ spectec/
   └─ ...
```

Spec authors are free to use any ordering or categorization they prefer.

### Tools
Because there are multiple versions of SpecTec with varying language constructs and features, editor support is limited and seldom up-to-date. However, it is still better to have a semi-functional syntax highlighter than nothing at all.
* *Syntax Highlighting*
  * [VSCode extension: Wasm-SpecTec syntax highlighter](https://marketplace.visualstudio.com/items?itemName=taichimaeda2.spectec-highlight)
  * [Neovim treesitter grammar for P4-SpecTec](https://github.com/KunJeong/tree-sitter-spectec)

## SpecTec with Examples
In this section, we showcase the various features of SpecTec by writing a specification for simple toy language. We start with a very small language and gradually add new features to it.

### Step 1: Language With Functions and Local Variables
Our first language has only one language construct: expressions. Expressions are recursively constructed into numeric literals, binary expressions, let expressions (local binding), variable expressions, lambda functions, and function applications. 

#### Defining Abstract Syntax
*Abstract syntax* can be understood as internal representations of language constructs, usually with a rough one-to-one correspondence to *concrete syntax* (actual program strings). SpecTec operates on this abstract syntax.

```spectec
;; 1-syntax.spectec
syntax id = text

syntax binop = ADD | MUL ;; +, *

syntax type

syntax expr =
  | NumE int              ;; n
  | BinE binop expr expr  ;; e1 op e2
  | LetE id expr expr     ;; let x = e1 in e2
  | VarE id               ;; x
  | FuncE id type expr    ;; λx:t.e
  | ApplyE expr expr      ;; e1 e2

```
First, we define identifiers as simple strings, using the built-in syntax type `text`.
Then we define binary arithmetic operators, which is specified using keywords `ADD` and `MUL`, called *atoms*. Atoms can appear anywhere inside syntax definitions.
We then build upon these to recursively define expressions. SpecTec allows such recursive syntax definitions by default. Note that the leading `|` in variant types is optional.
We also need to annotate the parameter types of lambda functions. However, we want to define types at a later stage. In such cases, we can just define the syntax and provide the definition later.

#### Defining Types
```spectec
;; 2.1-typing-type.spectec
syntax type =
  | IntT | UIntT
```
To be able to express a type system, we need to define types. In our simple language, a type is either an integer, or a function type.

The arrow symbol `->` here is a *symbolic atom*, which is a symbol that can be used freely within syntax definitions. However, unlike alpha-numeric atoms that allow any sequence of strings and numbers led by a capital letter, the set of symbolic atoms is pre-defined in SpecTec.

#### Defining Internal State
Like most real-world programming languages, our small language needs the help of internal data structures for static and dynamic semantics. For static semantics (typechecking), we need a *typing context* that maps variable `id`s to their `type`s. Here is an example of defining such a structure.
```spectec
;; 2.2-typing-context.spectec
syntax pair<K, V> = K -> V
syntax map<K, V> = (pair<K, V>)*

syntax context = map<id, type>
dec $lookup_context(context, id) : type

var C : context
```

We define a *generic syntax* `pair` for type parameters `K` and `V`, as a maaping from key to value. Then we define a `map` as a list of `pair`s using an *iterator*(`*`), which basically creates an unbounded list of the underlying type.  We can then use the `map` syntax for different key and value types. So, our typing context is a map from `id` to `type`. Let's also pretend that we have an auxiliary function `$lookup_context` that can fetch the type of a given id. The definition will be provided later.

We also declare a *meta-variable* `C`. This variable declaration ensures that variables named `C` has meta-type `context` wherever it is used.
By default, SpecTec allows the usage of variables that have the same name as their syntax, as well as subscripts and primed variations of these. For instance, `context`, `context_new`, `C`, and `C'` can all be used as variables of meta-type `context`.

#### Defining the Type System
```spectec
;; 2.3-typing.spectec
relation Type:
  context |- expr : type
  hint(input %0 %1)
```
First, we define a *relation* signature (a.k.a judgement).
The turnstile (`|-`) and colon(`:`) are once again just symbolic atoms, here used as separators to visually distinguish each component of the relation. We also provide an *input hint*, which tells the SpecTec typechecker and interpreter backend that this relation is in fact, a partial function that takes the first two arguments of `context` and `expr`, and returns a `type`.

Now, we can define *rules* of the relation. Each rule defines a single execution path, where the input(s) map to the output(s) under some side-conditions, called *premises*.
```spectec
;; 2.3-typing.spectec
var e : expr
var n : nat
```
We defined a few more meta-variables for utility.
```spectec
rule Type/numE:
  C |- NumE n : INT

rule Type/binE:
  C |- BinE _ e_l e_r : INT
  -- Type: C |- e_l: INT
  -- Type: C |- e_r: INT
```
The type of a numeric literal is always `INT`. The type of a binary expression is also always `INT`, but it requires that both operands must be `INT` as well. We express this using *rule premises*, which recursively applies the same relation to sub-expressions.

```spectec
rule Type/letE:
  C |- LetE id e_p e_b : type_b
  -- Type: C |- e_p : type_p
  -- Type: (id -> type_p) :: C |- e_b : type_b

rule Type/varE:
  C |- VarE id : type
  -- if $lookup_context(C, id) = type
```
Let expressions and variables require context access and manipulation. When typechecking a let expression, the parameter type (`type_p`) must be bound to the id before typing the body expression. A list of values can be extended using the `::` infix operator, which prepends a value to a list of the same type. As our context is a list of pairs denoted by `->`, we can write `(id -> type_p) :: C` to describe the original context extended with the new binding.
When using the variable, we can simply lookup the id from the typing context to get the bound type. We can check equality or a number of different logical predicates with these *if predicates*.

```spectec
rule Type/funcE:
  C |- FuncE id type_p e_b : type_p -> type_b
  -- Type: (id -> type_p) :: C |- e_b : type_b

rule Type/applyE:
  C |- ApplyE e_f e_a : type_b
  -- Type: C |- e_f : type_p -> type_b
  -- Type: C |- e_a : type_p
```
The typing rules for lambda functions is similar to let expressions. Function applications are typed by checking whether the left expression is a function type, then also checks if the argument matches the parameter type.
Notice how the two rule premises share the meta-variable `type_p`. It implies that the output of the two rules should follow a certain pattern. Unlike conventional programming languages, which would require creating two different variables and checking whether they are equal, SpecTec can understand these implicit conditions and insert implicit premises during the elaboration stage.

In addition, SpecTec does not explicitly define error states. For example, we do not define addition between two function types, or a function type and a number. Instead, every control flow that is not defined will automatically result in an error. The interpreter backend uses backtracking to check whether any single rule is applicable to the given input, and errors if no rule can apply.
