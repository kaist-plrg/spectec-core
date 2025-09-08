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
dec $lookup_context(context, id) : type?

var C : context
```

We define a *generic syntax* `pair` for type parameters `K` and `V`, as a maaping from key to value. Then we define a `map` as a list of `pair`s using an *iterator*(`*`), which basically creates an unbounded list of the underlying type.  We can then use the `map` syntax for different key and value types. So, our typing context is a map from `id` to `type`. Let's also pretend that we have an auxiliary function `$lookup_context` that can fetch the type of a given id. This function has a return type of `type?`; the `?` is the *optional iterator*, meaning the function either returns a `type` or `eps`(none). The function body will be provided later.

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
The turnstile (`|-`) and colon(`:`) are once again just symbolic atoms, here used as separators to visually distinguish each component of the relation. The combination of atoms and meta-types are called *notations*, which is the heart of SpecTec's expressive yet robust type system. We also provide an *input hint*, which tells the SpecTec typechecker and interpreter backend that this relation is in fact, a partial function that takes the first two arguments of `context` and `expr`, and returns a `type`.

Now, we can define *rules* of the relation. Each rule defines a single execution path, where the input(s) map to the output(s) under some side-conditions, called *premises*.
```spectec
;; 2.3-typing.spectec
var e : expr
var n : nat
```
We defined a few more meta-variables for utility.
```spectec
;; 2.3-typing.spectec
rule Type/numE:
  C |- NumE n : INT

rule Type/binE:
  C |- BinE _ e_l e_r : INT
  -- Type: C |- e_l: INT
  -- Type: C |- e_r: INT
```
The type of a numeric literal is always `INT`. The type of a binary expression is also always `INT`, but it requires that both operands must be `INT` as well. We express this using *rule premises*, which recursively applies the same relation to sub-expressions.

```spectec
;; 2.3-typing.spectec
rule Type/letE:
  C |- LetE id e_p e_b : type_b
  -- Type: C |- e_p : type_p
  -- Type: (id -> type_p) :: C |- e_b : type_b

rule Type/varE:
  C |- VarE id : type
  -- if $lookup_context(C, id) = type
```
Let expressions and variables require context access and manipulation. When typechecking a let expression, the parameter type (`type_p`) must be bound to the id before typing the body expression. A list of values can be extended using the `::` infix operator, which prepends a value to a list of the same type. As our context is a list of pairs denoted by `->`, we can write `(id -> type_p) :: C` to describe the original context extended with the new binding.
When using the variable, we can simply lookup the id from the typing context to get the bound type. Since `$lookup_context` returns `type?`, this rule only success when the context has an entry for `id`. We can check equality or a number of different logical predicates with such *if predicates*.

```spectec
;; 2.3-typing.spectec
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

In addition, SpecTec does not explicitly define error states. For example, we do not define addition between two function types, or a function type and an integer. Instead, every control flow that is not defined will automatically result in an error. The interpreter backend uses backtracking to check whether any single rule is applicable to the given input, and errors if no rule can apply.

#### Defining Values
Now we move on to the dynamic semantics, or evaluation. Evaluation should take an expression and result in a value. Therefore we first define vaues.
```spectec
;; 3.1-evaluation-value.spectec
syntax env

syntax value =
  | NumV int          ;; n
  | CloV id expr env  ;; <λx.e, env>

syntax env = map<id, value>
dec $lookup_env(env, id): value?

```
In our simple langauge, we only need two types of values: integer values and closures. Closures are first-class functions, or *functions as values*. Using functions as values allow us to pass a functions as arguments to other functions, which is also known as *high-order functions*. The concept is quite simple; we take the function definition (`id` and `expr`) along with the environment at the moment of definition, and we freeze it in time. We then pass this frozen capsule around pretending it's a value, and when the time comes to apply the function, we unpack the closure to reveal all the data.
This begs the question: what is an environment? The *environment* is not much different from the typing context; instead of mapping ids to types, we map ids to values. Local bindings will add bindings to the environment, like what we did in the typing rules. We also declare the same lookup function for environments.

#### Defining Evaluation
```spectec
;; 3.2-evaluation.spectec
relation Eval:
  env |- expr ==> value
  hint(input %0 %1)
```

The judgement for evaluation takes an expression and returns a value, under some environment. Thus the input is `env` and `expr`, and the output is `value`. We use a slightly different notation (`==>`) to visually distinguish typing from evaluation.

```spectec
;; 3.2-evaluation.spectec
rule Eval/numE:
  env |- NumE int ==> NumV int

rule Eval/binE-add:
  env |- BinE ADD e_l e_r ==> NumV $(n_l + n_r)
  -- Eval: env |- e_l ==> NumV n_l
  -- Eval: env |- e_r ==> NumV n_r

rule Eval/binE-mul:
  env |- BinE MUL e_l e_r ==> NumV $(n_l * n_r)
  -- Eval: env |- e_l ==> NumV n_l
  -- Eval: env |- e_r ==> NumV n_r

rule Type/letE:
  C |- LetE id e_p e_b : type_b
  -- Type: C |- e_p : type_p
  -- Type: (id -> type_p) :: C |- e_b : type_b

rule Type/varE:
  C |- VarE id : type
  -- if $lookup_context(C, id) = type
```
Evaluating an integer expression produces an integer value. Unlike typing, we have two separate rules for `ADD` and `MUL` which are straightforward. To describe the result we use *arithmetic expressions* surrounded by `$( )`, which is necessary to disambiguate operators such as `*` and `<=` which can have multiple meanings.
The evaluation of let and variable expressions are symmetric with their typing counterparts.

```spectec
;; 3.2-evaluation.spectec
rule Eval/funE:
  env |- FuncE id _ e ==> CloV id e env 

rule Eval/appE:
  env |- ApplyE e_f e_a ==> value_r
  -- Eval: env |- e_f ==> CloV id e_b env_clo
  -- Eval: env |- e_a ==> value_a
  -- Eval: (id -> value_a) :: env_clo |- e_b ==> value_r
```
Lambda functions and applications are slightly different. A function evaluation just creates a closure capturing all of its state. For function application, we evaluate the left-hand side, and if it is indeed a closure, unpack it. We then evaluate the argument, and finally we evaluate the body with the closure environment extended with a new binding (id to value of argument).

#### Defining Auxiliary Functions
All that is left is to provide the definitions of `$lookup_env` and `$lookup_context`. We can provide function definitions with `def`. In fact, we can once again leverage generic types to provide the definition of both at once.
```spectec
;; 4-aux.spectec
dec $lookup_<K, V>(map<K, V>, K) : V?

def $lookup_<K, V>(eps, K) = eps
def $lookup_<K, V>((K_h -> V_h)::(K_t -> V_t)*, K) = V_h
  -- if K_h = K_query
def $lookup_<K, V>((K_h -> V_h)::(K_t -> V_t)*, K)
  = $lookup_<K, V>((K_t -> V_t)*, K)
  -- otherwise

def $lookup_context(context, id) = $lookup_<id, type>(context, id)
def $lookup_env(env, id) = $lookup_<id, value>(env, id)

```

We declare a generic `$lookup_` that takes a map from `K` to `V` and optionally returns a `V`. Next, we provide the definitions. Like relations and rules, each function definition is also a single control flow from input to output. Therefore, multiple definition are needed to define the function completely.
The IL interpreter tries each of the definition *in the order they are written*. Thus, a rule of thumb is that functions on inductive types (recursively defined syntax or iterators) follow this general pattern: start from the base case and inductively build to general ones. That is exactly what happens here - we start with the easiest case of all: the empty list. `eps` stands for the greek letter epsilon (ε) which is often used to denote empty syntax. In SpecTec, `eps` can be used for empty iterators; i.e. empty lists and empty option types. So any lookup on an empty list would return None.
