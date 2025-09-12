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
│  ├─ 3.2-evaluation.spectec
│  └─ 4-aux.spectec
└─ spectec/
   └─ ...
```

Spec authors are free to use any ordering or categorization they prefer.

### Tools
Because there are multiple versions of SpecTec with varying language constructs and features, editor support is limited and seldom up-to-date. However, it is still better to have a semi-functional syntax highlighter than nothing at all.
* *Syntax Highlighting*
  * [VSCode extension: Wasm-SpecTec syntax highlighter](https://marketplace.visualstudio.com/items?itemName=taichimaeda2.spectec-highlight)
  * [Neovim treesitter grammar for P4-SpecTec](https://github.com/KunJeong/tree-sitter-spectec)
Fonts with custom ligatures for common programming and math symbols can greatly improve the readability of SpecTec's ASCII symbols, such as various arrows and turnstiles.

## SpecTec with Examples
In this section, we showcase the various features of SpecTec by writing a specification for simple toy language. We start with a very small language and gradually add new features to it.

### Language 1: Language With Functions and Local Variables
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

;; Case 1: map is empty
def $lookup_<K, V>(eps, K) = eps
;; Case 2: key is at the head
def $lookup_<K, V>((K_h -> V_h)::(K_t -> V_t)*, K) = V_h
  -- if K_h = K
;; Case 3: key is not at the head
def $lookup_<K, V>((K_h -> V_h)::(K_t -> V_t)*, K)
  = $lookup_<K, V>((K_t -> V_t)*, K)
  -- otherwise

def $lookup_context(context, id) = $lookup_<id, type>(context, id)
def $lookup_env(env, id) = $lookup_<id, value>(env, id)
```

We declare a generic `$lookup_` that takes a map from `K` to `V` and optionally returns a `V`. Next, we provide the definitions. Like relations and rules, each function definition is also a single control flow from input to output. Therefore, multiple definition are needed to define the function completely.

The IL interpreter tries each of the definition *in the order they are written*. A rule of thumb here is that functions on inductive types (recursively defined syntax or iterators) follow this general pattern: start from the base case and inductively build to general ones. That is exactly what happens here - we start with the easiest case of all: the empty list. `eps` stands for the greek letter epsilon (ε) which is often used to denote empty syntax. In SpecTec, `eps` can be used for empty iterators; i.e. empty lists and empty option types. So any lookup on an empty list would return None.

We then define another base case. Lists are either empty (`eps`), or can be deconstructed into the *head* element and *tail* list with the *cons*(`::`) operator: (`x_h::x_t*`). Since we already handled the empty case, we can now deconstruct the map into head and tail, each denoted by the pair syntax. Case 2 here is where the key we're searching for (`K`) is at the head(`K_h`), for which we return the value (`V_h`).

Case 3 is the inductive case. When the key is *not* at the head, we simply call lookup with the same key on the tail. This recursively completes the definition; if the tail is empty we go to Case 1, if the key was at position 2 we go to Case 2, and if we still fail to find the key we return to Case 3 with a smaller list.

Notice the usage of `-- otherwise` here, known as the *else premise*. Else premises negate all premises from the previous rule; here it negates `-- if K_h = K` becoming `-- if K_h =/= K`. In terms of the backtracking interpreter, it is a no-op; however else premises can help make it visually explicit that a function or relation has been fully defined.

Finally, we specialize the generic `$lookup_` with type parameters to get `$lookup_context` and `$lookup_env` which we used in typing and evaluation. The trailing `_` in `$lookup_` is to denote that the function is generic, thus incomplete. We discourage the use of generic functions in bodies of rules, as the presence of type parameters can be confusing when they are mixed with variables of the same type.

### Language 2: Language With Mutable Variables
So far we have defined the full static and dynamic semantics of a relatively small language. We now extend this language with a new feature: mutation. We skip the parts common with Language 1; you can find the whole specification in [spectec-with-examples].

#### Updating the Syntax and Type System
```spectec
;; 1-syntax.spectec

;; ...

syntax expr =
  ;; ...
  | RefE expr             ;; ref e
  | DerefE expr           ;; !e
  | UpdateE expr expr     ;; e1 := e2
```
The syntax is extended with three more expressions. `RefE` creates a reference to a mutable expression, `DerefE` accesses the underlying value, and `UpdateE` updates the underlying value. We can comine these expressions with local variables to create and use local variables. For instance, the program string `let x = ref 2 in x := x + 1` creates a mutable variable x with initial value 2 and later mutates it to 3. We can even pass these references to functions and mutate the values within the function body. What an exciting feature!
```spectec
;; 2.1-typing-type.spectec
syntax type =
  | INT
  | type -> type
  | REF type    ;; reference type
```
Now let's move on to types. We need a new type to disambiguate mutable expressions with immutable ones. Since references have underlying expressions, we can use the type of that expression to define reference types. The context definition is untouched.

```spectec
;; 2.3-typing.spectec

;; ...

rule Type/refE:
  C |- RefE e : REF type
  -- Type: C |- e : type

rule Type/derefE:
  C |- DerefE e : type
  -- Type: C |- e : REF type

rule Type/updateE:
  C |- UpdateE e_l e_r : type
  -- Type: C |- e_l : REF type
  -- Type: C |- e_r : type
```
The new typing rules are also straightforward. A reference expression creates a reference type by typing the underlying expression first. A derefencing expression unwraps the reference type. An update expression checks whether the reference's underlying type matches the type of the supplied expression.

#### Updating the Dynamic Semantics
While the type system was simple to extend, the evaluation is slightly more complex. 
```spectec
;; 3.1-evaluation-value.spectec
syntax value =
  | NumV int          ;; n
  | CloV id expr env  ;; <λx.e, env>
  | LocV nat          ;; loc

syntax sto = value*
```

We need a way to store the values in memory at runtime. Since we never deallocate memoery, we can model memory with an array of values, called a *store*. However, since every valid expression should be evaluated into a value, we need a new value for references. Since the store is an array, we can refer to values with their index in the array, which we call *location*. Thus `LocV` is defined with `nat`, the built-in type for 0 or greater integers - perfect for array indexes. We also declare an auxiliary function `$update_sto_at_index` which takes a store, an index, and a value, and returns an updated store.

```spectec
relation Eval:
  env; sto |- expr ==> value; sto
  hint(input %0 %1 %2)
```

The evaluation relation is updated as well; now expressions are not state-free, as they have the potential to update the store. Therefore we pass the store to evaluation and return the updated store. The `;` is another symbolic atom - having a separator here helps us write the inference rules without extra parentheses.

```spectec
rule Eval/numE:
  env; sto |- NumE i ==> NumV i; sto

rule Eval/binE-add:
  env; sto |- BinE ADD e_l e_r ==> NumV $(i_l + i_r); sto_2
  -- Eval: env; sto |- e_l ==> NumV i_l; sto_1
  -- Eval: env; sto_1 |- e_r ==> NumV i_r; sto_2
```
Stores are added to each side of the relation. This time, the two premises are no longer order-agnostic; as long as side-effects are involved, we must always use the updated store as an input to the next evaluation. The same changes are made to every evaluation rule.

```spectec
rule Eval/refE:
  env; sto |- RefE e ==> LocV n; sto_1 ++ [ value ]
  -- Eval: env; sto |- e ==> value; sto_1
  -- if |sto_1| = n
```
Now let's take a look at the new rules one by one. These showcase some list-related syntax features of SpecTec.

A reference expression is evaluated by evaluating the underlying expression first, to get `value`. We then check the length of the latest store with `|sto_1| = n`, as that will become the index for the new value. Therefore the whole expression evaluates to `LocV n`, and the new store is the latest store with `value` appended. Since we want to preserve order, instead of prepending with *cons*(`::`), we use the *list concatenation operator* `++` to concatenate `sto_1` with a singleton list `[ value ]`. The *list construction expression*(`[ x_1, x_2 ]`) can be used to construct a list type from a comma-separated list of values.

```spectec
rule Eval/derefE:
  env; sto |- DerefE e ==> value; sto_1
  -- Eval: env; sto |- e ==> LocV n; sto_1
  -- if sto_1[n] = value
```
For dereferencing, we evaluate the expression which should return a location. We then index the store with this location to get the desired value.

```spectec
rule Eval/updateE:
  env; sto |- UpdateE e_l e_r ==> value_r; sto_3
  -- Eval: env; sto |- e_l ==> LocV n; sto_1
  -- Eval: env; sto_1 |- e_r ==> value_r; sto_2
  -- if sto_2[[n] = value_r] = sto_3
```
For updating, we use another piece of SpecTec syntax, the *list update expression*, which allows us to produce an update copy of the store with the `n`th value updated to `value_r`. SpecTec disallows mutation (as to not surprise spec editors with unexpected side-effects), but instead provides declarative syntax to easily locate and update values when necessary.
