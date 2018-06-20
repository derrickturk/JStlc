# JStlc

### Compile (Extended) Simply-Typed Lambda Calculus to Javascript (with an interactive REPL)
#### by [dwt](https://www.github.com/derrickturk) | [terminus data science, LLC](https://www.terminusdatascience.com)

JStlc is a proof-of-concept interpreter and compiler which implements a
simply-typed lambda calculus, augmented with recursive bindings (via implicit
or explicit fixed-point combinator), "polymorphic" option and list types
as well as integer, boolean, and string types with basic operations.

The JStlc REPL parses, typechecks, and evaluates terms and statements in an
interactive way; it can also be used to compile the current global definitions
to a Javascript file.

When run as a compiler, JStlc will translate input JStlc files to output
Javascript files on a one-to-one basis.

The JStlc implementation uses many of Haskell's quasi-dependent typing features
to maximize static checking of the implementation itself. Many ideas are
borrowed from Richard Eisenberg's work on
[glambda](https://github.com/goldfirere/glambda) and
[stitch](https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.pdf).

JStlc hacks in a bit of polymorphism, building on the existentials-everywhere
techniques used in glambda and stitch. Currently, JStlc provides a polymorphic
equality operator (`==`) which works on non-function types as well as
polymorphic list (`[a]`) and option (`?`) types and their
operations/eliminators.

JStlc takes an extremely minimalist approach to compilation: expressions
are compiled to single Javascript expressions (using a Y combinator to
implement recursion where necessary).
Statements are emitted as top level function or variable definitions.

## Getting Started

 - Install [stack](https://www.haskellstack.org)
 - Clone this repository and navigate into the repository directory
 - `stack build` (and `stack install`, if desired)
 - `stack exec JStlc` (or `JStlc`, if installed) - to launch REPL
 - `stack exec JStlc file1.jstlc file2.jstlc ...` (or `JStlc file1.jstlc ...` if installed) - to execute compiler (will produce `file1.js` and so on)

## REPL commands

By default, the REPL will parse and evaluate entered expressions (to the
Haskell representations of their values).
Special commands, beginning with `:`, are also available:

 - `:help` - show this message
 - `:quit` - exit JStlc REPL
 - `:type expr` - parse and typecheck an expression
 - `:parse expr` - parse an expression and show the untyped AST
 - `:compile expr` - compile an expression to a JS expression
 - `:inspect expr` - parse, check, and compile an expression verbosely
 - `:exec stmt` - execute a statement
 - `:parseStmt stmt` - parse a statement and show the untyped AST
 - `:compileStmt stmt` - compile a statement to a JS definition
 - `:inspectStmt stmt` - parse, check, and compile a statement verbosely
 - `:write filename` - compile all current definitions to specified JS file

## Language Reference

Types are:
  - `Int`
  - `Bool`
  - `String`
  - `[type]` (lists)
  - `?type` (optionals)
  - `type -> type` (functions)

Expressions consist of:
  - Variables: `x`, `abc123`
  - Literals: `23`, `true`, `false`, `"happy \"birthday\""`, `[1, 2]`
  - Lambdas: `\x : Int => x + 17`, `\f : Bool -> [Int] => f true`
  - Function applications: `f x y z`
  - Let-expressions: `let x = "hello" in x & " world"`
  - Recursive let-expressions (type annotation required): `let rec f: Int -> Int = \x : Int => if x == 0 then 1 else x * f (x - 1) in f 5`
  - Fixed-point combinator application: `(fix \f: Int -> Int => \x: Int => if x == 0 then 1 else x * f (x - 1)) 5`
  - Type-annotated null optionals: `none : ?Int`
  - Non-null optional constructors: `some 23`
  - Type-annotated empty lists: `nil : [[Bool]]`
  - List construction by consing: `[true] :: nil : [[Bool]]`
  - Conditional expressions: `\x: Int => if x % 2 == 0 then true else false`
  - (Polymorphic) left folds over lists: `foldl (\a: Int => \b: Int => a + b) 0 [1, 2, 3]`
  - (Polymorphic) map over lists: `map (\x: String => "hello " & x) ["world", "sunshine"]`
  - (Polymorphic) map over optionals: `map (\x: String => "hello " & x) (some "world")`
  - Binary operators applied to expressions:
    - arithmetic: `+`, `-`, `/`, `*`, `%`
    - logical (lazily-evaluated): `&&`, `||`
    - arithmetic comparison: `<`, `<=`, `>`, `>=`
    - polymorphic equality: `==`
    - string concatenation: `&`
    - polymorphic list cons: `::`
    - polymorphic list append: `++`
    - infix function application: `f $ x`

Statements consist of:
  - (Type-inferred) definitions: `x = 17;`, `f = \x: Int => x - 3;`
  - (Type-checked) annotated definitions: `b: Bool = true;`, `c: Bool = b && false;`
  - Recursive definitions (annotation required): `rec downFrom: Int -> [Int] = \x: Int => if x == 0 then nil: [Int] else x :: downFrom (x -1);`

Statements are compiled to Javascript definitions one-for-one, e.g.:
```
JStlc> :compileStmt rec downFrom: Int -> [Int] = \x: Int => if x == 0 then nil: [Int] else x :: downFrom (x -1);
function downFrom(x) {
        return ((x) === (0.0)) ? ([]) : (([x]).concat((downFrom)((x) - (1.0))));
}
```

## Caveats
JStlc is a proof-of-concept and missing a lot of features a "real language"
might want.
The provided operators don't allow for much beyond arithmetic on integers,
and the output JS leaves a lot to be desired in terms of optimization and
number of unnecessary parentheses.

JStlc now uses type-level vectors to represent typing contexts, but it's
still not able to re-use the same heterogeneous-vector type for singleton
type contexts as for value contexts. This seems like it should be easier,
using the existing type families.

#### (c) 2018 dwt | terminus data science, LLC
