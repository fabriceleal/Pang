# Pang

A subset of scheme, implemented in F#.
Uses fslex and fsyacc.

## Supports

* Quasiquotation
* Recursive functions
* Anonymous functions

## Implemented

### Binding constructs

`let`, `let*`, `lambda`

### ...

`if`, `quote`, `define`, `set!`, `define-macro`, `begin`

## Functions implemented

### Input/Output

`display`

### Arithmetics

`+`, `-`, `*`, `/`

### Cons/List operations

`car`, `cdr`, `list`, `length`, `cons`

### Comparison

`=` for numbers

### Functional programming

`apply`, `map`

## Several Stuff

I was generating separate AST Objects for special forms; this means that this wouldnt work:

```
(eval `(,@(list 'if T T NIL)))
```

Now I'm reading EVERYTHING as lists, and added a gigantic:

```
match to_eval with
| Cons(Atom("if"), stuff) -> ...
| Cons(Atom("define"), stuff) -> ...
...
```

## TODOS

* more and better tests
* Compile as lib, so we can reuse for utilities, actual interpreter, tests, etc...
* Bugs / Missing

## Bugs / Missing

* Identifiers à-là `|this is an identifier with whitespaces i n s i d e |`
* Not supporting `#!fold-case`, `#!no-fold-case`
* Not supporting comments with `#;`. Only `;` or `#|` ... `|#`
* Escaped chars in strings are the same as F#s
* Character constants `\#c`
* Vector constants `#(1 2 3)`
* Bytevector constants `#u8(...)`
* Used in numbers `#e` `#i` `#b` `#o` `#d` `#x`
* Labeling data, like `#0#`, used for displaying circular structures
* More unknown stuff ...
* Type testing: `boolean?`, `bytevector?`, `char?`, `eof-object?`, `null?`, `number?`, `pair?`, `port?`, `procedure?`, `string?`, `symbol?`, `vector?`
* `read`, `write`, `eval`
* `lambda`s only work with a fixed number of args. Should work for variable and n or more variables (see spec 4.1.4)
* `if` only works in the form consequent, alternate
* `include`, `include-ci`
* `cond`
* `case`
* `letrec`, `letrec*`, `let-values`, `let*-values`
* `do`
* Delayed evaluation: `delay`, `delay-force`, `force`, `promise?`, `make-promise`
* Dynamic bindings: `make-parameter`, `parameterize`
* Exceptions: `guard`, `raise`
* `case-lambda`
... stopped at 4.3.1