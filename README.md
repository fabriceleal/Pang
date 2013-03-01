# Pang

A subset of scheme, implemented in F#.
Uses fslex and fsyacc.

## Bugs / Missing

* Identifiers à-là `|this is an identifier with whitespaces i n s i d e |`
* More unknown stuff ...

## Supports

* Quasiquotation
* Recursive functions
* Anonymous functions

## Implemented

`if`, `let`, `let*`, `lambda`, `quote`, `define`, `set!`, `define-macro`, `begin`

## Functions implemented

### Input/Output

`display`

### Arithmetics

`+`, `-`, `*`, `/`

### Cons/List operations

`car`, `cdr`, `list`, `length`, `cons`

### Comparison

`eq`

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
