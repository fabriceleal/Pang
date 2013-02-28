# Pang

A subset of scheme, implemented in F#.
Uses fslex and fsyacc.

## Bugs

I'm generating separate AST Objects for special forms, which I dont know if its
the way to go. This means that this wouldnt work:

```
(eval `(,@(list 'if T T NIL)))
```

The way to go is to read EVERYTHING as lists and add special forms to the environment,
as Syntax(_) objects.

## Supports

* Quasiquotation
* Recursive functions
* Anonymous functions

## Implemented

`if`, `let`, `let*`, `lambda`, `quote`, `define`, `set!`

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