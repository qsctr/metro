# Language Reference

Dtfpl is a functional language with a syntax similar to Haskell. However, unlike Haskell, dtfpl is not a pure functional language.

Currently, dtfpl does not have a standard library or any built-in functions. However, you can define bindings to JavaScript functions yourself.

## Declarations

### Functions

Functions are defined using the `def` keyword.

```
def factorial
    0 -> 1
    x -> mul x (factorial (dec x))
```

Functions can use pattern matching to return different values based on their arguments. Patterns are checked in order. In the above example, the `factorial` function returns `1` if its argument is `0`. Otherwise, it binds the argument to the name `x` and evaluates the second alternative.

You can also pattern match on strings.

```
def answer
    "hello" -> "hi"
    "how are you" -> "okay"
    "bye" -> "bye"
```

Note that the above function will not work if it is called with any argument other than those three that it pattern matches on, because it lacks a catch-all pattern.

You can use `_` to match any value and discard the result.

```
def answer
    "hello" -> "hi"
    _ -> "I don't understand"
```

Multi-argument functions are automatically curried, so they can be partially applied.

```
def either-is-zero
    _ 0 -> true
    0 _ -> true
    _ _ -> false
```

Functions can take other functions as arguments.

```
def apply-twice
    f x -> f (f x)
```

### Let

Values can be bound to names by using a let declaration.

```
let factorial-of-five = factorial 5
```

### Native functions

You can define bindings to JavaScript functions by using the `native` keyword after `def`.

```
def native dec
    x -> x - 1
```

There can be any valid JavaScript expression after the arrow. However, argument names should be limited to valid JS identifiers so you can refer to them in the body of the function.

### Native let

You can define bindings to native JavaScript values by using the `native` keyword after `let`.

```
let native true = true
let native pi = Math.PI
```

As with native functions, there can be any valid JS expression in the body.

In the first line, we are defining the dtfpl variable `true` to be equal to the JS value `true`, since `true` is not a built-in expression in dtfpl.

You can also use `let native` to bind to JS functions directly. However, these functions will not be automatically curried, so you should only do this when the function takes only one argument.

```
let native sin = Math.sin
```

## Expressions

### Lambda

Lambda expressions are anonymous functions. They are most useful when used as higher-order functions.

```
apply-twice (\x -> plus x 20) 2
```

### If-then-else

If-then-else is a conditional expression.

```
if either-is-zero x y then "one of them is zero" else "both are not zero"
```

### Case

Case expressions allow pattern matching locally in an expression.

The factorial function defined above is equivalent to the following:

```
let factorial = \x -> case x of
    0 -> 1
    x -> mul x (factorial (dec x))
```

You can also pattern-match on multiple expressions at once, separated by commas.

```
case a, b of
    1, 2 -> "one and two"
    3, 4 -> "three and four"
```

## Main function

The `main` function will be automatically executed when the compiled JS program is run with node. It takes one argument which is an array of arguments passed to the program at the command line. If your program does not use this then you can use `_` as the pattern.

```
let native print = console.log

def main
    _ -> print "hello world"
```

If you want to execute multiple statements, you can define a (curried) function which takes any number of arguments and simply evaluates the arguments for their side effects, ignoring the results. This can be done by defining a function that ignores its argument and returns itself.

```
def do _ -> do
```

Now you can do:

```
def main
    _ -> do
        (print "hello")
        (print "world")
```
