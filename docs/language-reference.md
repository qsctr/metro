# Language Reference

Dtfpl is a functional language with a syntax similar to Haskell. However, unlike Haskell, dtfpl is not a pure functional language.

Note that at the moment dtfpl does not come with any built-in functions whatsoever. The compiler can still compile programs by converting them into the equivalent JavaScript syntactically, but these programs will not run. This will change after support for JavaScript FFI is added. Therefore, the example code below is only for demonstration of syntax.

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

Multi-argument functions are automatically curried, so they can be partially applied.

```
def either-is-zero
    a 0 -> true
    0 b -> true
    a b -> false
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
