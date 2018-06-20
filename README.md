# dtfpl

Dtfpl is a dynamically typed functional programming language that compiles to JavaScript, with a syntax similar to Haskell.

## Current status

Dtfpl is still in development. Currently, the compiler is able to compile dtfpl programs into JavaScript, but it is alpha-quality and does not do much error-checking or optimization. The language itself is also quite basic for now, but features are actively being added, so make sure you check back in the future.

## Language docs

See the `docs` directory for documentation.

## Trying it out

The only way to use the dtfpl compiler at the moment is to build it from source. It is written in Haskell, so you'll need [stack](https://haskellstack.org).

1. Clone this repository.
2. Run `stack install` inside the directory.

Now you should have an executable named `dtfpl`.

## Why does dtfpl exist?

Many JavaScript programs and libraries today are adopting functional programming, using functional techniques like higher order functions, partial application, function composition, etc. However, the JavaScript language itself is not very well suited for functional programming. Therefore, dtfpl provides a better way to write and structure functional programs, while still compiling to high-level, readable, and modern JavaScript, and remaining dynamically typed for simpler JS interop and easier adoption. Dtfpl will also come with a standard library with many useful utility functions.

## About the name

Dtfpl is just a temporary name that stands for "dynamically typed functional programming language". Very creative, I know.
