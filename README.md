# metro

Metro is a dynamically typed functional programming language that compiles to JavaScript, with a syntax similar to Haskell.

## Current status

Metro is still in development. Currently, the compiler is able to compile metro programs into JavaScript, but it is alpha-quality and does not do much error-checking or optimization. The language itself is also quite basic for now, but features are actively being added, so make sure you check back in the future.

## Language docs

See the `docs` directory for documentation.

## Trying it out

The only way to use the metro compiler at the moment is to build it from source. It is written in Haskell, so you'll need [stack](https://haskellstack.org). You also need [node.js](https://nodejs.org) to run the compiler since it uses some JavaScript libraries.

1. Clone or download this repository. If you want a more stable version then use the most recent release.
2. Run `npm install` to install JS dependencies.
3. Run `stack install` to build the compiler.

Now you should have an executable named `metro`.

## About the name

Metro is just a temporary name that stands for "dynamically typed functional programming language". Very creative, I know.
