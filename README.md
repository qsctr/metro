# metro

Metro is a functional programming language.

Currently, the language is dynamically typed and compiles to JavaScript, but I plan to add a static type system and other backends in the future. The language itself is usable but quite basic at the moment. New features are being added, although at a relatively slow pace since schoolwork takes up most of my time. The [projects page](https://github.com/qsctr/metro/projects/1) contains a list of stuff I'm planning on adding. I may write up a more detailed plan at some point.

The purpose of this project is to explore not only language design, but also compiler design using Haskell. So I use this project as a place to experiment with advanced GHC extensions and new Haskell libraries.

## Documentation

See [the `docs` directory](docs/) for documentation.

## Installation

The only way to use the metro compiler at the moment is to build it from source. You'll need [stack](https://haskellstack.org) to build the compiler, and also [node.js](https://nodejs.org) to run the compiler since it uses some JavaScript libraries.

1. Clone or download this repository. If you want a more stable version then use the most recent release.
2. Run `npm install` to install JS dependencies.
3. Run `stack install` to build the compiler.

Now you should have an executable named `metro`.
