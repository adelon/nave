A prototype for a [controlled natural language](https://en.wikipedia.org/wiki/Controlled_natural_language)
for mathematical formalizations and verification with type-theoretical semantics.


## Building

This project uses [stack](http://haskellstack.org/).
A brief tutorial can be found [here](https://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html).

You can start the REPL (GHCi) with `stack repl` and build the project using `stack build`.


## Running the examples

Currently the program just tries to parse all the file in the `examples/` folder,
rendering the resulting AST to `.out` files in the same directory.
You can run it with `main` in the REPL
or with `stack exec nave` (after building) in the shell.
