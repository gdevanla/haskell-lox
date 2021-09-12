# haskell-lox (WIP)

Implementation of `Lox` interpreter as described in [Crafting Interpreters](https://craftinginterpreters.com/).

1. [Scanning](src/Scanner.hs) - [Commentary](https://gdevanla.github.io/posts/crafting-interpreter-scanning.html)


## Execute

* Run `stack exec -- haskell-lox-exe` to see "We're inside the application!"
* With `stack exec -- haskell-lox-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`
