# MyLang

This is my attempt to creating my own Programming language.
the language is now similar to typed lambda calculus, and has only a interpreter.
But i have a lot of plans:

 - [x] using bruijn index (don't know if its the best, but cool concept)
 - [x] using strict evaluation
 - [ ] opt-in laziness
 - [ ] compile via llvm
 - [ ] row polymorfisme
 - [ ] linear types

# Getting started

Installing via cabal:
```
cabal install --dependencies-only
cabal build
```
Installing via stack:
```
stack install
```

# Example

\> interperter
```
% let twice a = a * 2.0 in twice 4.0
Double
8.0
```
# Running Test

Test via Cabal:
```
cabal install --dependencies-only --enable-test
cabal configure --enable-test
cabal test
```
Test via Stack:
```
stack test
```
# license
This project is licensed under the BSD3 License - see the [LICENSE](LICENSE) file for details
