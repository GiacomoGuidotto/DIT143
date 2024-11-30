# DIT143 - Functional Programming

[![Haskell CI](https://github.com/GiacomoGuidotto/DIT143/actions/workflows/haskell.yaml/badge.svg)](https://github.com/GiacomoGuidotto/DIT143/actions/workflows/haskell.yaml)

This package contains the content of the course "DIT143 - Functional Programming" offered at the University of Gothenburg.
It is divided into labs, each of which is a self-contained Haskell project.

## Getting started

use cabal to build the project:

```bash
cabal build
```

run different test suites or executables with:

```bash
cabal run [test-suite-name]
```

## Test suites

- `playground-test` - test suite for playground
- `lab1-test` - test suite for lab1
- `lab2-test` - test suite for lab2
- `lab3-test` - test suite for lab3

## Executables

- `blackjack` - executable for blackjack game from lab2
- `calculator` - executable for calculator from lab4

## Resources

- [Cabal user guide](http://haskell.org/cabal/users-guide/)
- [Haskell PVP](https://pvp.haskell.org/)
