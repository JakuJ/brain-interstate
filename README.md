# brain-interstate

A Brainf\*\*k REPL and compiler, because why not. The name derives from the word "**inter**preter" and the **State** Monad, which is used to manage computations that modify the tape.

## Features

- A fully functional REPL for interpreting BrainF\*\*k line by line
- A [BF -> C -> Machine code] compiler
- Detecting infinite loops and mismatched brackets
- Unit test coverage of BF code parsing, command-line args parsing and the compiler itself

## Installation

Assuming you have stack installed, just run
```
stack install
```

Otherwise, compile the REPL and the compiler by hand using GHC. The flags and dependencies necessary for manual compilation are provided in `package.yaml`.

## Unit tests

Assuming you have stack installed, just run
```
stack test
```

Otherwise, compile the test suite in the `test` folder and run it.
