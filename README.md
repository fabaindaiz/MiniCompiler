# CDI-reference
Starter code for compilers homework

## Organization of the repository

The organization of the repository is designed for the development of your compiler. 
**(NOTA: para la entrega 0, algunos items no se van a usar en su pleno potencial, ver comentarios marcados "E0" abajo)**

- `compiler/`: the compiler, defined as a dune library (using a library allows us to play with our code in a REPL, see below)
**(E0: esta carpeta incluye solo un archivo con el parser y el interprete)**
- `bin/`: executables for tests and the compiler **(E0: interpreter)**
- **(E0: no se usa)** `tests/`: the test data used to test the compiler 

- **(E0: no se usa)** `bbctester/`: a library with helper tools to test the compiler 

- `dune-workspace`, `dune-project`: root configuration for the dune package manager
- `Makefile`: shortcuts to build and test

Dune will build everything inside the `_build/` directory.

## Makefile targets

- `make init`: generate .merlin files for autocompletion in IDE

- `make test`: execute the tests for the compiler defined in `bin/test.ml`
  variants include: 
  * `make ctest` for compact representation of the tests execution
  * **(E0: no se usa)** you can also add `F=<pat>` where `<pat>` is a pattern to filter which tests should be executed (eg. `make test F=arith` to run only test files whose name contains `arith`)
  
- `make clean`: cleans everything
  
- **(E0: no se usa)** `make clean-tests`: cleans the tests output 


## Writing tests

Tests are written using the [alcotest](https://github.com/mirage/alcotest) unit-testing framework. Examples can be found in `bin/test.ml`.

Alcotests executes a battery of unit-tests through the `run` function that takes a name (a string) and a list of items to be tested.

Each such item is composed itself from an identifier (a string) together with a list of unit-test obtained with the `test_case` function.
`test_case` takes a description of the test, a mode (either ``` `Quick ``` or ``` `Slow ```---use ``` `Quick ``` by default) and the test itself as a function `unit -> unit`.

A test is built with the `check` function which takes the following parameters:
- a way to test results of type `result_type testable`,
- an error message to be displayed when the test fails,
- the program to be tested, and the expected value (both of type `result_type`)


## Execution

There is a sample source file `example.src`, with a simple expression in it. To run it, 
use `dune exec bin/main.exe prog.src`.

To execute your interpreter interactively, use `dune utop` in a terminal, and then `open` the interpreter (`open Compiler.Interp;;`).

## Resources

Documentation for ocaml libraries:
- [containers](http://c-cube.github.io/ocaml-containers/last/)
- [alcotest](https://mirage.github.io/alcotest/alcotest/index.html)

