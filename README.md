# CDI-reference
Starter code for compilers homework

## Organization of the repository

- `src/`: all the resources of the compiler
- `src/compiler`: the compiler sources (as a library) **para la entrega 0, esta carpeta incluye solo un archivo con el parser e interprete**
- `src/execs`: executables for tests and the frontend of the compiler
- `src/tests`: the test data used to test the compiler **para la entrega 0, esta carpeta no se va a usar**

- `bbctester/`: a library with helper tools to test the compiler **para la entrega 0, esta librería no se va a usar**

- `dune-workspace`, `dune-project`: to make dune (ocaml build manager) happy (fix the root of the project)
- `Makefile`: shortcut to build and test


Dune will build everything inside the `_build/` directory.

## Makefile targets

- `make init`: generate .merlin files for autocompletion in IDE

- `make test`: execute the tests for the compiler in `src/execs/test.ml`
  variants include 
  * `make ctest` for compact representation ; and
  * `make test F=<pat>` where `<pat>` is a pattern to filter which tests should be executed **para la entrega 0, los filtros no se usarán**
  
- `make clean-tests`:
- `make clean`: cleans everything


## Writing tests

Tests are written using the [alcotest](https://github.com/mirage/alcotest) unit-testing framework. Examples can be found in `src/execs/test.ml`.

Alcotests executes a battery of unit-tests through the `run` function that takes a name (a string) and a list of items to be tested.

Each such item is composed itself from an identifier (a string) together with a list of unit-test obtained with the `test_case` function.
`test_case` takes a description of the test, a mode (either ``` `Quick ``` or ``` `Slow ```) and the test itself as a function `unit -> unit`.

A test is built with the `check` function which takes the following parameters:
- a way to test results of type `result_type testable`,
- an error message to be displayed when the test fails,
- the program to be tested, and the expected value (both of type `result_type`)



## Resources

Documentation for ocaml libraries:
- [containers](http://c-cube.github.io/ocaml-containers/last/)
- [alcotest](https://mirage.github.io/alcotest/alcotest/index.html)

