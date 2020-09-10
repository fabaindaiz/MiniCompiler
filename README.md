# CDI-reference
Starter code for compilers homework

## Requirements & Setup

See the [detailed description](https://users.dcc.uchile.cl/~etanter/CC5116-2020/#(part._materials)) on the page of the course.

## Organization of the repository

The organization of the repository is designed for the development of your compiler. 

- `compiler/`: the compiler, defined as a dune library 
(using a library allows us to play with our code in a REPL, see below)
- `bin/`: top-level executables for the compiler and tests 
- `tests/`: test files for the compiler 

- `bbctester/`: a library for supporting compiler test files

- `dune-workspace`, `dune-project`: root configuration for the dune package manager
- `Makefile`: shortcuts to build and test

Dune will build everything inside the `_build/` directory.

## Makefile targets

- `make init`: generate .merlin files for autocompletion in IDE

- `make test`: execute the tests for the compiler defined in `bin/test.ml`
  variants include: 
  * `make ctest` for compact representation of the tests execution
  * you can also add `F=<pat>` where `<pat>` is a pattern to filter which tests should be executed (eg. `make test F=arith` to run only test files whose name contains `arith`)
  * a few alcotest environment variable can also be set, e.g. `ALCOTEST_QUICK_TESTS=1 make test` to only run the quick tests (see the help documentation of alcotest for more informations)
  
- `make clean`: cleans everything
  
- `make clean-tests`: cleans the tests output 


## Writing tests

Tests are written using the [alcotest](https://github.com/mirage/alcotest) unit-testing framework. Examples can be found in `bin/test.ml`. 
*Add your additional tests to this file.*

Alcotests executes a battery of unit-tests through the `run` function that takes a name (a string) and a list of items to be tested.

Each such item is composed itself from an identifier (a string) together with a list of unit-test obtained with the `test_case` function.
`test_case` takes a description of the test, a mode (either ``` `Quick ``` or ``` `Slow ```---use ``` `Quick ``` by default) and the test itself as a function `unit -> unit`.

A test is built with the `check` function which takes the following parameters:
- a way to test results of type `result_type testable`,
- an error message to be displayed when the test fails,
- the program to be tested, and the expected value (both of type `result_type`)

Once written, tests can be executed with the relevant call to the Makefile (see above), or by calling
 `dune exec bin/tests.exe` potentially followed by `--` and arguments (for instance `dune exec bin/tests.exe -- --help` to access the documentation).


## Execution
 
Given a source program in `prog.src`, you can run it using `dune exec bin/run.exe prog.src`. You can also compile it using `dune exec bin/langc.exe -- prog.src` (see the manual with `--help` for more options).

Remember that to execute your code interactively, use `dune utop` in a terminal, and then load the modules you want to interact with (e.g. `open Compiler.Interp;;`).

## Resources

Documentation for ocaml libraries:
- [containers](http://c-cube.github.io/ocaml-containers/last/) for extensions to the standard library
- [alcotest](https://mirage.github.io/alcotest/alcotest/index.html) for unit-tests
- [Fmt](https://erratique.ch/software/fmt/doc/Fmt/index.html) for printing

