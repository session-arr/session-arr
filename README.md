# session-arrows

This is the artifact for the paper *Compiling First-Order Functions to
Session-Typed Parallel Code*. The tool is a Haskell EDSL, session-arrows, that
encodes the language `PAlg` described in the paper in terms of a
reimplementation of Haskell `Control.Arrow`. Session-arrows is meant for writing
first-order functions with participant (process) annotations. These functions
are then compiled to session-typed message-passing parallel code. Additionally,
we provide a binary `session-arrc` that takes a Haskell module, finds all
functions defined in terms of our EDSL constructs, and compiles them to C code.
The artifact also contains a number of benchmarks, used to measure the run-time
performance of the generated code, and its scalability.

## Contents

* README.md ...................... This file
* LICENSE ........................ BSD-3 license, anonymised for this submission
* src ............................ Source code of the EDSL + Compiler
* app ............................ Source code of the binary `session-arrc`
* examples ....................... Examples and benchmarks
* tutorial ....................... Small tutorial that illustrates the main
                                   features of `session-arrows`.
* benchmark_data ................. Old benchmark data and figures.
* benchmark.sh ................... Script to rerun all benchmarks.
