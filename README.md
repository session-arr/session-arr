# session-arrows

This is the artifact for the paper *Compiling First-Order Functions to
Session-Typed Parallel Code*. The tool is a Haskell EDSL, session-arrows, that
encodes the language _PAlg_ described in the paper in terms of a
reimplementation of Haskell `Control.Arrow`. Session-arrows is meant for writing
first-order functions with participant (process) annotations. These functions
are then compiled to session-typed message-passing parallel code. Additionally,
we provide a binary `session-arrc` that takes a Haskell module, finds all
functions defined in terms of our EDSL constructs, and compiles them to C code.
The artifact also contains a number of benchmarks, used to measure the run-time
performance of the generated code, and its scalability.

## Repository Contents

* **README.md**: This file
* **LICENSE**: BSD-3 license, anonymised for this submission
* **src**: Source code of the EDSL + Compiler
* **app**: Source code of the binary `session-arrc`
* **examples**: Examples and benchmarks
* **tutorial**: Small tutorial that illustrates the main features of `session-arrows`.
* **benchmark_data**: Old benchmark data and figures.
* **benchmark.sh**: Script to rerun all benchmarks.

## Overview

`session-arrows` is the implementation of the _PAlg_ and _Alg_ languages
described in *Compiling First-Order Functions to Session-Typed Parallel Code*.
The implementation is a Haskell EDSL in terms of a reimplementation of Haskell
Arrows. The source code is under *src*, and the main modules are:

- `Control.Monad.CGen`: the C code generation monad.

- `Data.C`: supported data-types that can be compiled to C.

- `Language.CArr` and `Language.CArr.CSyn`: reimplementation of the Haskell
  `Control.Arrow` classes. Different combinations of these classes provide all
  of the constructs of _Alg_ and _PAlg_. Particularly, constraint `PAlg`
  provides all constructs of the language _PAlg_ in the paper. `CSyn` is a
  syntactic sugar wrapper on top of `CArr`.

- `Language.Alg`: AST of a first-order, pointed functional language, used for
  sequential code generation and applying optimisations.

- `Language.SPar`: session-typed message-passing monad.

- `Language.SPar.Skel`: combinators for building message-passing parallel code.

- `Language.SPar.Compiler`: compiler from `SPar` to C.

- `Language.SessionTypes.Global`: deep embedding of global types in Haskell.

### Introduction

### Implementation

### Programming Methodology

## Exploring Generated Code and Protocol

### Haskell Code

### Generated C code

### Inferred Protocol

## Benchmarks

### Manual Execution

### Generating Graphs

## Tutorial

### General Information

### Haskell Code

### Implementing Missing C Functions

### Execution
