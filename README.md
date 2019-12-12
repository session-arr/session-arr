# session-arrows

This is the artifact for the paper *Compiling First-Order Functions to
Session-Typed Parallel Code*. The tool is a Haskell EDSL, **session-arrows**, that
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
* **tutorial**: Small tutorial that illustrates the main features of **session-arrows**.
* **benchmark_data**: Old benchmark data and figures.
* **benchmark.sh**: Script to rerun all benchmarks.

## Overview

**session-arrows** is the implementation of the _PAlg_ and _Alg_ languages
described in *Compiling First-Order Functions to Session-Typed Parallel Code*.
The implementation is a Haskell EDSL in terms of a reimplementation of Haskell
Arrows.

### Implementation

The source code is under *src*, and the main modules are:

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


### Programming Methodology

1. The programmer starts a Haskell module:

```haskell
{-# LANGUAGE RebindableSyntax #-}
module Mergesort where

import Control.CArr.CSyn

msort :: (CVal a, PAlg f) => f [a] [a]
...
```

Both `Control.CArr` and `Control.CArr.CSyn` make use of Haskell's
`RebindableSyntax` extensions, which allow us to provide our own interpretation
to common Haskell syntactic constructs, such as `if-then-else`.

2. Implement a first-order function using the constructs in either
   `Control.CArr` or `Control.CArr.CSyn`.

```haskell
msort = fix 2 $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (ms $ vtake sz2 x) $ \xl ->
    vlet (ms $ vdrop sz2 x) $ \xr ->
    prim "merge" $ pair (sz, pair (xl, xr))
```

3. Instrument the code to produce a parallel version. This instrumentation
   should guide the annotation strategy described in the paper:
```haskell
...
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (ms $ vdrop sz2 x) $ \xr ->
    prim "merge" $ pair (sz, pair (xl, xr))
```

4. Generate the (protocol) global type, and inspect the outcome of the achieved
   parallelisation. Repeat step *3* if the outcome is undesired.
```
msort ::: r0 -> r1
{l0. r0 -> r2 : (l0).
     r0 -> r3 : (l0).
     end;
l1. r0 -> r2 : (l1).
    r0 -> r3 : (l1).
    r0 -> r1 : (ECVec ECDbl).
    r1 -> r2
    {l0. r0 -> r3
         {l0. r1 -> r0 : (l0).
              r1 -> r0 : (ECVec ECDbl).
              end;
         l1. r0 -> r3 : (ECVec ECDbl).
             r3 -> r0 : (ECVec ECDbl).
             r1 -> r0 : (l1).
             r1 -> r0 : (ECVec ECDbl).
             end};
    l1. r1 -> r2 : (ECVec ECDbl).
        r2 -> r1 : (ECVec ECDbl).
        r0 -> r3
        {l0. r1 -> r0 : (l0).
             r1 -> r0 : (ECVec ECDbl).
             end;
        l1. r0 -> r3 : (ECVec ECDbl).
            r3 -> r0 : (ECVec ECDbl).
            r1 -> r0 : (l1).
            r1 -> r0 : (ECVec ECDbl).
            end}}}
```

5. Implement the missing C functions. In this example this is function `merge`,
   specified in the Haskell code as `prim "merge"`.

```c
vec_double_t merge(pair_int_pair_vec_double_vec_double_t in){
  ...
}
```

### Notes on Code Generation and Recursion

We are currently working on stabilising our prototype implementation, and
implementing a number of practical improvements, including: structural recursion
for global types and parallel code (to solve the LOC explosion that results of
recursion unrolling), and encoding more program transformations. Since the
only rewriting that we support at the moment is recursion unrolling, we
provide construct `fix`, parameterised with an upper bound of allowed
unrollings, that we use for a more user-friendly specification of recursive
functions.

## Benchmarks

The script `./benchmark.sh` takes all the measurements that we used in our
paper. Scripts `./plotall.sh <BENCHMARK> <CORES>` generates the speedup graphs
that we used under `examples/plots`, for benchmark `<BENCHMARK>`, and execution
times in `examples/<BENCHMARK>/data/t_CORES`. The benchmarks are:
`examples/DotProd`, `examples/FFT`, `examples/Mergesort`, `examples/Quicksort`
and `examples/ScalarMulMat`, and are described in the paper.

**Note**: our C back-end does not currently handle memory management. Therefore,
we introduced it manually in the implementation of the primitive functions that
we used.

### Manual Execution

### Generating Graphs

## Tutorial

### General Information

### Haskell Code

### Implementing Missing C Functions

### Execution
