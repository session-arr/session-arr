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
   `prim "merge"` is a primitive function that needs to be implemented in C (see
   Step 5).


3. Instrument the code to produce a parallel version. This instrumentation
   should guide the annotation strategy described in the paper:
```haskell
...
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (ms $ vdrop sz2 x) $ \xr ->
    prim "merge" $ pair (sz, pair (xl, xr))
```
   `par` marks the left occurrences of `ms` to be run by a different
   participant, and correspond to `ms @ r` in the paper, where `r` is a new
   participant.

4. Infer the (protocol) global type, and inspect the outcome of the achieved
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
   specified in the Haskell code (see Step 2) as `prim "merge"`.

```c
vec_double_t merge(pair_int_pair_vec_double_vec_double_t in){
  ...
}
```

### Notes on Code Generation and Recursion

We are currently working on stabilising our prototype implementation, and
implementing a number of practical improvements, including: structural recursion
for global types and parallel code (to solve the LOC explosion that results from
recursion unrolling), and encoding more program transformations. Since the
only rewriting that we support at the moment is recursion unrolling, we
provide construct `fix`, parameterised with an upper bound of allowed
unrollings, that we use for a more user-friendly specification of recursive
functions than the use of hylomorphisms, but they are semantically equivalent.

## Benchmarks

The script `./benchmark.sh` takes all the measurements that we used in our
paper. Script `./plotall.sh <BENCHMARK> <CORES>` generates the speedup graphs
under `examples/plots`, for benchmark `<BENCHMARK>`, and execution times in
`examples/<BENCHMARK>/data/t_<CORES>`. The script `./benchmark.sh` prints all
the commands that are relevant to replicate the experiments manually.

**Note**: the number of cores (must be less than the number of physical cores),
the maximum input size (must be >=15), and the number of repetitions per
experiment can be configures by setting the necessary environment variables:

```
CORES=4 REPETITIONS=50 MAXSIZE=30 ./benchmark.sh
```

The benchmarks are: `examples/DotProd`, `examples/FFT`, `examples/Mergesort`,
`examples/Quicksort` and `examples/ScalarMulMat`, and are described in the
paper.

### Manual Execution

We show step-by-step how to evaluate one of the benchmarks, `DotProd`.  The
first step is generating the C code from a Haskell module. We use the following
command for such purpose:

```
$ stack exec -- session-arrc DotProd.hs
Found functions:
dotProd1
dotProd2
dotProd4
dotProd8
dotProd16
dotProd24
dotProd32
dotProd64
```

The program `session-arrc` takes module `DotProd.hs`, finds all functions that
can be compiled to C, and produces two files: `DotProd.c` and `DotProd.h`.  In
the same directory, we find `main.c`, that contains the implementation of the
primitive functions used in `DotProd.hs`, as well as the main function
instrumented to measure execution times. We compile the C code as follows:

```
gcc DotProd.c main.c -DREPETITIONS=2 -pthread -lm -o bench
```

We use `-DREPETITIONS=<num_reps>` to set the number of repetitions per
experiment.

Each benchmark contains `./run.sh` to execute the benchmarks an all the sizes.
This outputs a file under `examples/DotProd/data/t_<CORES>` with the following
contents:

```
...
size: 1048576
	K: seq
		mean: 0.002663
		stddev: 0.000178
	K: 1
		mean: 0.002902
		stddev: 0.000292
	K: 2
		mean: 0.001655
		stddev: 0.000110
	K: 4
		mean: 0.001352
		stddev: 0.000332
	K: 8
		mean: 0.001641
		stddev: 0.000397
	K: 16
		mean: 0.001745
		stddev: 0.000257
	K: 24
		mean: 0.001938
		stddev: 0.000185
	K: 32
		mean: 0.002307
		stddev: 0.000636
...
```

Parameter `K` depends on each of the benchmarks, and is related to the number of
pthreads that are created.

### Generating Graphs

Script `./plotall.sh` is a wrapper that calls `python plot.py` to produce the
graphs. For example, from directory `examples`:

```
$ ./plotall.sh DotProd 4
```

This generates under `examples/plots/dotprod_4_k.pdf` and
`examples/plots/dotprod_4_s.pdf` the speedups with varying `K`, or varying input
size respectively.

***Note***: to visualise the plots, we recommend using `docker cp`. This command
must be run from **outside** the docker container. To copy all plots to a local
directory `<DIR>`, you can run `docker cp
<NAME>:/home/cc20-artifact/session-arr/examples/plots <DIR>`.  `<NAME>` is the
container name obtained via `docker ps -a`.

## Tutorial

This section is aimed at providing hands-on experience with building
message-passing parallel code using our tool.  The main directory for this
section is `tutorial`. It only contains one file:

- tutorial/Tutorial.hs

```haskell
module Tutorial where

maxL :: [Int] -> Int
maxL []  = 0
maxL [x] = x
maxL ls = max (maxL $ take sz ls) (maxL $ drop sz ls)
  where
    sz = length ls `div` 2
```

### Modifying Haskell Code

#### 1. Converting to `Alg`

**Run `vim examples/Tutorial.hs`, and add the following language extension and
import.**

```haskell
{-# LANGUAGE RebindableSyntax #-}
module Tutorial where

import Control.CArr.CSyn

maxL :: PAlg f => f [Int] Int
maxL []  = 0
maxL [x] = x
maxL ls = max (maxL $ take sz ls) (maxL $ drop sz ls)
  where
    sz = length ls `div` 2
```

Due to `RebindableSyntax`, now the function should not compile:
```
$ stack exec ghc -- Tutorial.hs
[1 of 1] Compiling Tutorial         ( Tutorial.hs, interpreted )

Tutorial.hs:9:11: error:
    • Variable not in scope: max :: Int -> Int -> Int
    • Perhaps you meant ‘maxL’ (line 7)
  |
9 | maxL ls = max (maxL $ take sz ls) (maxL $ drop sz ls)
  |           ^^^

...
```

**Convert to `Alg`**
The next step is to translate the difference language features to the constructs
supported by `PAlg`. There are mainly three features: *pattern matching*,
*recursion* and *where clauses*. We will substitute pattern matching by if
statements on the length of the list. To handle recursion and where clauses, we
use the following constructs:

```haskell
fix :: (PAlg t, CVal b, CVal a)
    => Int
    -> (forall f. PAlg f => (forall ctx. Expr f ctx a -> Expr f ctx b)
                         -> Var f a a -> Expr f a b)
    -> t a b
vlet :: PAlg t
     => Expr t ctx a
     -> (CVal a => Var t (a, ctx) a -> Expr t (a, ctx) b)
     -> Expr t ctx b
```

Type `Expr f ctx a` captures expressions of language `f`, inside context `ctx`,
of type `a`. Type `Var f ctx a` is a variable of type `a` inside context `ctx`.
`CVal` constraints type `a` to be _compilable to C_. Contexts are basically
n-tuples of elements.

To build a recursive function, we write `fix k (\rec x -> E)`, where `E` is an
expression that may use the recursive call `rec`, and whose input is `x`.  This
construct produces a *closed* function of type `t a b`. Parameter `k` will be
used to control the recursion unrolling. We set it to `1` for this tutorial.

**Edit `Tutorial.hs` and write the following**

```haskell
maxL :: PAlg f => f [Int] Int
maxL = fix 1 $ \rmax ls ->
  vlet (vsize ls) $ \sz ->
    if sz == 0 then 0
    else if sz == 1 then prim "head" ls
    else prim "max" $ pair (rmax $ vtake (sz / 2) ls, rmax $ vdrop (sz / 2) ls)
```

Note that we write `prim "max"` and `prim "head`. We are leaving these functions
unspecified, and will implement then in C in step **4**.

The code now compiles:
```
stack exec -- ghci Tutorial.hs
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Tutorial         ( Tutorial.hs, interpreted )
Ok, one module loaded.
*Tutorial>
```

#### 2. Introduce annotations

Construct `par` works as a function application, but it will annotate the
function with a new, fresh participant identifier.
```haskell
par :: PAlg t
    => (Var t ctx a -> Expr t ctx b)
    -> Expr t ctx a
    -> Expr t ctx b
```

You can use this function to introduce annotations. E.g.:
```haskell
maxL :: PAlg f => f [Int] Int
maxL = fix 1 $ \rmax ls ->
  vlet (vsize ls) $ \sz ->
    if sz == 0 then 0
    else if sz == 1 then prim "head" ls
    else prim "max" $ pair (par rmax $ vtake (sz / 2) ls, par rmax $ vdrop (sz / 2) ls)
```
Additionally, you can use the following construct:

```
(@@) :: PAlg t => (Var t ctx a -> Expr t ctx b) -> Prelude.Integer
     -> Expr t ctx a -> Expr t ctx b
```

This will run a function at a particular participant identifier.


#### 3. Generate global type

Inspect the achieved parallelisation by running:

```
$ stack exec -- session-arrc --infer=maxL Tutorial.hs
$ cat Tutorial_maxL.mpst
$ maxL ::: r0 -> r1
{l0. r0 -> r2 : (l0).
     end;
l1. r0 -> r2 : (l1).
    r0 -> r1
    {l0. r0 -> r2 : (l0).
         end;
    l1. r0 -> r2 : (l1).
        r0 -> r1 : (ECVec ECInt).
        r0 -> r2 : (ECVec ECInt).
        r2 -> r1 : (ECInt).
        r1 -> r0 : (ECInt).
        end}}
```

Change the annotations, and inspect alternative parallelisations to see the
difference in the global types.

#### 4. Compile to C

The final step involves the compilation to C, using the following command:
```
$ stack exec -- session-arrc Tutorial.hs

Found functions:
maxL
```

Inspect files `Tutorial.c` and `Tutorial.h` to see the result. File
`Tutorial.h` should contain a number of type definitions, as well as three
function signatures:

```c
int head(vec_int_t);
int max(pair_int_int_t);
int maxL(vec_int_t);
```

Function `maxL` is the compiled code for our Haskell `maxL` function, and `head`
and `max` are our primitive functions.


### Implementing Missing C Functions

Edit file `main.c`, include `Tutorial.h`, and write a small test case. By
reading `Tutorial.h`, you can observe that vectors are structs that contain
a pointer and a size:
```c
typedef struct vec_int {
            int * elems; size_t size;
        } vec_int_t;
```

You can use this information to build the test case:
```c
#include "Tutorial.h"

int main(){
  int test_data[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vec_int_t in = { test_data, 10 };

  printf("The maximum is: %d\n", maxL(in));
}
```

If you compile this code, GCC will complain with undefined references to `max`
and `head`.

```
$ gcc Tutorial.c main.c -o tutorial -lpthread
/usr/bin/ld: /tmp/ccxYdxMU.o: in function `maxL_part_0':
Tutorial.c:(.text+0x780): undefined reference to `head'
/usr/bin/ld: /tmp/ccxYdxMU.o: in function `fn':
Tutorial.c:(.text+0x98e): undefined reference to `head'
/usr/bin/ld: Tutorial.c:(.text+0xa2d): undefined reference to `max'
/usr/bin/ld: /tmp/ccxYdxMU.o: in function `maxL_part_1':
Tutorial.c:(.text+0xb4e): undefined reference to `max'
collect2: error: ld returned 1 exit status
```

To fix the errors, provide an implementation satisfying the interface:
```c
#include "Tutorial.h"

int max(pair_int_int_t x){
  return (x.fst < x.snd ? x.snd : x.fst);
}

int head (vec_int_t x){
  return (x.elems[0]);
}


int main(){
  int test_data[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vec_int_t in = { test_data, 10 };

  printf("The maximum is: %d\n", maxL(in));
}
```

It should now be possible to compile and run the program:
```
$ gcc Tutorial.c main.c -o tutorial -lpthread
$ ./tutorial
The maximum is: 10
```
