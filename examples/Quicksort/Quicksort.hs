{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Quicksort where

import Control.CArr.CSyn

filter :: forall f ctx. (CAlg f, CVal ctx)
       => Expr f ctx [Int] -> Expr f ctx ([Int], [Int])
filter = prim "filter"

qsort :: forall f. (CAlg f, CArrFix f) => Int -> f [Int] [Int]
qsort n = fix n $ \qs x ->
  if vsize x <= 1
  then x
  else vlet (filter x) $ \lr ->
    vlet (par qs $ fst lr) $ \xl ->
    vlet (par qs $ snd lr) $ \xr ->
    prim "cat" @@ 0 $ pair (xl, xr)

parMsort0, parMsort1, parMsort2, parMsort3, parMsort4, parMsort5, parMsort6,
  parMsort7, parMsort8, parMsort9, parMsort10  :: CAlg f => f [Int] [Int]

parMsort0 = qsort 0
parMsort1 = qsort 1
parMsort2 = qsort 2
parMsort3 = qsort 3
parMsort4 = qsort 4
parMsort5 = qsort 5
parMsort6 = qsort 6
parMsort7 = qsort 7
parMsort8 = qsort 8
parMsort9 = qsort 9
parMsort10 = qsort 10
