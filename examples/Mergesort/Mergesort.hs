{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Mergesort where

import Control.CArr.CSyn

msort :: (CVal a, PAlg f) => Int -> f [a] [a]
msort n = fix n $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (par ms $ vdrop sz2 x) $ \xr ->
    app (merge ()) $ pair (sz, pair (xl, xr))

-- prevent it from being compiled
merge :: (CVal a, PAlg f, CArrFix f) => () -> f (Int, ([a], [a])) [a]
merge _ = cfun $  prim "merge"

parMsort0 :: PAlg f => f [Double] [Double]
parMsort0 = msort 0

parMsort1 :: PAlg f => f [Double] [Double]
parMsort1 = msort 1

-- parMsort1a :: PAlg f => f [Double] [Double]
-- parMsort1a = msort_2

parMsort2 :: PAlg f => f [Double] [Double]
parMsort2 = msort 2

parMsort3 :: PAlg f => f [Double] [Double]
parMsort3 = msort 3

parMsort4 :: PAlg f => f [Double] [Double]
parMsort4 = msort 4

parMsort5 :: PAlg f => f [Double] [Double]
parMsort5 = msort 5

parMsort6 :: PAlg f => f [Double] [Double]
parMsort6 = msort 6

parMsort7 :: PAlg f => f [Double] [Double]
parMsort7 = msort 7

parMsort8 :: PAlg f => f [Double] [Double]
parMsort8 = msort 8
