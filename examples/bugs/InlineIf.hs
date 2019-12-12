{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module InlineIf where

import Language.SPar.Skel

testIf :: (Bool, Int) :-> Int
testIf = mif >>> 1 ||| 2

test :: (CAlg f, CArrPar f (:->)) => f (Bool, Int) Int
test = testIf `runAt` 0
