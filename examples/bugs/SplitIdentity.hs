{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
module SplitIdentity where

import Language.SPar.Skel

spl :: CAlg f => f [Int] [Int]
spl = vtake 1

test :: CAlg f => f [Int] ([Int], [Int])
test = id &&& id
